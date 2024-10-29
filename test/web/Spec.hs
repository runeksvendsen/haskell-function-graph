{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
-- WIP: unit test with "parseResult . renderResult = id"
module Main (main) where

import FunGraph.Test.Util
import Control.Monad (forM_, unless)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified FunGraph
import qualified FunGraph.Test
import qualified Server
import qualified Test.Hspec as HSpec
import qualified Text.HTML.Parser
import qualified Text.HTML.Tree
import qualified Lucid.Base
import qualified Server.Pages.Search
import qualified Data.ByteString.Lazy
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import Data.Bifunctor (first)
import qualified Data.Tree
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Types
import qualified Server.Api
import qualified GHC.IO.Unsafe
import qualified System.Console.ANSI as ANSI
import qualified System.IO.Temp
import qualified System.Environment
import qualified Test.Hspec.Runner as HSpec
import Data.Functor (void)

main :: IO ()
main = do
  config <- System.Environment.getArgs
    >>= HSpec.readConfig HSpec.defaultConfig
  System.IO.Temp.withSystemTempDirectory "haskell-function-graph-test-web" $ \failureReportFileDir -> do
    let failureReportFile = failureReportFileDir <> "/failure.report"
        setConfigFailureReport cfg = cfg{HSpec.configFailureReport = Just failureReportFile}
    summary <- withSpec defaultSearchConfig
      (HSpec.hspecWithResult $ setConfigFailureReport config)
    unless (HSpec.isSuccess summary) $ do
      putStrLn "\nTest suite had failures. Rerunning failed test cases with tracing enabled..."
      let mkRerunConfig cfg =
            cfg{HSpec.configRerun = True, HSpec.configConcurrentJobs = Just 1} -- tracing output is difficult to read if test cases are run in parallel
      void $ withSpec defaultSearchConfig{Server.searchConfigTrace = Just traceFun}
        (HSpec.hspecWithResult $ mkRerunConfig $ setConfigFailureReport config)
    HSpec.evaluateSummary summary
  where
    defaultSearchConfig = Server.defaultSearchConfig{Server.searchConfigTimeout = 100}

    traceFun str =
      let color color' = concat
            [ ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Dull color']
            , str
            , ANSI.setSGRCode [ANSI.Reset]
            ]
      in pure $
        GHC.IO.Unsafe.unsafePerformIO $ do
          putStrLn $ color ANSI.Red

withSpec
  :: Server.Pages.Search.SearchConfig
  -> (HSpec.Spec -> IO a)
  -> IO a
withSpec searchConfig f =
  Server.withHandlers logger searchConfig mempty testDataFileName $ \handlers ->
    runWarpTestRandomPort (Server.app handlers) $ \port -> do
      queryFun <- mkQueryFunction port
      let runQuery noGraph = fmap parsePPFunctions <$> queryFun id noGraph
      main' runQuery >>= f
  where
    logger = const $ pure ()

main'
  :: (Maybe Server.Api.NoGraph -> FunGraph.Test.QueryTest -> IO (Either String [FunGraph.Test.PPFunctions]))
  -> IO HSpec.Spec
main' runQuery = do
  let testCase test =
        let (maxCount, _) = FunGraph.Test.queryTest_args test
        in HSpec.describe (FunGraph.Test.queryTest_name test <> " maxCount=" <> show maxCount) $ do
          HSpec.describe "contained in top query results" $
            forM_ [("with graph", Nothing), ("no graph", Just Server.Api.NoGraph)] $ \(name, noGraph) ->
              HSpec.it name $ do
                result <- either fail pure =<< runQuery noGraph test
                Set.fromList (map IgnorePackage result)
                  `isSupersetOf`
                    Set.map IgnorePackage (FunGraph.Test.queryTest_expectedResult test)
  pure $ HSpec.describe "Integration tests" $ do
    HSpec.describe "Expected result" $
      forM_ FunGraph.Test.allTestCases testCase

parsePPFunctions
  :: BSL.ByteString -- ^ Entire /search endpoint response
  -> Either String [FunGraph.Test.PPFunctions]
parsePPFunctions bs = do
  tokenTree <-
      first show
    . Text.HTML.Tree.tokensToForest
    . Text.HTML.Parser.parseTokens
    . Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
    . Data.ByteString.Lazy.toStrict
    $ bs
  parseTokenTree tokenTree
  where
    parseTokenTree
      :: [Data.Tree.Tree Text.HTML.Parser.Token]
      -> Either String [FunGraph.Test.PPFunctions]
    parseTokenTree =
      fmap (map $ FunGraph.Test.PPFunctions . NE.toList)
      . sequence
      . parseTokenTreeResults

    textTokens :: [Text.HTML.Parser.Token] -> T.Text
    textTokens =
      let contentText = \case
            Text.HTML.Parser.ContentText text -> Just text
            _ -> Nothing
      in T.concat
      . mapMaybe contentText
      . Text.HTML.Parser.canonicalizeTokens

    parseTokenTreeResults
      :: [Data.Tree.Tree Text.HTML.Parser.Token]
      -> [Either String (FunGraph.NonEmpty FunGraph.UntypedFunction)]
    parseTokenTreeResults =
      let go :: Data.Tree.Tree Text.HTML.Parser.Token
             -> [Either String (FunGraph.NonEmpty FunGraph.UntypedFunction)]
          go (Data.Tree.Node label subForest) =
            if hasResultAttribute label
              then [FunGraph.parseComposedFunctionsNoPackage $ textTokens $ Text.HTML.Tree.tokensFromForest subForest]
              else concatMap go subForest
      in concatMap go

    Lucid.Base.Attribute resultHtmlAttributeText _ =
      Server.Pages.Search.mkResultAttribute ""

    hasResultAttribute = \case
      Text.HTML.Parser.TagOpen _ attributes ->
        resultHtmlAttributeText `elem` map attrName attributes
      _ -> False

    attrName (Text.HTML.Parser.Attr name _) = name

newtype IgnorePackage a = IgnorePackage a

instance Show (IgnorePackage FunGraph.Test.PPFunctions) where
  show (IgnorePackage a) = show a

instance Eq (IgnorePackage FunGraph.Test.PPFunctions) where
  IgnorePackage a == IgnorePackage b =
    strikePackagePP a == strikePackagePP b

instance Ord (IgnorePackage FunGraph.Test.PPFunctions) where
  IgnorePackage a <= IgnorePackage b =
    strikePackagePP a <= strikePackagePP b

strikePackagePP :: FunGraph.Test.PPFunctions -> FunGraph.Test.PPFunctions
strikePackagePP =
  FunGraph.Test.PPFunctions . map strikePackage . FunGraph.Test.unPPFunctions
  where
    strikePackage function = function{FunGraph._function_package = Types.FgPackage "" ""}
