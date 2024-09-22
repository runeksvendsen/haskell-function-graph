{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
-- WIP: unit test with "parseResult . renderResult = id"
module Main (main) where

import FunGraph.Test.Util
import Control.Monad (forM_)
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

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main =
  Server.withHandlers logger mempty testDataFileName $ \handlers ->
    runWarpTestRandomPort (Server.app handlers) $ \port -> do
      queryFun <- mkQueryFunction port
      let runQuery = fmap parsePPFunctions <$> queryFun id (Just Server.Api.NoGraph)
      main' runQuery >>= HSpec.hspec
  where
    logger = const $ pure ()

main'
  :: (FunGraph.Test.QueryTest -> IO (Either String [FunGraph.Test.PPFunctions]))
  -> IO HSpec.Spec
main' runQuery = do
  let testCase test =
        let (maxCount, _) = FunGraph.Test.queryTest_args test
        in HSpec.describe (FunGraph.Test.queryTest_name test <> " maxCount=" <> show maxCount) $ do
          HSpec.it "contained in top query results" $ do
            result <- either fail pure =<< runQuery test
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
  deriving (Show)

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
