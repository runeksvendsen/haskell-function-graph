{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
-- WIP: unit test with "parseResult . renderResult = id"
module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Proxy(Proxy))
import Data.Functor (void)
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Ex
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import qualified Data.Streaming.Network
import qualified Data.Text as T
import qualified FunGraph
import qualified FunGraph.Test
import qualified Lucid
import qualified Network.HTTP.Client
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp
import qualified Servant.Client
import qualified Servant.Client.Streaming
import qualified Server
import qualified Server.Api
import qualified Streaming.Prelude
import qualified Test.Hspec as HSpec
import Servant.API.ContentTypes
import Servant.HTML.Lucid (HTML)
import Server.HtmlStream (HtmlStream, toStream)
import Test.Hspec.Expectations.Pretty (shouldBe)
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

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main =
  Server.withHandlers logger mempty testDataFileName $ \handlers ->
    runWarpTestRandomPort (Server.app handlers) $ \port -> do
      runQuery <- mkQueryFunction port
      main' runQuery >>= HSpec.hspec
  where
    logger = const $ pure ()

-- TODO: don't duplicate
main'
  :: (FunGraph.Test.QueryTest -> IO (Either String [FunGraph.Test.PPFunctions]))
  -> IO HSpec.Spec
main' runQuery = do
  let testCase test =
        let (maxCount, _) = FunGraph.Test.queryTest_args test
        in HSpec.describe (FunGraph.Test.queryTest_name test <> " maxCount=" <> show maxCount) $ do
          HSpec.it "contained in top query results" $ do
            eResult <- runQuery test
            result <- either fail pure eResult
            Set.fromList (map IgnorePackage result)
              `isSupersetOf`
                Set.map IgnorePackage (FunGraph.Test.queryTest_expectedResult test)
  pure $ HSpec.describe "Integration tests" $ do
    HSpec.describe "Expected result" $
      forM_ FunGraph.Test.allTestCases testCase

-- TODO: don't duplicate
isSupersetOf :: (Show a, Ord a) => Set.Set a -> Set.Set a -> IO ()
isSupersetOf actual expected =
  actual `shouldBe` Set.union actual expected

-- TODO: don't duplicate
runWarpTestRandomPort
  :: Network.Wai.Application -- ^ Warp 'Network.Wai.Application'
  -> (Int -> IO ()) -- ^ Test action to run when the listening socket is ready. Argument: server port number. The server will be shut down when this IO action returns.
  -> IO ()
runWarpTestRandomPort app runApp = do
  (port, socket) <- Data.Streaming.Network.bindRandomPortTCP "!4"
  readyMVar <- MVar.newEmptyMVar
  let warpSettings =
        Network.Wai.Handler.Warp.setPort port $
        Network.Wai.Handler.Warp.setBeforeMainLoop
          (void $ Conc.forkIO $ runApp port >> Conc.putMVar readyMVar ())
          Network.Wai.Handler.Warp.defaultSettings
  eRes <- Async.race
    (Network.Wai.Handler.Warp.runSettingsSocket warpSettings socket app)
    (Conc.takeMVar readyMVar)
  either (const $ fail "server exited") pure eRes

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

-- TODO: don't duplicate
mkQueryFunction
  :: Int
  -> IO (FunGraph.Test.QueryTest -> IO (Either String [FunGraph.Test.PPFunctions]))
mkQueryFunction port = do
  manager <- Network.HTTP.Client.newManager $
    setResponseTimeoutSeconds 60 Network.HTTP.Client.defaultManagerSettings
  pure $ fmap parsePPFunctions . runHttpRequest manager Nothing -- TODO: also 'Just NoGraph'
  where
    setResponseTimeoutSeconds seconds settings =
      settings{Network.HTTP.Client.managerResponseTimeout = Network.HTTP.Client.responseTimeoutMicro $ seconds * 1e6}

    mkClientEnv manager =
      let baseUrl' = Servant.Client.BaseUrl Servant.Client.Http "127.0.0.1" port ""
      in Servant.Client.ClientEnv manager baseUrl' Nothing Servant.Client.defaultMakeClientRequest

    runHttpRequest
      :: Network.HTTP.Client.Manager
      -> Maybe Server.Api.NoGraph
      -> FunGraph.Test.QueryTest
      -> IO BSL.ByteString
    runHttpRequest manager mNoGraph qt =
      let htmlStreamToStream stream = do
            res Streaming.Prelude.:> () <- liftIO $ Streaming.Prelude.mconcat $
              Server.HtmlStream.toStream stream
            pure res
          (maxCount, (src, dst)) = FunGraph.Test.queryTest_args qt
          clientEnv = mkClientEnv manager
          clientM = searchClientM
            htmlStreamToStream
            Nothing -- TODO: also bench 'HX-Boosted'?
            (Just $ FunGraph.renderFullyQualifiedType src)
            (Just $ FunGraph.renderFullyQualifiedType dst)
            (Just $ fromIntegral maxCount)
            mNoGraph
      in Servant.Client.Streaming.runClientM clientM clientEnv >>= either Ex.throwIO pure

-- TODO: don't duplicate
searchClientM
  :: (HtmlStream IO () -> Servant.Client.Streaming.ClientM (Lucid.Html ()))
  -> Maybe Server.Api.HxBoosted -- HX-Boosted header
  -> Maybe T.Text -- src
  -> Maybe T.Text -- dst
  -> Maybe Word -- limit
  -> Maybe Server.Api.NoGraph -- don't draw graph?
  -> Servant.Client.Streaming.ClientM BSL.ByteString
searchClientM consumeHtmlStream hxBoosted mSrc mDst mLimit mNoGraph =
  Lucid.renderBS <$>
    (queryApi hxBoosted mSrc mDst mLimit mNoGraph >>= consumeHtmlStream)
  where
    queryApi
      :: Maybe Server.Api.HxBoosted
      -> Maybe T.Text
      -> Maybe T.Text
      -> Maybe Word
      -> Maybe Server.Api.NoGraph
      -> Servant.Client.Streaming.ClientM (HtmlStream IO ())
    queryApi =
      Servant.Client.Streaming.client searchApi

    searchApi :: Proxy Server.Api.Search
    searchApi = Proxy

instance MimeUnrender HTML (Lucid.Html ()) where
   mimeUnrender _ = Right . Lucid.toHtmlRaw

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
