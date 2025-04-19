{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module Server
  ( main
  , app
  , withHandlers
  , Server.Pages.Search.defaultSearchConfig, Server.Pages.Search.SearchConfig(..)
  , Config.mkDefaultServerConfig, Config.ServerConfig
  )
  where

import qualified Server.Config as Config
import qualified Server.Pages.Root
import qualified Server.Pages.Search
import qualified Server.Pages.Typeahead
import Servant
import Network.Wai.Handler.Warp (run)
import Server.Api
import qualified FunGraph
import Lucid
import qualified Data.Text as T
import qualified Server.GraphViz
import qualified Network.Wai.Middleware.Servant.Errors as Errors
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified Control.Monad.ST as ST
import qualified Data.Graph.Digraph as DG
import qualified FunGraph.Util
import Server.HtmlStream (HtmlStream)
import qualified Data.FileEmbed
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Data.List.NonEmpty as NE
import Control.Monad (unless)

main :: Config.ServerConfig -> IO ()
main serverConfig =
  withHandlers FunGraph.Util.putStrFlush searchConfig appendToHead graphDataFilename $ \handlers -> do
    FunGraph.Util.putStrFlush $ "Running server on " <> "http://localhost:" <> show port <> "\n"
    run port $ enableProdMiddleware $ app handlers
  where
    searchConfig = Config.serverConfigSearchConfig serverConfig
    appendToHead = Config.serverConfigAppendToHeadHtml serverConfig
    port = Config.serverConfigPort serverConfig
    graphDataFilename = Config.serverConfigGraphDataFilename serverConfig

withHandlers
  :: (String -> IO ()) -- ^ Log 'String' without trailing newline
  -> Server.Pages.Search.SearchConfig
  -> Html () -- ^ Add to HTML @head@ element
  -> FilePath -- ^ Graph data filename, e.g. @data/all3.json@
  -> (Handlers -> IO a) -- ^ Do something with a 'Handlers'
  -> IO a
withHandlers logStr searchConfig appendToHead graphDataFilename f = do
  Server.GraphViz.healthCheck logStr
  logStr "Building graph... "
  FunGraph.withGraphFromFile FunGraph.defaultBuildConfig graphDataFilename $ \graph -> do
    getGraphInfo graph >>= \graphInfo -> logStr $ "done. " <> graphInfo <> "\n"
    logStr "Initializing handlers... "
    handlers <- mkHandlers searchConfig appendToHead graph
    logStr "done\n"
    f handlers
  where
    getGraphInfo graph = ST.stToIO $ do
      vertexCount <- DG.vertexCount graph
      edgeCount <- DG.edgeCountMulti graph
      pure $ "Vertex count: " <> show vertexCount <> ", edge count: " <> show edgeCount <> "."

mkHandlers
  :: Server.Pages.Search.SearchConfig
  -> Html ()
  -> FunGraph.Graph ST.RealWorld
  -> IO Handlers
mkHandlers searchConfig appendToHead graph = do
  unless (Server.Pages.Search.searchConfigSuggestionLimit searchConfig >= 1) $
    fail "'searchConfigSuggestionLimit' must be greater than or equal to 1."
  (typeaheadHandler, lookupFunction, initalSuggestions) <-
    Server.Pages.Typeahead.mkHandler (Just typeaheadCountLimit) graph
  let mkRootHandler = Server.Pages.Root.page (fixSvgWidth <> appendToHead <> bodyMargin) htmxScript initalSuggestions
      lookupFunctionLimited = fmap (NE.fromList . NE.take (fromIntegral $ Server.Pages.Search.searchConfigSuggestionLimit searchConfig)) . lookupFunction
  searchEnv <- Server.Pages.Search.createSearchEnv mkRootHandler graph lookupFunctionLimited
  pure $ Handlers
        (mkRootHandler (mempty, (Nothing, Nothing))) -- root handler
        (\mHxBoosted mSrc mDst mMaxCount mNoGraph -> do -- search handler
            Server.Pages.Search.handler
              searchConfig
              searchEnv
              mHxBoosted
              mSrc
              mDst
              mMaxCount
              mNoGraph
        )
        typeaheadHandler -- typeahead handler
  where
    typeaheadCountLimit = 25

    fixSvgWidth = style_ $ T.unlines
      [ "svg {"
      , "  max-width: 100%;"
      , "  height: 400%;"
      , "  display: block;"
      , "}"
      ]

    bodyMargin = style_ $ T.unlines
      [ "body {"
      , "  margin: 20px;"
      , "}"
      ]

    htmxJsBs = $(Data.FileEmbed.makeRelativeToProject "js/htmx-1.9.4.js" >>= Data.FileEmbed.embedFile)
    htmxExtBs = $(Data.FileEmbed.makeRelativeToProject "js/htmx-ext-chunked-transfer.js" >>= Data.FileEmbed.embedFile)

    htmxScript =
      let decodeUtf8 = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
      in script_ (decodeUtf8 htmxJsBs) >> script_ (decodeUtf8 htmxExtBs)

data Handlers = Handlers
  !Server.Pages.Root.HandlerType
  !(Server.Pages.Search.HandlerType (HtmlStream IO ()))
  !Server.Pages.Typeahead.HandlerType

app :: Handlers -> Application
app (Handlers rootHandler searchHandler typeaheadHandler) =
  serve myApi server
  where
    myApi :: Proxy Api
    myApi = Proxy

    server =
      pure rootHandler :<|>
      searchHandler :<|>
      typeaheadHandler

enableProdMiddleware :: Application -> Application
enableProdMiddleware =
    RL.logStdoutDev
  . Errors.errorMw @JSON @'["error", "status"]
