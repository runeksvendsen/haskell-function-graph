{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Server
  ( main
  , app
  , withHandlers
  )
  where

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

main :: Html () -> Int -> FilePath -> IO ()
main appendToHead port graphDataFilename =
  withHandlers FunGraph.Util.putStrFlush appendToHead graphDataFilename $ \handlers -> do
    putStrLn $ "Running server on " <> "http://localhost:" <> show port
    run port $ enableProdMiddleware $ app handlers

withHandlers
  :: (String -> IO ()) -- ^ Log 'String' without trailing newline
  -> Html () -- ^ Add to HTML @head@ element
  -> FilePath -- ^ Graph data filename, e.g. @data/all3.json@
  -> (Handlers -> IO a) -- ^ Do something with a 'Handlers'
  -> IO a
withHandlers logStr appendToHead graphDataFilename f = do
  Server.GraphViz.healthCheck logStr
  logStr "Building graph... "
  FunGraph.withGraphFromFile FunGraph.defaultBuildConfig graphDataFilename $ \graph -> do
    getGraphInfo graph >>= \graphInfo -> logStr $ "done. " <> graphInfo <> "\n"
    logStr "Initializing handlers... "
    handlers <- mkHandlers appendToHead graph
    logStr "done\n"
    f handlers
  where
    getGraphInfo graph = ST.stToIO $ do
      vertexCount <- DG.vertexCount graph
      edgeCount <- DG.edgeCount graph
      pure $ "Vertex count: " <> show vertexCount <> ", edge count: " <> show edgeCount <> "."

mkHandlers
  :: Html ()
  -> FunGraph.Graph ST.RealWorld
  -> IO Handlers
mkHandlers appendToHead graph = do
  (typeaheadHandler, initalSuggestions) <-
    Server.Pages.Typeahead.mkHandler (Just typeaheadCountLimit) graph
  let mkRootHandler = Server.Pages.Root.page (htmx <> fixSvgWidth <> appendToHead <> bodyMargin) initalSuggestions
  searchEnv <- Server.Pages.Search.createSearchEnv graph
  pure $ Handlers
        (mkRootHandler ("", Nothing))
        (\mHxBoosted mSrc mDst mMaxCount mNoGraph -> do
            let runSearchHandler = Server.Pages.Search.handler searchEnv mHxBoosted mSrc mDst mMaxCount mNoGraph
            case mHxBoosted of
              Just HxBoosted -> do
                (searchResult, _) <- runSearchHandler
                pure searchResult
              Nothing -> do
                (searchResult, (src, dst)) <- runSearchHandler
                pure $ mkRootHandler (searchResult, Just (src, dst))
        )
        typeaheadHandler
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

    -- TODO: inline
    htmx = toHtmlRaw $ T.unlines
      [ "<script"
      , "  src=\"https://unpkg.com/htmx.org@1.9.4\""
      , "  integrity=\"sha384-zUfuhFKKZCbHTY6aRR46gxiqszMk5tcHjsVFxnUo8VMus4kHGVdIYVbOYYNlKmHV\""
      , "  crossorigin=\"anonymous\""
      , "></script>"
      ]

data Handlers = Handlers
  !Server.Pages.Root.HandlerType
  !(Server.Pages.Search.HandlerType (Html ()))
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
