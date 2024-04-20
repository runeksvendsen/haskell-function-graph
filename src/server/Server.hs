{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Server (main) where

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
import qualified System.IO
import qualified Control.Monad.ST as ST
import qualified Data.Graph.Digraph as DG

main :: Html () -> Int -> FilePath -> IO ()
main appendToHead port graphDataFilename = do
  Server.GraphViz.healthCheck
  putStrFlush "Building graph... "
  FunGraph.withGraphFromFile FunGraph.defaultBuildConfig graphDataFilename $ \graph -> do
    getGraphInfo graph >>= \graphInfo -> putStrLn $ "done. " <> graphInfo
    putStrFlush "Initializing handlers... "
    handlers <- mkHandlers appendToHead graph
    putStrLn "done"
    putStrLn $ "Running server on " <> "http://localhost:" <> show port
    run port $ app handlers
  where
    putStrFlush str = putStr str >> System.IO.hFlush System.IO.stdout

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
  searchEnv <- Server.Pages.Search.createSearchEnv graph
  pure $ Handlers
        (Server.Pages.Root.page (htmx <> fixSvgWidth <> appendToHead <> bodyMargin) initalSuggestions)
        (Server.Pages.Search.handler searchEnv)
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
  !(Html ()) -- ^ Root handler
  !(Maybe T.Text -> Maybe T.Text -> Maybe Word -> Handler (Html ())) -- ^ Search handler
  !(Maybe T.Text -> Maybe T.Text -> Handler (Html ()))

app :: Handlers -> Application
app (Handlers rootHandler searchHandler typeaheadHandler) =
  enableMiddleware $ serve myApi server
  where
    myApi :: Proxy Api
    myApi = Proxy

    server =
      pure rootHandler :<|>
      searchHandler :<|>
      typeaheadHandler

    enableMiddleware =
        RL.logStdoutDev
      . Errors.errorMw @JSON @'["error", "status"]
