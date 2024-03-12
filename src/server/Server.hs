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

main :: Html () -> Int -> FilePath -> IO ()
main appendToHead port graphDataFilename = do
  Server.GraphViz.healthCheck
  FunGraph.withFrozenGraphFromFile FunGraph.defaultBuildConfig graphDataFilename $ \igraph -> do
    handlers <- mkHandlers appendToHead igraph
    putStrLn $ "Running server on " <> "http://localhost:" <> show port
    run port $ app handlers

mkHandlers
  :: Html ()
  -> FunGraph.FrozenGraph
  -> IO Handlers
mkHandlers appendToHead igraph = do
  (typeaheadHandler, initalSuggestions) <-
    Server.Pages.Typeahead.mkHandler (Just typeaheadCountLimit) igraph
  pure $ Handlers
        (Server.Pages.Root.page (htmx <> fixSvgWidth <> appendToHead) initalSuggestions)
        (Server.Pages.Search.handler igraph)
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
