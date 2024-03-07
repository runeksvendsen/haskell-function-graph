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
  FunGraph.withFrozenGraphFromFile FunGraph.defaultBuildConfig graphDataFilename $ \graph -> do
    putStrLn $ "Running server on " <> "http://localhost:" <> show port
    run port $
      app appendToHead graph

app :: Html () -> FunGraph.FrozenGraph -> Application
app appendToHead graph =
  enableMiddleware $ serve myApi server
  where
    myApi :: Proxy Api
    myApi = Proxy

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

    server =
      pure (Server.Pages.Root.page (htmx <> fixSvgWidth <> appendToHead)) :<|>
      Server.Pages.Search.handler graph :<|>
      Server.Pages.Typeahead.handler graph

    enableMiddleware =
        RL.logStdoutDev
      . Errors.errorMw @JSON @'["error", "status"]
