{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Server (main) where

import qualified Server.Pages.Root
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
  FunGraph.withFrozenGraphFromFile FunGraph.defaultBuildConfig graphDataFilename $ \graph ->
    run port $
      app appendToHead graph

app :: Html () -> FunGraph.FrozenGraph -> Application
app appendToHead graph =
  enableMiddleware $ serve myApi server
  where
    myApi :: Proxy Root
    myApi = Proxy

    fixSvgWidth = style_ $ T.unlines
      [ "svg {"
      , "  max-width: 100%;"
      , "  height: 400%;"
      , "  display: block;"
      , "}"
      ]

    server =
      Server.Pages.Root.handler (fixSvgWidth <> appendToHead) graph

    enableMiddleware =
        RL.logStdoutDev
      . Errors.errorMw @JSON @'["error", "status"]
