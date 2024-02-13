{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (main) where

import qualified Server.Pages.Root
import Servant
import Network.Wai.Handler.Warp (run)
import Server.Api
import qualified FunGraph
import Lucid
import qualified Data.Text as T

main :: Html () -> Int -> FilePath -> IO ()
main appendToHead port graphDataFilename =
  FunGraph.withFrozenGraphFromFile FunGraph.defaultBuildConfig graphDataFilename $ \graph ->
    run port $
      app appendToHead graph

app :: Html () -> FunGraph.FrozenGraph -> Application
app appendToHead graph =
  serve myApi server
  where
    myApi :: Proxy Root
    myApi = Proxy

    fixSvgWidth = style_ $ T.unlines
      [ "svg {"
      , "  max-width: 100%;"
      , "  height: auto;"
      , "  display: block;"
      , "}"
      ]

    server =
      Server.Pages.Root.handler (fixSvgWidth <> appendToHead) graph
