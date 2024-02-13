{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (main) where

import qualified Server.Pages.Root
import Servant
import Network.Wai.Handler.Warp (run)
import Server.Api
import qualified FunGraph
import Lucid

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

    server =
      Server.Pages.Root.handler appendToHead graph
