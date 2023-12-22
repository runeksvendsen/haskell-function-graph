{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (main) where

import qualified Server.Pages.Root
import Servant
import Network.Wai.Handler.Warp (run)
import Server.Api
import qualified MyLib
import Lucid

main :: Html () -> Int -> FilePath -> IO ()
main appendToHead port graphDataFilename =
  MyLib.withFrozenGraphFromFile graphDataFilename $ \graph ->
    run port $
      app appendToHead graph

app :: Html () -> MyLib.FrozenGraph -> Application
app appendToHead graph =
  serve myApi server
  where
    myApi :: Proxy Root
    myApi = Proxy

    server =
      Server.Pages.Root.handler appendToHead graph
