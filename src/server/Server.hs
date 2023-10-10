{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (main) where

import qualified Server.Pages.Root
import Servant
import Network.Wai.Handler.Warp (run)
import Server.Api
import qualified MyLib

main :: Int -> FilePath -> IO ()
main port graphDataFilename =
  MyLib.withGraphFromFile graphDataFilename $ \graph ->
    run port (app graph)

server :: MyLib.Graph -> Server Root
server graph =
  Server.Pages.Root.handler graph

app :: MyLib.Graph -> Application
app graph =
  serve myApi (server graph)
  where
    myApi :: Proxy Root
    myApi = Proxy
