{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (main) where

import qualified Server.Pages.Root
import Servant
import Network.Wai.Handler.Warp (run)
import Server.Api
import Lucid (Html)

main :: Int -> IO ()
main port = run port app

server :: Server Root
server = helloHandler

helloHandler :: Handler (Html ())
helloHandler = return Server.Pages.Root.page

app :: Application
app =
  serve myApi server
  where
    myApi :: Proxy Root
    myApi = Proxy
