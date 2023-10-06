{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (main) where

import Servant
import Network.Wai.Handler.Warp (run)
import Server.Api

main :: Int -> IO ()
main port = run port app

myApi :: Proxy MyApi
myApi = Proxy

server :: Server MyApi
server = helloHandler

helloHandler :: Handler String
helloHandler = return "Hello, World!"

app :: Application
app = serve myApi server