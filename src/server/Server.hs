{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Server (main) where

import qualified Server.Pages.Root
import Servant
import Network.Wai.Handler.Warp (run)
import Server.Api
import qualified MyLib
import qualified Data.FileEmbed
import qualified Data.Text.Encoding as TE
import Lucid

main :: Int -> FilePath -> IO ()
main port graphDataFilename =
  MyLib.withGraphFromFile graphDataFilename $ \graph ->
    run port $
      app (style_ $ TE.decodeUtf8 cssBS) graph
  where
    cssBS = $(Data.FileEmbed.makeRelativeToProject "css/chota.css" >>= Data.FileEmbed.embedFile)

server :: Html () -> MyLib.Graph -> Server Root
server appendToHead graph =
  Server.Pages.Root.handler appendToHead graph

app :: Html () -> MyLib.Graph -> Application
app appendToHead graph =
  serve myApi (server appendToHead graph)
  where
    myApi :: Proxy Root
    myApi = Proxy
