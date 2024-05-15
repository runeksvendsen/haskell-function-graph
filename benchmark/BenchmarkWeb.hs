{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Benchmark the web server
--
-- TODO: Make exe exit properly for CLI arguments that don't need server (e.g. --help, --list)
module Main (main) where

import qualified Server.Api
import qualified FunGraph
import qualified FunGraph.Test
import qualified Server

import Servant.API.ContentTypes
import qualified Servant.Client
import qualified Lucid
import Servant.HTML.Lucid (HTML)
import Criterion.Main

import Data.Data (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wai.Handler.Warp
import qualified Network.HTTP.Client
import qualified Data.Streaming.Network
import qualified Network.Wai
import qualified Control.Exception as Ex
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent as Conc
import Data.Functor (void)
import qualified Control.Concurrent.Async as Async

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main =
  Server.withHandlers logger mempty testDataFileName $ \handlers ->
    runWarpTestRandomPort (Server.app handlers) runTests
  where
    logger = const $ pure ()

runWarpTestRandomPort
  :: Network.Wai.Application -- ^ Warp 'Network.Wai.Application'
  -> (Int -> IO ()) -- ^ Test action to run when the listening socket is ready. Argument: server port number. The server will be shut down when this IO action returns.
  -> IO ()
runWarpTestRandomPort app runApp = do
  (port, socket) <- Data.Streaming.Network.bindRandomPortTCP "!4"
  readyMVar <- MVar.newEmptyMVar
  let warpSettings =
        Network.Wai.Handler.Warp.setPort port $
        Network.Wai.Handler.Warp.setBeforeMainLoop
          (void $ Conc.forkIO $ runApp port >> Conc.putMVar readyMVar ())
          Network.Wai.Handler.Warp.defaultSettings
  eRes <- Async.race
    (Network.Wai.Handler.Warp.runSettingsSocket warpSettings socket app)
    (Conc.takeMVar readyMVar)
  either (const $ fail "server exited") pure eRes

runTests :: Int -> IO ()
runTests port = do
  manager <- Network.HTTP.Client.newManager $
    setResponseTimeoutSeconds 60 Network.HTTP.Client.defaultManagerSettings
  defaultMain
    [ bgroup "Web server"
      [ bgroup "search"
        [ bgroup "with graph" $
            map (benchHttpRequest manager Nothing) FunGraph.Test.allTestCases
        , bgroup "without graph" $
            map (benchHttpRequest manager (Just Server.Api.NoGraph)) FunGraph.Test.allTestCases
        ]
      ]
    ]
  where
    setResponseTimeoutSeconds seconds settings =
      settings{Network.HTTP.Client.managerResponseTimeout = Network.HTTP.Client.responseTimeoutMicro $ seconds * 1e6}

    mkClientEnv manager =
      let baseUrl' = Servant.Client.BaseUrl Servant.Client.Http "127.0.0.1" port ""
      in Servant.Client.ClientEnv manager baseUrl' Nothing Servant.Client.defaultMakeClientRequest

    benchHttpRequest
      :: Network.HTTP.Client.Manager
      -> Maybe Server.Api.NoGraph
      -> FunGraph.Test.QueryTest -> Benchmark
    benchHttpRequest manager mNoGraph qt =
      let (maxCount, (src, dst)) = FunGraph.Test.queryTest_args qt
          clientEnv = mkClientEnv manager
          clientM = searchClientM
            Nothing -- TODO: also bench 'HX-Boosted'?
            (Just $ FunGraph.renderFullyQualifiedType src)
            (Just $ FunGraph.renderFullyQualifiedType dst)
            (Just $ fromIntegral maxCount)
            mNoGraph
      in bench (FunGraph.Test.queryTest_name qt <> " maxCount=" <> show maxCount) $
        nfIO $ Servant.Client.runClientM clientM clientEnv >>= either Ex.throwIO pure

searchClientM
  :: Maybe Server.Api.HxBoosted -- HX-Boosted header
  -> Maybe T.Text -- src
  -> Maybe T.Text -- dst
  -> Maybe Word -- limit
  -> Maybe Server.Api.NoGraph -- don't draw graph?
  -> Servant.Client.ClientM BSL.ByteString
searchClientM hxBoosted mSrc mDst mLimit mNoGraph =
  Lucid.renderBS <$>
    Servant.Client.client searchApi hxBoosted mSrc mDst mLimit mNoGraph
  where
    searchApi :: Proxy Server.Api.Search
    searchApi = Proxy

instance MimeUnrender HTML (Lucid.Html ()) where
   mimeUnrender _ = Right . Lucid.toHtmlRaw
