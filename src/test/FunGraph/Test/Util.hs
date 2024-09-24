{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- WIP: unit test with "parseResult . renderResult = id"
module FunGraph.Test.Util
( searchClientM
, mkQueryFunction
, runWarpTestRandomPort
, isSupersetOf
, StreamIOHtml
)
where

import Control.Monad.IO.Class (liftIO)
import Data.Data (Proxy(Proxy))
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Ex
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import qualified Data.Streaming.Network
import qualified Data.Text as T
import qualified FunGraph
import qualified FunGraph.Test
import qualified Lucid
import qualified Network.HTTP.Client
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp
import qualified Servant.Client
import qualified Servant.Client.Streaming
import qualified Server.Api
import qualified Streaming.Prelude
import Streaming.Prelude (Stream, Of)
import Servant.API.ContentTypes
import Servant.HTML.Lucid (HTML)
import Server.HtmlStream (HtmlStream, toStream)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Lucid.Base (Html)

isSupersetOf :: (Show a, Ord a) => Set.Set a -> Set.Set a -> IO ()
isSupersetOf actual expected =
  actual `shouldBe` Set.union actual expected

-- Run a 'Network.Wai.Application' on a random port and execute an IO action when the listening socket is ready.
runWarpTestRandomPort
  :: Network.Wai.Application
  -- ^ Warp 'Network.Wai.Application'
  -> (Int -> IO a)
  -- ^ Test action to run when the listening socket is ready. Argument: server port number.
  --
  -- The server will be shut down when this IO action terminates.
  -- An exception thrown by this IO action will be rethrown by 'runWarpTestRandomPort'
  -> IO a
runWarpTestRandomPort app testAction = do
  (port, socket) <- Data.Streaming.Network.bindRandomPortTCP "!4"
  asyncMVar <- MVar.newEmptyMVar
  let warpSettings =
        Network.Wai.Handler.Warp.setPort port $
        Network.Wai.Handler.Warp.setBeforeMainLoop
          (Async.async (testAction port) >>= Conc.putMVar asyncMVar)
          Network.Wai.Handler.Warp.defaultSettings
  eRes <- Async.race
    (Network.Wai.Handler.Warp.runSettingsSocket warpSettings socket app)
    (Conc.takeMVar asyncMVar >>= Async.wait)
  either (const $ fail "runWarpTestRandomPort: Wai Application exited unexpectedly") pure eRes

type StreamIOHtml = Stream (Of (Html ())) IO ()

mkQueryFunction
  :: Int
  -> IO ((StreamIOHtml -> StreamIOHtml) -> Maybe Server.Api.NoGraph -> FunGraph.Test.QueryTest -> IO BSL.ByteString)
mkQueryFunction port = do
  manager <- Network.HTTP.Client.newManager $
    setResponseTimeoutSeconds 60 Network.HTTP.Client.defaultManagerSettings
  pure $ runHttpRequest manager -- TODO: also 'Just NoGraph'
  where
    setResponseTimeoutSeconds seconds settings =
      settings{Network.HTTP.Client.managerResponseTimeout = Network.HTTP.Client.responseTimeoutMicro $ seconds * 1e6}

    mkClientEnv manager =
      let baseUrl' = Servant.Client.BaseUrl Servant.Client.Http "127.0.0.1" port ""
      in Servant.Client.ClientEnv manager baseUrl' Nothing Servant.Client.defaultMakeClientRequest

    runHttpRequest
      :: Network.HTTP.Client.Manager
      -> (StreamIOHtml -> StreamIOHtml)
      -> Maybe Server.Api.NoGraph
      -> FunGraph.Test.QueryTest
      -> IO BSL.ByteString
    runHttpRequest manager modifyStream mNoGraph qt =
      let htmlStreamToStream stream = do
            res Streaming.Prelude.:> () <- liftIO $ Streaming.Prelude.mconcat $
              modifyStream $ Server.HtmlStream.toStream stream
            pure res
          (maxCount, (src, dst)) = FunGraph.Test.queryTest_args qt
          clientEnv = mkClientEnv manager
          clientM = searchClientM
            htmlStreamToStream
            Nothing -- TODO: also bench 'HX-Boosted'?
            (Just $ FunGraph.renderFullyQualifiedType src)
            (Just $ FunGraph.renderFullyQualifiedType dst)
            (Just $ fromIntegral maxCount)
            mNoGraph
      in Servant.Client.Streaming.runClientM clientM clientEnv >>= either Ex.throwIO pure

searchClientM
  :: (HtmlStream IO () -> Servant.Client.Streaming.ClientM (Lucid.Html ()))
  -> Maybe Server.Api.HxBoosted -- HX-Boosted header
  -> Maybe T.Text -- src
  -> Maybe T.Text -- dst
  -> Maybe Word -- limit
  -> Maybe Server.Api.NoGraph -- don't draw graph?
  -> Servant.Client.Streaming.ClientM BSL.ByteString
searchClientM consumeHtmlStream hxBoosted mSrc mDst mLimit mNoGraph =
  Lucid.renderBS <$>
    (queryApi hxBoosted mSrc mDst mLimit mNoGraph >>= consumeHtmlStream)
  where
    queryApi
      :: Maybe Server.Api.HxBoosted
      -> Maybe T.Text
      -> Maybe T.Text
      -> Maybe Word
      -> Maybe Server.Api.NoGraph
      -> Servant.Client.Streaming.ClientM (HtmlStream IO ())
    queryApi =
      Servant.Client.Streaming.client searchApi

    searchApi :: Proxy Server.Api.Search
    searchApi = Proxy

instance MimeUnrender HTML (Lucid.Html ()) where
   mimeUnrender _ = Right . Lucid.toHtmlRaw
