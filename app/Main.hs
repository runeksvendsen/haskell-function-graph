{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main (main) where

import           Control.Concurrent
                 (threadDelay)
import           Control.Monad.IO.Class
                 (MonadIO (..))

import           Data.Maybe
                 (fromMaybe)

import           Network.Wai
                 (Application)
import           System.Environment
                 (getArgs, lookupEnv)
import           Text.Read
                 (readMaybe)

import           Servant

import qualified Servant.Types.SourceT as S
import Lucid

import qualified Network.Wai.Handler.Warp     as Warp
import qualified Data.Text as T
import Data.Functor.Identity (Identity)
import qualified Data.Text.Lazy as LT
-- import Servant.HTML.Lucid (HTML)
import qualified Data.List.NonEmpty as NE
import           Data.Typeable      (Typeable)
import           Lucid              (ToHtml (..), renderBS)
import qualified Network.HTTP.Media as M
import           Servant.API        (Accept (..), MimeRender (..))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as LTE

data HTML deriving Typeable

-- | @text/html;charset=utf-8@
instance Accept HTML where
    contentTypes _ =
      "text" M.// "html" M./: ("charset", "utf-8") NE.:|
      ["text" M.// "html"]

instance MimeRender HTML T.Text where
    mimeRender _ = LTE.encodeUtf8 . LT.fromStrict

type API =
    Get '[HTML] T.Text
    :<|> "slow" :> StreamGet NewlineFraming HTML (SourceIO T.Text)
    :<|> "search" :> QueryParam "query" String :> StreamGet NewlineFraming HTML (SourceIO T.Text)
    :<|> "slown" :> Capture "num" Int :> StreamGet NewlineFraming HTML (SourceIO T.Text)

api :: Proxy API
api = Proxy

server :: Server API
server =
    root :<|> slow 10 :<|> (\_ -> slow 10) :<|> slow
  where
    root = pure $ T.unlines
      [ "<html>"
      , "<head>"
      , "<script src=\"https://unpkg.com/htmx.org@2.0.1\" integrity=\"sha384-QWGpdj554B4ETpJJC9z+ZHJcA/i59TyjxEPXiiUgN2WmTyV5OEZWCD6gQhgkdpB/\" crossorigin=\"anonymous\"></script>"
      , "</head>"

      , "<body>"
      , "<a href=\"/slow\">Regular link to /slow</a>"
      , "<br>"
      , "<br>"
      , "<a href=\"/slow\" hx-boost=\"true\">Boosted link to /slow</a>"
      , "<br>"
      , "<br>"
      , "<a href=\"/slow\" hx-boost=\"true\" hx-target=\"#results\">Boosted link to /slow with hx-target=#results</a>"
      , "<br>"
      , "<br>"
      , "<div id=\"results\">#results div</div>"

      , "</body>"
      , "</html>"
      ]

    slow n = liftIO $ do
        putStrLn $ "/slow/" ++ show n
        return $ slowSource n

    slowSource :: Int -> S.SourceT IO T.Text
    slowSource num =
      let go :: Int -> S.StepT IO T.Text
          go !n
            | n > num =
                S.Yield "</ol></body></html>" S.Stop
            | otherwise = S.Effect $ do
                threadDelay 500000
                pure $ S.Yield (LT.toStrict $ renderText $ resultItem n) (go (n+1))

          resultItem :: Int -> Html ()
          resultItem n = li_ $ a_ [href_ "todo"] (toHtml $ "result " <> T.pack (show n))
      in S.fromStepT $ S.Yield "<html><body><ol>" (go 1)

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Starting cookbook-basic-streaming at http://localhost:8000"
  port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
  Warp.run port app
