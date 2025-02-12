{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Servant.API.Stream.ToSourceIO for the `streaming` package's `Stream` type
module Server.Api
  ( Api
  , Root
  , Search
  , Typeahead
  , HxBoosted(..)
  , NoGraph(..)
  ) where

import Servant.API
import Servant.API.Stream
import Servant.Types.SourceT
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Servant.HTML.Lucid (HTML)
import Lucid (Html)
import qualified Data.Text as T
import Server.HtmlStream (HtmlStream)

type Api = Root :<|> Search :<|> Typeahead

type Root
  =  StreamGet NoFraming HTML (HtmlStream IO ())

type Search
  =  "search"
  :> Header "HX-Boosted" HxBoosted -- if present, we send only a fragment of a page, otherwise a whole page
  :> QueryParam "src" T.Text
  :> QueryParam "dst" T.Text
  :> QueryParam "limit" Word
  :> QueryParam "no_graph" NoGraph
  :> StreamGet NoFraming HTML (HtmlStream IO ())

type Typeahead
  =  "typeahead"
  :> QueryParam "src" T.Text -- NB: matches input_ "name" in Server.Pages.Root.form
  :> QueryParam "dst" T.Text -- NB: matches input_ "name" in Server.Pages.Root.form
  :> Get '[HTML] (Html ())

-- | Signals that the @HX-Boosted@ HTTP header is set
data HxBoosted = HxBoosted
  deriving Show

instance FromHttpApiData HxBoosted where
  parseQueryParam _ = pure HxBoosted

instance ToHttpApiData HxBoosted where
  toQueryParam HxBoosted = ""

-- | If absent, draw a graph (otherwise don't)
data NoGraph = NoGraph
  deriving Show

instance FromHttpApiData NoGraph where
  parseQueryParam _ = pure NoGraph

instance ToHttpApiData NoGraph where
  toQueryParam NoGraph = ""

type StreamIO a = S.Stream (S.Of a) IO ()

instance Servant.API.Stream.ToSourceIO a (StreamIO a) where
  toSourceIO stream =
    let go s = Effect $
          S.inspect s >>= \case
            Left () -> pure Stop
            Right (a S.:> s') -> pure $ Yield a (go s')
    in fromStepT $ go stream

instance Servant.API.Stream.FromSourceIO a (StreamIO a) where
  fromSourceIO (SourceT m) =
    S.effect $ m (pure . stepToStream)
    where
      stepToStream :: StepT IO a -> S.Stream (S.Of a) IO ()
      stepToStream =
        let go = \case
              Stop -> pure ()
              Error err -> S.lift $ fail err
              Skip s -> go s
              Yield x s -> S.yield x >> go s
              Effect ms -> S.lift ms >>= go
        in go
