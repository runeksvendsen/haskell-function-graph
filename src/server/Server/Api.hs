{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api where

import Servant.API
import Servant.HTML.Lucid
import Lucid (Html)
import qualified Data.Text as T

type Api = Root :<|> Search

type Root
  =  Get '[HTML] (Html ())

type Search
  =  "search"
  :> QueryParam "src" T.Text
  :> QueryParam "dst" T.Text
  :> QueryParam "limit" Word
  :> Get '[HTML] (Html ())
