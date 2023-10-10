{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api where

import Servant.API
import Servant.HTML.Lucid
import Lucid (Html)
import qualified Data.Text as T

type Root
  =  "root"
  :> QueryParam "src" T.Text
  :> QueryParam "dst" T.Text
  :> Get '[HTML] (Html ())
