{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api where

import Servant.API
import Servant.HTML.Lucid
import Lucid (Html)

type Root = "yo" :> Get '[HTML] (Html ())
