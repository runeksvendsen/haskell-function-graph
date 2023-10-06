{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api where

import Servant.API

type MyApi = "hello" :> Get '[JSON] String