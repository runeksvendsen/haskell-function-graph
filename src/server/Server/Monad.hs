{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Monad
( AppM, runAppM
, Graph
)
where

import MyLib
import Control.Monad.Reader (Reader)
import qualified Control.Monad.Reader as R
import Data.Functor.Identity (runIdentity)

newtype AppM a = AppM { unAppM :: Reader FrozenGraph a }
  deriving (Functor, Applicative, Monad)

runAppM :: FrozenGraph -> AppM a -> a
runAppM g m = runIdentity $ R.runReaderT (unAppM m) g
