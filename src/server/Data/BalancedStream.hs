{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
-- | A stream where an item streamed earlier in the stream
--   must be terminated by an item streamed later in the stream
module Data.BalancedStream
( BalancedStream
, toStream
, yield
, yieldBalanced
, yieldBalancedM
, liftStream
)
where

import qualified Streaming.Prelude as S
import Data.Bifunctor (first)
import Control.Monad.Trans.Class (MonadTrans(..))

-- | A stream where an item streamed earlier in the stream
--   must be terminated by an item streamed later in the stream.
--
--  Example 1: we're streaming a string that expresses a mathematical calculation,
--  and we want an opening parenthesis to be terminated by a closing one, e.g. @1 * (3 - (2 + 1))@:
--
--  >>> let yieldParens = yieldBalanced "(" ")"
--  >>>     stream = yield "1 * " >> yieldParens >> yield "3 - " >> yieldParens >> yield "2 + 1"
--  >>> concat $ Data.Functor.Identity.runIdentity $ Streaming.Prelude.toList_ (toStream stream)
-- "1 * (3 - (2 + 1))"
--
--  Example 2: different kinds of opening/closing characters:
--
--  >>> let stream = yieldBalanced "{ " " }" >> yield "a" >> yieldBalanced "[" "]" >> yieldBalanced "(" ")" >> yield "i+1"
--  >>> concat $ Data.Functor.Identity.runIdentity $ Streaming.Prelude.toList_ (toStream stream)
-- "{ a[(i+1)] }"
newtype BalancedStream a m r = BalancedStream
  { unBalancedStream :: S.Stream (S.Of a) m (r, [[a]])
    -- ^ The @S.Stream (S.Of a) m r@ is the first part of the stream.
    --   The @[[a]]@ is the last part of the stream (closes things opened by 'yieldBalanced')
  }

instance Monad m => Functor (BalancedStream a m) where
  fmap f bs =
    bs{ unBalancedStream = fmap (first f) (unBalancedStream bs)
      }

instance Monad m => Semigroup (BalancedStream a m r) where
  b1 <> b2 = b1 >> b2

instance (Monoid r, Monad m) => Monoid (BalancedStream a m r) where
  mempty = BalancedStream mempty

instance Monad m => Applicative (BalancedStream a m) where
  pure a = BalancedStream $ pure (a, [])
  (<*>)
    :: Monad m
    => BalancedStream a m (r -> r')
    -> BalancedStream a m r
    -> BalancedStream a m r'
  bsF <*> bsR = do
    f <- bsF
    f <$> bsR

instance Monad m => Monad (BalancedStream a m) where
  (>>=)
    :: Monad m
    => BalancedStream a m r
    -> (r -> BalancedStream a m r')
    -> BalancedStream a m r'
  BalancedStream s >>= f =
    BalancedStream $ do
      (r, state) <- s
      (r', state') <- unBalancedStream $ f r
      pure (r', state' <> state)

instance MonadTrans (BalancedStream a) where
  lift s = BalancedStream $ (,[[]]) <$> lift s

liftStream
  :: Monad m
  => S.Stream (S.Of a) m r
  -> BalancedStream a m r
liftStream s = BalancedStream $ (,[]) <$> s

toStream
  :: Monad m
  => BalancedStream a m r
  -> S.Stream (S.Of a) m r
toStream (BalancedStream s) = do
  (r, state) <- s
  S.each $ concat state
  pure r

-- | Unbalanced 'S.yield'
yield
  :: Monad m
  => a
  -> BalancedStream a m ()
yield a =
  BalancedStream $ (,[]) <$> S.yield a

-- | Balanced 'S.yield'
yieldBalanced
  :: Monad m
  => a -- ^ Initial item
  -> a -- ^ Terminating item
  -> BalancedStream a m ()
yieldBalanced a1 a2 =
  BalancedStream $ (,[[a2]]) <$> S.yield a1

-- ^ @yieldBalancedM initial terminating bs@ is just @yieldBalanced a1 a2 >> bs@.
--
--   TODO: allows nesting do-actions inside tags
yieldBalancedM
  :: Monad m
  => a -- ^ Initial item
  -> a -- ^ Terminating item
  -> BalancedStream a m () -- ^ Between initial and terminating item
  -> BalancedStream a m ()
yieldBalancedM a1 a2 bs =
  yieldBalanced a1 a2 >> bs

