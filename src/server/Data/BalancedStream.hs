{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- | A stream where an item streamed earlier in the stream
--   must be terminated by an item streamed later in the stream.
--
--   See 'BalancedStream' for an example.
module Data.BalancedStream
( -- * @BalancedStream@
  BalancedStream
, toStream
, yield
, yieldBalanced
, yieldBalancedM
, liftStream
  -- * Generic 'S.Stream' functions
, appendStreamAccum
, returnStreamAccum
  -- * Re-exports
, S.Stream, S.Of
)
where

import qualified Streaming.Prelude as S
import Data.Bifunctor (first)
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Streaming as S

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
--
--  Example 3: Using 'yieldBalancedM':
--
-- >>> let stream = yield "BEGIN " >> yieldBalancedM "(" ")" (yield "i+1") >> yield " END"
-- >>> concat $ Data.Functor.Identity.runIdentity $ Streaming.Prelude.toList_ (toStream stream)
-- "BEGIN (i+1) END"
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
  lift = liftStream . lift

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

-- | Insert a 'BalancedStream' between an initial an terminating stream item.
--
-- Example:
--
-- >>> let stream = yieldBalancedM "START" "END" $ yield "1" >> yield "2" >> yield "3"
-- >>> concat $ Data.Functor.Identity.runIdentity $ Streaming.Prelude.toList_ (toStream stream)
-- "START123END"
yieldBalancedM
  :: Monad m
  => a -- ^ Initial item
  -> a -- ^ Terminating item
  -> BalancedStream a m r -- ^ Between initial and terminating item
  -> BalancedStream a m r
yieldBalancedM a1 a2 bs =
  liftStream $
    toStream $
      yieldBalanced a1 a2 >> bs

-- | Append an element to the end of a stream based on all previously streamed elements.
--
-- Example:
--
-- >>> import Data.Functor.Identity
-- >>> import Streaming.Prelude
-- >>> let yieldSum = const $ Streaming.Prelude.yield . Prelude.sum
-- >>> toList_ $ appendStreamAccum yieldSum $ S.each [1, 2, 3, 4]
-- [1,2,3,4,10]
appendStreamAccum
  :: Monad m
  => (r -> [a] -> S.Stream (S.Of a) m r')
  -- ^ @mkStream@ function. Append an element to the end of the original stream.
  --   Arguments:
  --      2. Return value of original stream
  --      1. All streamed items in the original stream (in reverse order)
  -> S.Stream (S.Of a) m r
  -- ^ Original stream
  -> S.Stream (S.Of a) m r'
  -- ^ Original stream with the result of the @mkStream@ function appended to it.
appendStreamAccum mkStream stream =
  S.map fst $ go [] stream
  where
    go accum s =
      lift (S.inspect s) >>= \case
        Left r -> S.map (,accum) $ mkStream r accum
        Right (a S.:> s') -> do
          let accum' = a : accum
          S.yield (a, accum')
          go accum' s'

-- | Make a stream return all previously streamed elements
returnStreamAccum
  :: Monad m
  => S.Stream (S.Of a) m r
  -- ^ Original stream
  -> S.Stream (S.Of a) m [a]
  -- ^ Original stream with a return value of all previously streamed elements
returnStreamAccum = appendStreamAccum (const pure)
