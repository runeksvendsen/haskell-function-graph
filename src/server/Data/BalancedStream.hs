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
, timeoutStream
  -- * Re-exports
, S.Stream, S.Of
)
where

import qualified Streaming.Prelude as S
import Data.Bifunctor (first)
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Streaming as S
import qualified GHC.Clock
import qualified System.Timeout
import Data.Word (Word64)
import Data.Int (Int64)

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

-- | TODO: allows @BEFORE { blah } AFTER@ instead of @BEFORE { blah AFTER }@.
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
-- >>> let yieldSum = Streaming.Prelude.yield . Prelude.sum
-- >>> toList_ $ appendStreamAccum yieldSum $ S.each [1, 2, 3, 4]
-- [1,2,3,4,10]
appendStreamAccum
  :: Monad m
  => ([a] -> S.Stream (S.Of a) m r')
  -- ^ @mkStream@ function. Append an element to the end of the original stream.
  --   Argument: all streamed items in the original stream (in reverse order).
  -> S.Stream (S.Of a) m r
  -- ^ Original stream
  -> S.Stream (S.Of a) m r'
  -- ^ Original stream with the result of the @mkStream@ function appended to it.
appendStreamAccum mkStream stream =
  S.map fst $ go [] stream
  where
    go accum s =
      lift (S.inspect s) >>= \case
        Left _ -> S.map (,accum) $ mkStream accum
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
returnStreamAccum = appendStreamAccum pure

-- | 'System.Timeout.timeout' for streams.
--
-- Limit a stream to the elements that are produced within the specified timeout.
--
-- Example:
--
-- >>> :set -XNumericUnderscores
-- >>> import qualified Streaming.Prelude as S
-- >>> let stream = S.yield "hello" >> S.repeatM (Control.Concurrent.threadDelay 10_000 >> pure "hello")
-- >>> S.toList_ (timeoutStream 105_000 stream)
-- >>> S.toList_ (timeoutStream 25_000 stream)
-- >>> S.toList_ (timeoutStream 1 stream)
-- ["hello","hello","hello","hello","hello","hello","hello","hello","hello","hello"]
-- ["hello","hello","hello"]
-- []
timeoutStream
  :: Word64
  -- ^ Timeout in microseconds
  -> S.Stream (S.Of a) IO r
  -- ^ Original stream
  -> S.Stream (S.Of a) IO (Maybe r)
  -- ^ Time-limited stream. Returns a 'Just' if no elements were removed, otherwise 'Nothing'.
timeoutStream timeoutMicros stream = do
  startTimeNanos <- fromIntegral <$> lift GHC.Clock.getMonotonicTimeNSec
  let endTimeNanos :: Int64
      endTimeNanos = startTimeNanos + (fromIntegral timeoutMicros * 1000)
  go endTimeNanos stream
  where
    go endTimeNanos s = do
      currentTimeNanos <- fromIntegral <$> lift GHC.Clock.getMonotonicTimeNSec
      -- NOTE: Int64 is used instead of Word64 to avoid the below subtraction causing overflow
      let timeLeftMicros = fromIntegral $ (endTimeNanos - currentTimeNanos) `div` 1000
      if timeLeftMicros <= 0
        then pure Nothing
        else lift (System.Timeout.timeout timeLeftMicros $ S.inspect s) >>= \case
          Nothing -> -- timed out
            pure Nothing
          Just eRes -> -- did not time out
            case eRes of
              Left r -> -- reached end of the stream
                pure (Just r)
              Right (a S.:> s') -> -- the stream yielded an element within the time limit
                S.yield a >> go endTimeNanos s'
