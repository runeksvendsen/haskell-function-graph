{-# LANGUAGE LambdaCase #-}
module Streaming.Prelude.Extras
( timeoutStream
)
where

import qualified Streaming.Prelude as S
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Streaming as S
import qualified GHC.Clock
import qualified System.Timeout

-- | 'System.Timeout.timeout' for streams.
--
-- Limit a stream to the elements that are produced within the specified timeout.
--
-- Example:
--
-- >>> :set -XNumericUnderscores
-- >>> import qualified Streaming.Prelude as S
-- >>> let stream = S.take 10 $ S.iterateM (\count -> Control.Concurrent.threadDelay 10_000 >> pure (count+1)) (pure 1)
-- >>> S.toList_ (timeoutStream 55_000 stream)
-- >>> S.toList_ (timeoutStream 25_000 stream)
-- >>> S.toList_ (timeoutStream 1 stream)
-- >>> S.toList_ (timeoutStream minBound stream)
-- >>> S.toList_ (timeoutStream maxBound stream)
-- [1,2,3,4,5,6]
-- [1,2,3]
-- []
-- []
-- [1,2,3,4,5,6,7,8,9,10]
timeoutStream
  :: Int
  -- ^ Timeout in microseconds
  -> S.Stream (S.Of a) IO r
  -- ^ Original stream
  -> S.Stream (S.Of a) IO (Maybe r)
  -- ^ Time-limited stream. Returns a 'Just' if no elements were removed, otherwise 'Nothing'.
timeoutStream timeoutMicros stream = do
  startTimeNanos <- fromIntegral <$> lift GHC.Clock.getMonotonicTimeNSec
  -- NOTE: Integer is used to avoid having to think about under/overflow
  let endTimeNanos :: Integer
      endTimeNanos = startTimeNanos + (fromIntegral timeoutMicros * 1000)
  go endTimeNanos stream
  where
    go :: Integer
       -> S.Stream (S.Of a) IO r
       -> S.Stream (S.Of a) IO (Maybe r)
    go endTimeNanos s = do
      currentTimeNanos <- fromIntegral <$> lift GHC.Clock.getMonotonicTimeNSec
      let timeLeftMicros = (endTimeNanos - currentTimeNanos) `div` 1000
      if timeLeftMicros <= 0
        then pure Nothing
        else lift (System.Timeout.timeout (fromIntegral timeLeftMicros) $ S.inspect s) >>= \case
          Nothing -> -- timed out
            pure Nothing
          Just eRes -> -- did not time out
            case eRes of
              Left r -> -- reached end of the stream
                pure (Just r)
              Right (a S.:> s') -> -- the stream yielded an element within the time limit
                S.yield a >> go endTimeNanos s'
