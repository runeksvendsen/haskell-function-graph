{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
-- | Benchmark the web server
--
-- TODO: Make exe exit properly for CLI arguments that don't need server (e.g. --help, --list)
module Main (main) where

import FunGraph.Test.Util
import qualified Server.Api
import qualified FunGraph.Test
import qualified Server
import qualified Server.Pages.Search
import qualified Lucid
import Criterion.Main

import Data.Functor ((<&>))
import Streaming.Prelude (Stream, Of)
import qualified Streaming.Prelude
import qualified Lucid.Base
import qualified Data.Text.Lazy as LT
import Streaming (lift)
import Control.Monad (when)
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy as BSL

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main =
  Server.withHandlers logger mempty testDataFileName $ \handlers ->
    runWarpTestRandomPort (Server.app handlers) $ \port ->
      mkQueryFunction port >>= runTests
  where
    logger = const $ pure ()

runTests
  :: ((StreamIOHtml -> StreamIOHtml) -> Maybe Server.Api.NoGraph -> FunGraph.Test.QueryTest -> IO BSL.ByteString)
  -> IO ()
runTests runQuery = do
  defaultMain
    [ bgroup "Web server" $
      [ ("(all results)", id)
      , ("(first result)", untilFirstResult) -- WIP: verify this actually works
      ] <&> \(postFix, modifyStream) ->
        bgroup ("search " ++ postFix)
          [ bgroup "with graph" $
              map (benchHttpRequest modifyStream Nothing) FunGraph.Test.allTestCases
          , bgroup "without graph" $
              map (benchHttpRequest modifyStream (Just Server.Api.NoGraph)) FunGraph.Test.allTestCases
          ]
    ]
  where
    benchHttpRequest
      :: (StreamIOHtml -> StreamIOHtml)
      -> Maybe Server.Api.NoGraph
      -> FunGraph.Test.QueryTest
      -> Benchmark
    benchHttpRequest modifyStream mNoGraph qt =
      let maxCount = fst $ FunGraph.Test.queryTest_args qt
      in bench (FunGraph.Test.queryTest_name qt <> " maxCount=" <> show maxCount) $
        nfIO $ runQuery modifyStream mNoGraph qt

    untilFirstResult
      :: StreamIOHtml
      -> StreamIOHtml
    untilFirstResult s = do
      let Lucid.Base.Attribute resultHtmlAttributeText _ = Server.Pages.Search.mkResultAttribute ""
          containsSearchResult :: Lucid.Html () -> Bool
          containsSearchResult = LT.isInfixOf (LT.fromStrict resultHtmlAttributeText) . Lucid.renderText
      maybeNotContainsSearchResult <- takeWhileMaybe (not . containsSearchResult) s
      when (isJust maybeNotContainsSearchResult) $
        fail "Search result not found"

    -- Same as 'Streaming.Prelude.takeWhile' except returns 'Nothing' if the predicate returned False at some point
    --  (so that the number of stream elements was reduced).
    -- In other words: returns a 'Just' if the output stream is the same as the input stream.
    takeWhileMaybe :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m (Maybe r)
    takeWhileMaybe thePred =
      let loop str = lift (Streaming.Prelude.next str) >>= \case
            Right (a, as) ->
              if thePred a
                then Streaming.Prelude.yield a >> loop as
                else pure Nothing
            Left r ->
              pure $ Just r
      in loop
