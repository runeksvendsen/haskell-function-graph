{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified MyLib
import qualified System.Environment as Env
import qualified Control.Monad.ST as ST
import qualified Data.Graph.Digraph as DG
import qualified Data.Map.Strict as Map
import qualified System.Timeout
import Control.Monad (when, forM_)
import Data.Maybe (listToMaybe, fromMaybe)
import Text.Read (readMaybe)

-- TODO:
--   * Remove functions with src/dst (because it seems all types are convertible to each other via these types):
--      * String / FastString / ShortByteString
--      * Bool / Bool#
--      * Int(32/64) + Word(32/64)
--      * (Lazy) ByteString
--      * (Lazy) Text
--      * ByteArray#
--
--      * of all of the above:
--         * []
--         * 'Ptr'
--         * + "#"

main :: IO ()
main = do
  [fileName, timoutMillisStr] <- Env.getArgs
  timoutMillis <- maybe (fail $ "Invalid timeout: " <> show timoutMillisStr) pure (readMaybe timoutMillisStr)
  MyLib.withGraphFromFile fileName $ \graph -> do
    vertices <- ST.stToIO $ DG.vertexLabels graph
    forM_ vertices $ \src ->
      forM_ vertices $ \dst -> do
        res <- query timoutMillis graph src dst
        case res of
          [] -> pure ()
          (fns,_):_ -> when (length fns > 5) $ do
            putStrLn . ((show (length fns) <> " ") <>) . MyLib.bsToStr $
              mconcat
                [ MyLib.unFullyQualifiedType src
                , " -> "
                , MyLib.unFullyQualifiedType dst
                , " : "
                , MyLib.renderComposedFunctions fns
                ]
  where
    maxCount = 1

    query timoutMillis graph src dst = fmap (fromMaybe []) $
      System.Timeout.timeout (timoutMillis * 1000) $
        ST.stToIO $
          MyLib.runQueryAllST (MyLib.runQuery graph) maxCount (src, dst)

data ListMaxMin a = ListMaxMin
  !Int -- capacity
  !(Map.Map Int a)

empty :: Int -> ListMaxMin a
empty cap = ListMaxMin cap mempty

insert :: (a -> Int) -> ListMaxMin a -> a -> Maybe (ListMaxMin a)
insert getSize (ListMaxMin cap items) item
  | Map.size items < cap = Just $
      let newMap = Map.insert (getSize item) item items
      in ListMaxMin cap newMap
  | Just min' <- listToMaybe $ Map.toAscList items = undefined
  | otherwise = undefined
  where
