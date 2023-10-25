module Main where

import Criterion.Main
import qualified MyLib
import MyLib.Examples
import qualified Control.Monad.ST as ST
import Data.Functor (void)
import Control.Monad ((<=<))
import Debug.Trace

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main = do
  graphData <- readGraphData testDataFileName
  frozenGraph <- buildGraphFreezeIO graphData
  defaultMain
    [ bgroup "Graph"
      [ bench "Create" $ nfAppIO (ST.stToIO . void . MyLib.buildGraphMut) graphData
      , bench "Create+freeze" $ nfAppIO buildGraphFreezeIO graphData
      , bench "Thaw" $ nfAppIO (void . ST.stToIO . MyLib.thaw) frozenGraph
      , bench "Thaw+freeze" $ nfAppIO (void . ST.stToIO . (MyLib.freeze <=< MyLib.thaw)) frozenGraph
      ]
    , bgroup "Query"
      [ bgroup "runQueryAll"
        [ runQueryAll 1 (strictByteString, string) frozenGraph
        , runQueryAll 1 (string, strictByteString) frozenGraph
        , runQueryAll 11 (lazyText, strictByteString) frozenGraph
        , runQueryAll 37 (strictByteString, lazyText) frozenGraph
        ]
      , bgroup "runQuerySingleResult"
        [ bench "TODO" $ nf (trace "HEY" . MyLib.runQuerySingleResult (fst strictByteString, fst string)) frozenGraph
        ]
      ]
    ]
  where
    runQueryAll maxCount (src, dst) frozenGraph =
      bench (snd src <> " -> " <> snd dst) $
          nf (MyLib.runQueryAll maxCount (fst src, fst dst)) frozenGraph

    readGraphData fileName =
      either fail pure =<< MyLib.fileReadDeclarationMap fileName

    buildGraphFreeze graphData =
      MyLib.buildGraphMut graphData >>= MyLib.freeze

    buildGraphFreezeIO = ST.stToIO . buildGraphFreeze

yourFunction1 :: Int -> Int
yourFunction1 x = x * 2

yourFunction2 :: Int -> Int
yourFunction2 x = x * 3

input1, input2 :: Int
input1 = 10
input2 = 20
