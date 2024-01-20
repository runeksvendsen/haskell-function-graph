module Main where

import Criterion.Main
import qualified MyLib
import qualified MyLib.Test
import qualified Control.Monad.ST as ST
import Data.Functor (void)
import Control.Monad ((<=<))

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main = do
  graphData <- readGraphData testDataFileName
  mutGraph <- ST.stToIO $ MyLib.buildGraphMut graphData
  frozenGraph <- ST.stToIO $ buildGraphFreeze graphData
  defaultMain
    [ bgroup "Graph"
      [ bench "Create" $ nfAppIO (ST.stToIO . void . MyLib.buildGraphMut) graphData
      , bench "Freeze" $ nfAppIO (ST.stToIO . MyLib.freeze) mutGraph
      , bench "Thaw" $ nfAppIO (void . ST.stToIO . MyLib.thaw) frozenGraph
      , bench "Thaw+freeze" $ nfAppIO (void . ST.stToIO . (MyLib.freeze <=< MyLib.thaw)) frozenGraph
      ]
    , bgroup "Query"
      [ bgroup "runQueryAll" $
          map (runQueryAll mutGraph) MyLib.Test.allTestCases
      ]
    ]
  where
    runQueryAll mutGraph test =
      bench (MyLib.Test.queryTest_name test) $
        nfAppIO (ST.stToIO . MyLib.Test.queryTest_runQuery test) mutGraph

    readGraphData fileName =
      either fail pure =<< MyLib.fileReadDeclarationMap fileName

    buildGraphFreeze graphData =
      MyLib.buildGraphMut graphData >>= MyLib.freeze
