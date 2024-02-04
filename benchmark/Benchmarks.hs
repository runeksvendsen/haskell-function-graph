module Main where

import Criterion.Main
import qualified FunGraph
import qualified FunGraph.Test
import qualified Control.Monad.ST as ST
import Data.Functor (void)
import Control.Monad ((<=<))

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main = do
  graphData <- readGraphData testDataFileName
  mutGraph <- ST.stToIO $ FunGraph.buildGraphMut graphData
  frozenGraph <- ST.stToIO $ buildGraphFreeze graphData
  defaultMain
    [ bgroup "Graph"
      [ bench "Create" $ nfAppIO (ST.stToIO . void . FunGraph.buildGraphMut) graphData
      , bench "Freeze" $ nfAppIO (ST.stToIO . FunGraph.freeze) mutGraph
      , bench "Thaw" $ nfAppIO (void . ST.stToIO . FunGraph.thaw) frozenGraph
      , bench "Thaw+freeze" $ nfAppIO (void . ST.stToIO . (FunGraph.freeze <=< FunGraph.thaw)) frozenGraph
      ]
    , bgroup "Query"
      [ bgroup "runQueryAll" $
          map (runQueryAll mutGraph) FunGraph.Test.allTestCases
      ]
    ]
  where
    runQueryAll mutGraph test =
      bench (FunGraph.Test.queryTest_name test) $
        nfAppIO (\g -> ST.stToIO $ FunGraph.Test.queryTest_runQuery test (FunGraph.runQuery g)) mutGraph

    readGraphData fileName =
      either fail pure =<< FunGraph.fileReadDeclarationMap fileName

    buildGraphFreeze graphData =
      FunGraph.buildGraphMut graphData >>= FunGraph.freeze
