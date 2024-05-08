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
  mutGraph <- ST.stToIO $ FunGraph.buildGraphMut FunGraph.defaultBuildConfig graphData
  frozenGraph <- ST.stToIO $ buildGraphFreeze graphData
  defaultMain
    [ bgroup "Graph"
      [ bench "Create" $ nfAppIO (ST.stToIO . void . FunGraph.buildGraphMut FunGraph.defaultBuildConfig) graphData
      , bench "Freeze" $ nfAppIO (ST.stToIO . FunGraph.freeze) mutGraph
      , bench "Thaw" $ nfAppIO (void . ST.stToIO . FunGraph.thaw) frozenGraph
      , bench "Thaw+freeze" $ nfAppIO (void . ST.stToIO . (FunGraph.freeze <=< FunGraph.thaw)) frozenGraph
      ]
    , bgroup "Query"
      [ bgroup "queryPaths" $
          map (queryPaths mutGraph) FunGraph.Test.allTestCases
      ]
    ]
  where
    queryPaths mutGraph test =
      bench (FunGraph.Test.queryTest_name test) $
        nfAppIO (\g -> ST.stToIO $ FunGraph.runGraphAction g $ FunGraph.Test.queryTest_runQuery test) mutGraph

    readGraphData fileName =
      either fail pure =<< FunGraph.fileReadDeclarationMap fileName

    buildGraphFreeze graphData =
      FunGraph.buildGraphMut FunGraph.defaultBuildConfig graphData >>= FunGraph.freeze
