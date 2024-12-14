{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main
(main)
where
import Criterion.Main
import qualified FunGraph
import qualified FunGraph.Test
import qualified Control.Monad.ST as ST
import Data.Functor (void, (<&>))
import Control.Monad ((<=<))
import qualified Data.List.NonEmpty as NE
import qualified FunGraph.Test.Util
import qualified Server.GraphViz
import qualified FunGraph.Util
import qualified Control.Exception as Ex

dataFileName :: FilePath
dataFileName = FunGraph.Test.Util.testDataFileName

main :: IO ()
main = do
  (graphData, mutGraph, frozenGraph, queryResults) <- setupEnv
  defaultMain
    [ bgroup "Graph"
      [ bench "Create" $ nfAppIO (ST.stToIO . void . FunGraph.buildGraphMut FunGraph.defaultBuildConfig) graphData
      , bench "Read graph data list" $ nfIO (FunGraph.fileReadDeclarationMap dataFileName)
      , bench "Freeze" $ nfAppIO (ST.stToIO . FunGraph.freeze) mutGraph
      , bench "Thaw" $ nfAppIO (void . ST.stToIO . FunGraph.thaw) frozenGraph
      , bench "Thaw+freeze" $ nfAppIO (void . ST.stToIO . (FunGraph.freeze <=< FunGraph.thaw)) frozenGraph
      ]
    , bgroup "Query" $
        map (queryPaths mutGraph) FunGraph.Test.allTestCases
    , bgroup "UI"
        [ bgroup "Dot graph rendering" $
            map (\(queryResult, name) -> bench name $ nfAppIO createRenderGraph queryResult) queryResults
        ]
    ]
  where
    setupEnv = do
      graphData <- readGraphData dataFileName
      mutGraph <- ST.stToIO $ FunGraph.buildGraphMut FunGraph.defaultBuildConfig graphData
      frozenGraph <- ST.stToIO $ buildGraphFreeze graphData
      queryResults <- mapM (runQuery mutGraph) FunGraph.Test.allTestCases
      pure (graphData, mutGraph, frozenGraph, queryResults)

    queryPaths mutGraph test =
      let args@(maxCount, _) = FunGraph.Test.queryTest_args test
          nameWithMaxCount = FunGraph.Test.queryTest_name test <> " maxCount=" <> show maxCount
      in bgroup nameWithMaxCount $ NE.toList $
        FunGraph.Test.mkQueryFunctions False <&> \(queryFunctionName, queryFunction) ->
          bench queryFunctionName $
            nfAppIO (queryFunction args) mutGraph

    readGraphData fileName =
      either fail pure =<< FunGraph.fileReadDeclarationMap fileName

    buildGraphFreeze graphData =
      FunGraph.buildGraphMut FunGraph.defaultBuildConfig graphData >>= FunGraph.freeze

    runQuery mutGraph test =
      let (maxCount, srcDst) = FunGraph.Test.queryTest_args test
          name = FunGraph.Test.queryTest_name test
      in fmap (,name) $ throwErrorIO $
        ST.stToIO $ FunGraph.runGraphAction mutGraph $ FunGraph.queryTreeGA maxCount srcDst

    createRenderGraph queryResult = throwErrorIO $
      ST.stToIO (FunGraph.Util.graphFromQueryResult queryResult)
      >>= ST.stToIO . FunGraph.Util.graphToDot ""
      >>= Server.GraphViz.renderDotGraph

    throwErrorIO :: Show a => IO (Either a b) -> IO b
    throwErrorIO = (either (Ex.throwIO . Ex.ErrorCall . show) return =<<)
