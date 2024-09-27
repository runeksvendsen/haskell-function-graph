{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import FunGraph.Test.Util
import FunGraph.Test.ExtraArgs
import qualified FunGraph
import qualified FunGraph.Test
import qualified Test.Hspec as HSpec
import qualified Test.Hspec.Runner as HSpec
import qualified Data.Set as Set
import Debug.Trace (trace)
import Control.Monad (forM_, when)
import qualified Control.Monad.ST as ST
import qualified Data.Graph.Digraph as DG
import qualified Data.List.NonEmpty as NE
import Data.Functor (void)

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main =
  withTraceArg $ \shouldTrace ->
    FunGraph.withGraphFromFile FunGraph.defaultBuildConfig testDataFileName $ \graph -> do
      spec <- main' shouldTrace graph
      runHspecWithTraceTip shouldTrace spec
  where
    traceCLIArg :: String
    traceCLIArg = "--trace"

    withTraceArg ioa =
      withExtraArgs [(traceCLIArg, "print tracing information")] $ \args ->
        ioa (args == [traceCLIArg])

    runHspecWithTraceTip shouldTrace spec = do
      summary <- HSpec.hspecResult spec
      when (not (HSpec.isSuccess summary) && not shouldTrace) $
        putStrLn $ "\nTest suite had failures. Run again with the " <> traceCLIArg <> " argument to print tracing information."
      HSpec.evaluateSummary summary

main'
  :: Bool
  -> FunGraph.Graph ST.RealWorld
  -> IO HSpec.Spec
main' shouldTrace graph = do
  graphEdgeSet <- ST.stToIO (DG.toEdges graph)
  let graphEdges = Set.map void $ Set.fromList $ concat $ Set.map (NE.toList . DG.eMeta) graphEdgeSet
  let testCase test =
        let (maxCount, _) = FunGraph.Test.queryTest_args test
        in HSpec.describe (FunGraph.Test.queryTest_name test <> " maxCount=" <> show maxCount) $ do
          HSpec.it "edges are contained in the graph" $ do
            let ppFunctions = Set.fromList $
                  concatMap FunGraph.Test.unPPFunctions $
                  Set.toList $
                  FunGraph.Test.queryTest_expectedResult test
            graphEdges `isSupersetOf` ppFunctions
          HSpec.it "contained in top query results" $ do
            let queryTestStream = FunGraph.Test.queryTreeAndPathsGAStreamTest timeout args
                args = FunGraph.Test.queryTest_args test
                timeout = 1000
            eResult <- queryTestStream graph
            result <- either handleError pure eResult
            Set.fromList (map fst $ traceFunction result)
              `isSupersetOf`
                FunGraph.Test.queryTest_expectedResult test
  pure $ HSpec.describe "Unit tests" $ do
    HSpec.describe "Expected result" $
      HSpec.describe "Stream" $
        forM_ FunGraph.Test.allTestCases testCase
  where
    traceFunction = if shouldTrace then traceResults else id

    runQueryFunction = if shouldTrace then FunGraph.runGraphActionTrace else FunGraph.runGraphAction

    handleError = \case
      FunGraph.GraphActionError_NoSuchVertex v ->
        fail $ "unknown vertex " <> show (FunGraph.renderFullyQualifiedType v)

    traceResults :: [(FunGraph.Test.PPFunctions, Double)] -> [(FunGraph.Test.PPFunctions, Double)]
    traceResults results = if null results then results else -- make `traceResults []` a no-op
        trace (unlines $ "" : map (\(path, weight) -> show weight <> ": " <> show path) results) results
