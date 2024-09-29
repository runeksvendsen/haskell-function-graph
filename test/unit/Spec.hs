{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE LambdaCase #-}

module Main
(main)
where

import FunGraph.Test.Util
import FunGraph.Test.ExtraArgs
import qualified FunGraph
import qualified FunGraph.Test
import qualified Test.Hspec as HSpec
import qualified Test.Hspec.Runner as HSpec
import qualified Data.Set as Set
import Debug.Trace (trace)
import Control.Monad (forM_, when, unless)
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
      spec <- main' shouldTrace queryFunctions graph
      runHspecWithTraceTip shouldTrace spec
  where
    queryFunctions = NE.fromList
      [("Stream", queryTestStream)]
      -- TODO: list-based query function

    queryTestStream
      :: FunGraph.Test.Args
      -> FunGraph.Graph ST.RealWorld
      -> IO QueryResults
    queryTestStream =
      FunGraph.Test.queryTreeAndPathsGAStreamTest timeout
      where
        timeout = 1000

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

type QueryResults =
  Either (FunGraph.GraphActionError FunGraph.FullyQualifiedType) [(FunGraph.Test.PPFunctions, Double)]

main'
  :: Bool
  -> NE.NonEmpty (String, FunGraph.Test.Args -> FunGraph.Graph ST.RealWorld -> IO QueryResults)
  -> FunGraph.Graph ST.RealWorld
  -> IO HSpec.Spec
main' shouldTrace queryFunctions graph = do
  graphEdgeSet <- ST.stToIO (DG.toEdges graph)
  let graphEdges = Set.map void $ Set.fromList $ concat $ Set.map (NE.toList . DG.eMeta) graphEdgeSet
  let testCase test =
        let args@(maxCount, _) = FunGraph.Test.queryTest_args test
        in HSpec.describe (FunGraph.Test.queryTest_name test <> " maxCount=" <> show maxCount) $ do
          HSpec.it "edges are contained in the graph" $ do
            let ppFunctions = Set.fromList $
                  concatMap FunGraph.Test.unPPFunctions $
                  Set.toList $
                  FunGraph.Test.queryTest_expectedResult test
            graphEdges `isSupersetOf` ppFunctions
          HSpec.describe "contained in top query results" $
            forM_ queryFunctions $ \(name, queryFunction) ->
              HSpec.it name $ do
                eResult <- queryFunction args graph
                result <- either handleError pure eResult
                -- Fail unless we get at least 'maxCount' results.
                -- If this fails then reduce maxCount to the minimum required to pass.
                let resultCount = length result
                unless (resultCount == 0 || maxCount <= resultCount) $
                  fail $ unwords
                    [ "Unnecessarily high maxCount."
                    , "maxCount is"
                    , show maxCount
                    , "but only got"
                    , show resultCount
                    , "results."
                    , "Results:"
                    , show $ map fst result
                    ]
                Set.fromList (map fst $ traceFunction result)
                  `isSupersetOf`
                    FunGraph.Test.queryTest_expectedResult test
  pure $ HSpec.describe "Unit tests" $ do
    HSpec.describe "Expected result" $
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
