{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified MyLib
import qualified MyLib.Test
import Test.Hspec.Expectations.Pretty (shouldBe)
import qualified Test.Hspec as HSpec
import qualified Data.Set as Set
import Debug.Trace (trace)
import Control.Monad (forM_)
import qualified Control.Monad.ST as ST
import qualified Data.Graph.Digraph as DG
import qualified Data.List.NonEmpty as NE
import Data.Functor (void)

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main = MyLib.withGraphFromFile testDataFileName $ \graph -> do
  graphEdgeSet <- ST.stToIO (DG.toEdges graph)
  let graphEdges = Set.map void $ Set.fromList $ concat $ Set.map (NE.toList . DG.eMeta) graphEdgeSet
  let testCase test =
        HSpec.describe (MyLib.Test.queryTest_name test) $ do
          HSpec.it "edges are contained in the graph" $ do
            let ppFunctions = Set.fromList $
                  concatMap MyLib.Test.unPPFunctions $
                  Set.toList $
                  MyLib.Test.queryTest_expectedResult test
            graphEdges `isSupersetOf` ppFunctions
          HSpec.it "contained in top query results" $ do
            result <- ST.stToIO $ MyLib.Test.queryTest_runQuery test graph
            Set.fromList (map fst $ traceFunction result)
              `isSupersetOf`
                MyLib.Test.queryTest_expectedResult test
  HSpec.hspec $
    HSpec.describe "Unit tests" $ do
      HSpec.describe "Expected result" $
        forM_ MyLib.Test.allTestCases testCase
  where
    traceFunction = if shouldTrace then traceResults else id
    shouldTrace = True

    traceResults :: [(MyLib.Test.PPFunctions, Double)] -> [(MyLib.Test.PPFunctions, Double)]
    traceResults results =
        trace (unlines $ "" : map (\(path, weight) -> show weight <> ": " <> show path) results) results

isSupersetOf :: (Show a, Ord a) => Set.Set a -> Set.Set a -> IO ()
isSupersetOf superSet subSet =
  Set.intersection superSet subSet `shouldBe` subSet
