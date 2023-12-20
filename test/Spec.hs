{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified MyLib
import qualified MyLib.Test
import Test.Hspec.Expectations.Pretty (shouldBe)
import qualified Test.Hspec as HSpec
import qualified Data.Set as Set
import Debug.Trace (trace)
import Control.Monad (forM_)

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main = MyLib.withGraphFromFile testDataFileName $ \graph -> do
  let testCase test =
        HSpec.it (MyLib.Test.queryTest_name test) $ do
          Set.fromList (traceFunction $ MyLib.Test.queryTest_runQuery test graph)
            `isSupersetOf`
              MyLib.Test.queryTest_expectedResult test
  HSpec.hspec $
    HSpec.describe "Unit tests" $ do
      HSpec.describe "Expected result contained in top query results" $
        forM_ MyLib.Test.allTestCases testCase
  where
    traceFunction = if shouldTrace then traceResults else id
    shouldTrace = False

    traceResults :: [MyLib.Test.PPFunctions] -> [MyLib.Test.PPFunctions]
    traceResults results =
        trace (unlines $ "" : map show results) results

isSupersetOf :: (Show a, Ord a) => Set.Set a -> Set.Set a -> IO ()
isSupersetOf superSet subSet =
  Set.intersection superSet subSet `shouldBe` subSet
