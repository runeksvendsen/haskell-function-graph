{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module Main where

import qualified FunGraph
import qualified FunGraph.Test
import Test.Hspec.Expectations.Pretty (shouldBe)
import qualified Test.Hspec as HSpec
import qualified Data.Set as Set
import Debug.Trace (trace)
import Control.Monad (forM_, when)
import qualified Control.Monad.ST as ST
import qualified Data.Graph.Digraph as DG
import qualified Data.List.NonEmpty as NE
import Data.Functor (void)
-- BEGIN: extra args
import Data.List
import qualified System.Environment as Arg
import qualified Control.Exception as Ex
import qualified System.Exit as Exit
import qualified Test.Hspec.Runner as Hspec
-- END: extra args

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
      summary <- Hspec.hspecResult spec
      when (not (Hspec.isSuccess summary) && not shouldTrace) $
        putStrLn $ "\nTest suite had failures. Run again with the " <> traceCLIArg <> " argument to print tracing information."
      Hspec.evaluateSummary summary

main'
  :: Bool
  -> FunGraph.Graph ST.RealWorld
  -> IO Hspec.Spec
main' shouldTrace graph = do
  graphEdgeSet <- ST.stToIO (DG.toEdges graph)
  let graphEdges = Set.map void $ Set.fromList $ concat $ Set.map (NE.toList . DG.eMeta) graphEdgeSet
  let testCase test =
        HSpec.describe (FunGraph.Test.queryTest_name test) $ do
          HSpec.it "edges are contained in the graph" $ do
            let ppFunctions = Set.fromList $
                  concatMap FunGraph.Test.unPPFunctions $
                  Set.toList $
                  FunGraph.Test.queryTest_expectedResult test
            graphEdges `isSupersetOf` ppFunctions
          HSpec.it "contained in top query results" $ do
            result <- ST.stToIO $ runQueryFunction graph $ FunGraph.Test.queryTest_runQuery test
            Set.fromList (map fst $ traceFunction result)
              `isSupersetOf`
                FunGraph.Test.queryTest_expectedResult test
  pure $ HSpec.describe "Unit tests" $ do
    HSpec.describe "Expected result" $
      forM_ FunGraph.Test.allTestCases testCase
  where
    traceFunction = if shouldTrace then traceResults else id

    runQueryFunction = if shouldTrace then FunGraph.runGraphActionTrace else FunGraph.runGraphAction

    traceResults :: [(FunGraph.Test.PPFunctions, Double)] -> [(FunGraph.Test.PPFunctions, Double)]
    traceResults results = if null results then results else -- make `traceResults []` a no-op
        trace (unlines $ "" : map (\(path, weight) -> show weight <> ": " <> show path) results) results

isSupersetOf :: (Show a, Ord a) => Set.Set a -> Set.Set a -> IO ()
isSupersetOf superSet subSet =
  Set.intersection superSet subSet `shouldBe` subSet

-- | Pass extra arguments to an IO action that parses CLI arguments.
--
--   The use case is an IO action that does CLI argument parsing by itself,
--   and would thus fail with "unknown argument" if you tried to run it with
--   your custom argument.
--
--   Example:
--
-- >>> :{
--  import qualified System.Environment as Arg
--  import System.IO.Unsafe (unsafePerformIO)
--  let myProgram = pure ()
--  unsafePerformIO $
--    Arg.withArgs ["--extra-test-arg", "--help"] $
--      withExtraArgs [("--extra-test-arg", "extra test arg")] $ \extraArgs -> do
--        getArgs <- Arg.getArgs
--        pure (getArgs, extraArgs)
-- :}
withExtraArgs
  :: [(String, String)]
     -- ^ @extraArgs@: extra arguments: (option, help text)
  -> ([String] -> IO a)
     -- ^ argument: the list of options from @extraArgs@ that was passed as arguments.
     --   the IO action is run without these arguments.
  -> IO a
     -- ^ IO action that is not passed the extra CLI arguments
withExtraArgs extraArgs ioAction = do
  args <- Arg.getArgs
  let passedArgs = intersect args (map fst extraArgs)
      remainingArgs = args \\ passedArgs
  Arg.withArgs remainingArgs $ do
    eitherA <- Ex.try $ ioAction passedArgs
    when (args == ["--help"]) $
      putStr $ unlines $
          ""
        : "EXTRA OPTIONS"
        : map (\(option, helpText) -> "    " <> option <> "  " <> helpText) extraArgs
    either (Ex.throwIO @Exit.ExitCode) pure eitherA
