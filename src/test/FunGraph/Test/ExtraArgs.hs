{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE LambdaCase #-}

module FunGraph.Test.ExtraArgs where

-- import FunGraph.Test.Util
-- import qualified FunGraph
-- import qualified FunGraph.Test
-- import Test.Hspec.Expectations.Pretty (shouldBe)
-- import qualified Test.Hspec as HSpec
-- import qualified Data.Set as Set
-- import Debug.Trace (trace)
-- import Control.Monad (forM_, when)
-- import qualified Control.Monad.ST as ST
-- import qualified Data.Graph.Digraph as DG
-- import qualified Data.List.NonEmpty as NE
-- import Data.Functor (void)

-- BEGIN: extra args
import Data.List
import qualified System.Environment as Arg
import qualified Control.Exception as Ex
import qualified System.Exit as Exit
import Control.Monad (when)
-- END: extra args

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
