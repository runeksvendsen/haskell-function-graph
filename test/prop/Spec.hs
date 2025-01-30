module Main (main) where

import qualified Spec.PrioTrie
import qualified Test.Tasty as Tasty
import qualified System.IO.Unsafe as Unsafe

main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup "Properties"
      [ Tasty.withResource Spec.PrioTrie.setup mempty $
          -- NOTE: For some reason, 'Tasty.withResource' provides the resource to the 'TestTree' as an IO action.
          --       We get around this by using 'unsafePerformIO'. Would be nice if tasty didn't require this.
          --       Note the docs for 'Tasty.withResource' say that the "setup" IO action will be evaluated
          --       only once regardless of how many times the IO action below is evaluted.
          Spec.PrioTrie.spec . Unsafe.unsafePerformIO
      ]
