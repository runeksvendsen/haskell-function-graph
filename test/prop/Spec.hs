module Main (main) where

import qualified Spec.PrioTrie
import qualified Test.Tasty as Tasty

main :: IO ()
main = do
  prioTrieSpecSetup <- Spec.PrioTrie.setup
  Tasty.defaultMain $
    Tasty.testGroup "Properties"
      [ Spec.PrioTrie.spec prioTrieSpecSetup
      ]
