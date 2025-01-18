module Main (main) where

import qualified Spec.PrioTrie
import qualified Test.Tasty as Tasty

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup "Properties"
      [ Spec.PrioTrie.spec
      ]
