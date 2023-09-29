{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified MyLib
import qualified MyLib.Examples as Examples
import Test.Hspec.Expectations.Pretty (shouldContain)
import Data.Functor (void)
import qualified Test.Hspec as HSpec

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main = do
  graphData <- either fail pure =<< MyLib.fileReadDeclarationMap testDataFileName
  let getResults' = map (map void) . getResults graphData
  HSpec.hspec $
    HSpec.describe "Unit tests" $
      HSpec.it "Strict ByteString to String" $ do
        getResults' (Examples.strictByteString, Examples.string)
          `shouldContain`
            [ [ MyLib.Function "toStrict" "Data.Text.Lazy" "text-2.0.2" ()
              , MyLib.Function "encodeUtf8" "Data.Text.Encoding" "text-2.0.2" ()
              ]
            ]
  where
    maxCount = 10
    maxCountSpTrees = maxCount

    getResults graphData (src, dst) =
      let res = concat $ map MyLib.spTreeToPaths $ MyLib.runQueryAll maxCountSpTrees (src, dst) graphData
          !first = take maxCount res
      in first

simpleQuery2 =
  ( Examples.lazyText
  , Examples.strictByteString
  )
