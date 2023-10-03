{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified MyLib
import qualified MyLib.Examples as Examples
import Test.Hspec.Expectations.Pretty (shouldBe)
import Data.Functor (void)
import qualified Test.Hspec as HSpec
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

testDataFileName :: FilePath
testDataFileName = "data/all3.json"

main :: IO ()
main = do
  graphData <- either fail pure =<< MyLib.fileReadDeclarationMap testDataFileName
  let getResults' = map (PPFunctions . map void) . getResults graphData
  HSpec.hspec $
    HSpec.describe "Unit tests" $ do
      HSpec.describe ("Expected result contained in top " <> show maxCount <> " query results") $ do
        HSpec.it "strict ByteString to String" $ do
          getResults' Examples.strictBytestring2String
            `isSupersetOf`
              fns
                [ "bytestring-0.11.4.0:Data.ByteString.Char8.unpack"
                ]
        HSpec.it "lazy Text to strict ByteString" $ do
          getResults' Examples.lazyText2StrictBytestring
            `isSupersetOf`
              fns
                [ "bytestring-0.11.4.0:Data.ByteString.Char8.pack . text-2.0.2:Data.Text.Lazy.unpack"
                , "text-2.0.2:Data.Text.Encoding.encodeUtf16BE . text-2.0.2:Data.Text.Lazy.toStrict"
                , "text-2.0.2:Data.Text.Encoding.encodeUtf32LE . text-2.0.2:Data.Text.Lazy.toStrict"
                , "text-2.0.2:Data.Text.Encoding.encodeUtf8 . text-2.0.2:Data.Text.Lazy.toStrict"
                , "bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf16LE"
                , "bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf32BE"
                , "bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf8"
                ]
  where
    maxCount = 11
    maxCountSpTrees = maxCount

    fns lst = map (PPFunctions . NE.toList . fn) lst

    fn bs = fromMaybe
      (error $ "parseComposedFunctions: bad input: " <> BSC8.unpack bs)
      (MyLib.parseComposedFunctions bs)

    getResults graphData (src, dst) =
      let res = MyLib.runQueryAll maxCountSpTrees (src, dst) graphData
          !first = take maxCount res
      in first

newtype PPFunctions = PPFunctions { unPPFunctions :: [MyLib.Function ()] }
  deriving (Eq, Ord)

instance Show PPFunctions where
  show = MyLib.renderComposedFunctionsStr . unPPFunctions

isSupersetOf :: (Show a, Ord a) => [a] -> [a] -> IO ()
isSupersetOf superSetLst subSetLst =
  let superSet = Set.fromList superSetLst
      subSet = Set.fromList subSetLst
  in Set.intersection superSet subSet `shouldBe` subSet
