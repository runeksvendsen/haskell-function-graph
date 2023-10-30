{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified MyLib
import MyLib.Examples
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
main = MyLib.withGraphFromFile testDataFileName $ \graph -> do
  let getResults' maxCount = map (PPFunctions . map void) . getResults maxCount graph
      testCase maxCount (from, to) expectedList =
        HSpec.it (unwords [snd from, "to", snd to]) $ do
          getResults' maxCount (fst from, fst to)
            `isSupersetOf`
              fns expectedList
  HSpec.hspec $
    HSpec.describe "Unit tests" $ do
      HSpec.describe ("Expected result contained in top query results") $ do
        testCase 1
          (strictByteString, string)
            ["bytestring-0.11.4.0:Data.ByteString.Char8.unpack"]
        testCase 1
          (string, strictByteString)
          ["bytestring-0.11.4.0:Data.ByteString.Char8.pack"]
        testCase 30
          (lazyText, strictByteString)
          [ "bytestring-0.11.4.0:Data.ByteString.Char8.pack . text-2.0.2:Data.Text.Lazy.unpack"
          , "text-2.0.2:Data.Text.Encoding.encodeUtf16BE . text-2.0.2:Data.Text.Lazy.toStrict"
          , "text-2.0.2:Data.Text.Encoding.encodeUtf32LE . text-2.0.2:Data.Text.Lazy.toStrict"
          , "text-2.0.2:Data.Text.Encoding.encodeUtf8 . text-2.0.2:Data.Text.Lazy.toStrict"
          , "bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf16LE"
          , "bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf32BE"
          , "bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf8"
          ]
        testCase 61
          (strictByteString, lazyText)
          [ "text-2.0.2:Data.Text.Lazy.pack . bytestring-0.11.4.0:Data.ByteString.Char8.unpack"
          , "text-2.0.2:Data.Text.Lazy.fromStrict . text-2.0.2:Data.Text.Encoding.decodeASCII"
          , "text-2.0.2:Data.Text.Lazy.fromStrict . text-2.0.2:Data.Text.Encoding.decodeLatin1"
          , "text-2.0.2:Data.Text.Lazy.fromStrict . text-2.0.2:Data.Text.Encoding.decodeUtf16BE"
          , "text-2.0.2:Data.Text.Lazy.fromStrict . text-2.0.2:Data.Text.Encoding.decodeUtf16LE"
          , "text-2.0.2:Data.Text.Lazy.fromStrict . text-2.0.2:Data.Text.Encoding.decodeUtf32BE"
          , "text-2.0.2:Data.Text.Lazy.fromStrict . text-2.0.2:Data.Text.Encoding.decodeUtf32LE"
          , "text-2.0.2:Data.Text.Lazy.fromStrict . text-2.0.2:Data.Text.Encoding.decodeUtf8"
          , "text-2.0.2:Data.Text.Lazy.fromStrict . text-2.0.2:Data.Text.Encoding.decodeUtf8Lenient"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeUtf8 . bytestring-0.11.4.0:Data.ByteString.fromStrict"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeUtf8 . bytestring-0.11.4.0:Data.ByteString.Char8.fromStrict"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeUtf8 . bytestring-0.11.4.0:Data.ByteString.Lazy.fromStrict"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeUtf8 . bytestring-0.11.4.0:Data.ByteString.Lazy.Char8.fromStrict"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeASCII . bytestring-0.11.4.0:Data.ByteString.Lazy.Char8.fromStrict"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeLatin1 . bytestring-0.11.4.0:Data.ByteString.fromStrict"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeLatin1 . bytestring-0.11.4.0:Data.ByteString.Lazy.Char8.fromStrict"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeUtf16BE . bytestring-0.11.4.0:Data.ByteString.fromStrict"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeUtf16LE . bytestring-0.11.4.0:Data.ByteString.Char8.fromStrict"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeUtf32BE . bytestring-0.11.4.0:Data.ByteString.Lazy.fromStrict"
          , "text-2.0.2:Data.Text.Lazy.Encoding.decodeUtf32LE . bytestring-0.11.4.0:Data.ByteString.Lazy.Char8.fromStrict"
          ]
  where
    fns lst = map (PPFunctions . NE.toList . fn) lst

    fn bs = fromMaybe
      (error $ "parseComposedFunctions: bad input: " <> BSC8.unpack bs)
      (MyLib.parseComposedFunctions bs)

    getResults maxCount graph (src, dst) =
      take maxCount $ MyLib.runQueryAll maxCount (src, dst) graph

newtype PPFunctions = PPFunctions { unPPFunctions :: [MyLib.Function ()] }
  deriving (Eq, Ord)

instance Show PPFunctions where
  show = MyLib.renderComposedFunctionsStr . unPPFunctions

isSupersetOf :: (Show a, Ord a) => [a] -> [a] -> IO ()
isSupersetOf superSetLst subSetLst =
  let superSet = Set.fromList superSetLst
      subSet = Set.fromList subSetLst
  in Set.intersection superSet subSet `shouldBe` subSet
