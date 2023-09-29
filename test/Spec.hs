{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

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
  let getResults' = map (PPFunctions . map void) . getResults graphData
  HSpec.hspec $
    HSpec.describe "Unit tests" $ do
      HSpec.describe ("Expected result contained in top " <> show maxCount <> " query results") $ do
        HSpec.it "strict ByteString to String" $ do
          getResults' Examples.strictBytestring2String
            `shouldContain`
              [ PPFunctions
                [ MyLib.Function "unpack" "Data.ByteString.Char8" "bytestring-0.11.4.0" ()
                ]
              ]
        HSpec.it "lazy Text to strict ByteString" $ do
          getResults' Examples.lazyText2StrictBytestring
            `shouldContain`
              [ PPFunctions
                [ MyLib.Function "toStrict" "Data.Text.Lazy" "text-2.0.2" ()
                , MyLib.Function "encodeUtf8" "Data.Text.Encoding" "text-2.0.2" ()
                ]
              ]
            -- TODO: use 'parseFunction' to assert on all of the following:
              -- bytestring-0.11.4.0:Data.ByteString.Char8.pack . text-2.0.2:Data.Text.Lazy.unpack
              -- text-2.0.2:Data.Text.Encoding.encodeUtf16BE . text-2.0.2:Data.Text.Lazy.toStrict
              -- text-2.0.2:Data.Text.Encoding.encodeUtf32LE . text-2.0.2:Data.Text.Lazy.toStrict
              -- text-2.0.2:Data.Text.Encoding.encodeUtf8 . text-2.0.2:Data.Text.Lazy.toStrict
              -- bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf16LE
              -- bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf32BE
              -- bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf8
  where
    maxCount = 10
    maxCountSpTrees = maxCount

    getResults graphData (src, dst) =
      let res = MyLib.runQueryAll maxCountSpTrees (src, dst) graphData
          !first = take maxCount res
      in first

newtype PPFunctions = PPFunctions { unPPFunctions :: [MyLib.Function ()] }
  deriving (Eq, Ord)

instance Show PPFunctions where
  show = MyLib.renderPath . unPPFunctions
