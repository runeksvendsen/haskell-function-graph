{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module FunGraph.Test
( allTestCases
, case1
, case2
, case3
, case4
, QueryTest(..)
, PPFunctions(..)
)
where

import qualified FunGraph
import FunGraph.Examples

import Data.Functor (void)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Control.Monad.ST as ST
import Data.Bifunctor (first)
import qualified Data.Graph.Dijkstra as Dijkstra

data QueryTest = QueryTest
    { queryTest_name :: String
    , queryTest_runQuery
        :: forall s v meta.
           (v ~ FunGraph.FullyQualifiedType, meta ~ NE.NonEmpty FunGraph.TypedFunction)
        => (forall a. (Double -> meta -> Double) -> Double -> Dijkstra.Dijkstra s v meta a -> ST.ST s a)
        -> ST.ST s [(PPFunctions, Double)]
    , queryTest_expectedResult :: Set.Set PPFunctions
    }

mkTestCase
  :: Int
  -> ((FunGraph.FullyQualifiedType, String), (FunGraph.FullyQualifiedType, String))
  -> [BSC8.ByteString]
  -> QueryTest
mkTestCase maxCount (from, to) expectedList =
    QueryTest
        { queryTest_name = unwords [snd from, "to", snd to]
        , queryTest_runQuery = \runner ->
            mapQueryResult . take maxCount <$> FunGraph.runQueryAllST runner maxCount (fst from, fst to)
        , queryTest_expectedResult = Set.fromList $ fns expectedList
        }
  where
    mapQueryResult = map (first $ PPFunctions . map void)

    fns :: [BSC8.ByteString] -> [PPFunctions]
    fns = map (PPFunctions . NE.toList . fn)

    fn :: BSC8.ByteString -> NE.NonEmpty FunGraph.UntypedFunction
    fn bs = fromMaybe
      (error $ "parseComposedFunctions: bad input: " <> BSC8.unpack bs)
      (FunGraph.parseComposedFunctions bs)

allTestCases :: [QueryTest]
allTestCases =
  [ case1
  , case2
  , case3
  , case4
  ]

case1 :: QueryTest
case1 =
  mkTestCase 1
    (strictByteString, string)
    ["bytestring-0.11.4.0:Data.ByteString.Char8.unpack"]

case2 :: QueryTest
case2 =
    mkTestCase 1
        (string, strictByteString)
        ["bytestring-0.11.4.0:Data.ByteString.Char8.pack"]

case3 :: QueryTest
case3 =
  mkTestCase 26
    (lazyText, strictByteString)
    [ "bytestring-0.11.4.0:Data.ByteString.Char8.pack . text-2.0.2:Data.Text.Lazy.unpack"
    , "text-2.0.2:Data.Text.Encoding.encodeUtf16BE . text-2.0.2:Data.Text.Lazy.toStrict"
    , "text-2.0.2:Data.Text.Encoding.encodeUtf32LE . text-2.0.2:Data.Text.Lazy.toStrict"
    , "text-2.0.2:Data.Text.Encoding.encodeUtf8 . text-2.0.2:Data.Text.Lazy.toStrict"
    , "bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf16LE"
    , "bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf32BE"
    , "bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf8"
    ]

case4 :: QueryTest
case4 =
  mkTestCase 45
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

newtype PPFunctions = PPFunctions { unPPFunctions :: [FunGraph.Function ()] }
  deriving (Eq, Ord, Generic)

instance NFData PPFunctions

instance Show PPFunctions where
  show = FunGraph.renderComposedFunctionsStr . unPPFunctions