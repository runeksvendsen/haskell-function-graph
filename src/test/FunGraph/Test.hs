{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use fmap" #-}
module FunGraph.Test
( -- * Test cases
  allTestCases
, QueryTest(..), Args
  -- * Helper types
, PPFunctions(..)
, QueryResults
  -- * Query functions
, mkQueryFunctions
)
where

import qualified FunGraph
import FunGraph.Examples

import Data.Functor (void)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Control.Monad.Trans.Except as Except
import qualified Streaming.Prelude as S
import qualified Control.Monad.ST as ST
import qualified Data.Time
import qualified Streaming as S

data QueryTest = QueryTest
    { queryTest_name :: String
    , queryTest_args :: Args
    , queryTest_expectedResult :: Set.Set PPFunctions
    }

type Args = (Int, (FunGraph.FullyQualifiedType, Maybe FunGraph.FullyQualifiedType)) -- ^ (maxCount, (src, dst))

-- | Test 'FunGraph.queryTreeAndPathsGA'
queryTreeAndPathsGAListTest
  :: Args
  -> FunGraph.GraphAction s FunGraph.FullyQualifiedType (NE.NonEmpty FunGraph.TypedFunction) [(PPFunctions, Double)]
queryTreeAndPathsGAListTest args =
  mapQueryResult . snd <$> uncurry FunGraph.queryTreeAndPathsGA args
  where
    mapQueryResult = map (first $ PPFunctions . map void)

mkTestCase
  :: Int
  -> ((FunGraph.FullyQualifiedType, String), (FunGraph.FullyQualifiedType, String))
  -> [T.Text]
  -> QueryTest
mkTestCase maxCount (from, to) expectedList =
    QueryTest
        { queryTest_name = unwords [snd from, "to", snd to]
        , queryTest_args = (maxCount, (fst from, Just $ fst to))
        , queryTest_expectedResult = Set.fromList $ fns expectedList
        }
  where
    fns :: [T.Text] -> [PPFunctions]
    fns = map (PPFunctions . NE.toList . fn)

    fn :: T.Text -> NE.NonEmpty FunGraph.UntypedFunction
    fn bs = either
      (error $ "parseComposedFunctions: bad input: " <> T.unpack bs)
      id
      (FunGraph.parseComposedFunctions bs)

-- | Test 'FunGraph.queryTreeTimeoutIO'
queryTreeAndPathsGAStreamTest
  :: ( v ~ FunGraph.FullyQualifiedType
     )
  => Data.Time.NominalDiffTime -- ^ timeout
  -> Args
  -> FunGraph.Digraph ST.RealWorld FunGraph.FullyQualifiedType (NE.NonEmpty FunGraph.TypedFunction)
  -> IO (Either (FunGraph.GraphActionError v) [(PPFunctions, Double)])
queryTreeAndPathsGAStreamTest timeout (maxCount, srcDst) graph =
  Except.runExceptT $
    S.lift . S.toList_ =<< streamExcept
  where
    streamExcept =
      S.concat . S.map toPPFunctions <$> queryTreeAndPathsGAStream

    queryTreeAndPathsGAStream =
      S.map (\tree -> (tree, FunGraph.queryResultTreeToPaths maxCount srcDst [tree]))
        <$> FunGraph.queryTreeTimeoutIO graph timeout maxCount srcDst

    toPPFunctions
      :: (([NE.NonEmpty FunGraph.TypedFunction], Double), [([FunGraph.TypedFunction], Double)])
      -> [(PPFunctions, Double)]
    toPPFunctions = map (first $ PPFunctions . map void) . snd

type QueryResults =
  Either (FunGraph.GraphActionError FunGraph.FullyQualifiedType) [(FunGraph.Test.PPFunctions, Double)]

-- | The query functions we want to test
mkQueryFunctions
  :: Bool -- ^ enable tracing?
  -> NE.NonEmpty (String, Args -> FunGraph.Graph ST.RealWorld -> IO QueryResults)
mkQueryFunctions shouldTrace = NE.fromList
  [ ("Stream", FunGraph.Test.queryTreeAndPathsGAStreamTest 1000)
  , ("List", queryTestList)
  ]
  where
    queryTestList
      :: FunGraph.Test.Args
      -> FunGraph.Graph ST.RealWorld
      -> IO QueryResults
    queryTestList args graph =
      let runQueryFunction =
            if shouldTrace then FunGraph.runGraphActionTrace else FunGraph.runGraphAction
      in ST.stToIO $ runQueryFunction graph $ FunGraph.Test.queryTreeAndPathsGAListTest args

allTestCases :: [QueryTest]
allTestCases =
  [ case1
  , case2
  , case3
  , case4
  , case5
  , case6
  , case7
  , case8
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

-- NB: There exists no path from src to dst, but this query is _really_ slow for the web server (45-50 seconds) with maxCount=100
case5 :: QueryTest
case5 =
  mkTestCase 1
    ( ( stringBuffer
      , T.unpack $ FunGraph.renderFullyQualifiedType stringBuffer
      )
    , ( stringTemplate
      , T.unpack $ FunGraph.renderFullyQualifiedType stringTemplate
      )
    )
  []
  where
    stringBuffer = FunGraph.parsePprTyConSingleton "ghc-9.6.2:GHC.Data.StringBuffer.StringBuffer"

    stringTemplate = FunGraph.parsePprTyConMulti $
      FunGraph.FgType_TyConApp
        "HStringTemplate-0.8.8:Text.StringTemplate.Base.StringTemplate"
        [FunGraph.FgType_List $ FunGraph.FgType_TyConApp "ghc-prim-0.10.0:GHC.Types.Char" []]

-- Also a slow web query (~20s) with maxCount=100
case6 :: QueryTest
case6 =
  mkTestCase 1
    ( ( FunGraph.parsePprTyConSingleton "network-3.1.4.0:Network.Socket.Types.Socket"
      , "Socket"
      )
    , ( FunGraph.parsePprTyConSingleton "network-3.1.4.0:Network.Socket.Types.SockAddr"
      , "SockAddr"
      )
    )
  []

case7 :: QueryTest
case7 =
  mkTestCase 8
    (strictByteString, strictText)
    [ "text-2.0.2:Data.Text.Encoding.decodeASCII"
    , "text-2.0.2:Data.Text.Encoding.decodeLatin1"
    , "text-2.0.2:Data.Text.Encoding.decodeUtf16BE"
    , "text-2.0.2:Data.Text.Encoding.decodeUtf16LE"
    , "text-2.0.2:Data.Text.Encoding.decodeUtf32BE"
    , "text-2.0.2:Data.Text.Encoding.decodeUtf32LE"
    , "text-2.0.2:Data.Text.Encoding.decodeUtf8"
    , "text-2.0.2:Data.Text.Encoding.decodeUtf8Lenient"
    ]

case8 :: QueryTest
case8 =
  mkTestCase 5
    (strictText, strictByteString)
    [ "text-2.0.2:Data.Text.Encoding.encodeUtf16BE"
    , "text-2.0.2:Data.Text.Encoding.encodeUtf16LE"
    , "text-2.0.2:Data.Text.Encoding.encodeUtf32BE"
    , "text-2.0.2:Data.Text.Encoding.encodeUtf32LE"
    , "text-2.0.2:Data.Text.Encoding.encodeUtf8"
    ]

newtype PPFunctions = PPFunctions { unPPFunctions :: [FunGraph.Function ()] }
  deriving (Eq, Ord, Generic)

instance NFData PPFunctions

instance Show PPFunctions where
  show = FunGraph.renderComposedFunctionsStr . unPPFunctions
