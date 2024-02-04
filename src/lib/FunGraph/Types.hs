{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FunGraph.Types
  ( Function(..)
  , TypedFunction
  , UntypedFunction
  , PrettyTypedFunction(..)
  , FullyQualifiedType(..)
  , functionPackageNoVersion
  , renderComposedFunctions
  , renderComposedFunctionsStr
  , parseComposedFunctions
  , renderFunction
  , renderTypedFunction
  , parseFunction
  , fqtPackage
  , textToFullyQualifiedType
  , fullyQualifiedTypeToText
  , declarationMapJsonToFunctions
  ) where

import qualified Json
import qualified Data.Graph.Digraph as DG
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Search as Search
import qualified Data.Map as Map
import Data.Functor ((<&>))
import qualified Codec.Binary.UTF8.String as UTF8
import Data.String (IsString)
import Control.Monad (guard)
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Control.DeepSeq (NFData)

-- | A function that takes a single non-function argument and returns a non-function value.
--
-- Generic over type signature.
data Function typeSig = Function
  { _function_name :: BS.ByteString -- ^ e.g. "pack"
  , _function_module :: BS.ByteString -- ^ e.g. "Data.Text"
  , _function_package :: BS.ByteString -- ^ e.g. "text-1.2.4.1"
  , _function_typeSig :: typeSig
  } deriving (Eq, Show, Ord, Generic)

instance NFData a => NFData (Function a)

instance Functor Function where
  fmap f fn = fn { _function_typeSig = f $ _function_typeSig fn }

functionPackageNoVersion
  :: Show typeSig
  => Function typeSig
  -> BS.ByteString
functionPackageNoVersion fn =
  case Search.split "-" (_function_package fn) of
    lst | length lst > 1 ->
      BS.concat $ intersperse "-" (init lst)
    _ ->
      error $ "Invalid package name for Function: " <> show fn

-- | Render composed functions.
--
-- Function list is in order of application, e.g. @g . f@ is @[f, g]@.
renderComposedFunctions :: [Function typeSig] -> BS.ByteString
renderComposedFunctions fnLst =
  let dispFunLine = BS.concat . intersperse " . " . map renderFunction . reverse
  in dispFunLine fnLst

-- | Same as 'renderComposedFunctions' but returns a 'String'
renderComposedFunctionsStr :: [Function typeSig] -> String
renderComposedFunctionsStr =
  bsToStr . renderComposedFunctions

-- | Parse the output of 'renderComposedFunctions'
parseComposedFunctions :: BS.ByteString -> Maybe (NE.NonEmpty UntypedFunction)
parseComposedFunctions bs = do
  bsFunctionList <- NE.nonEmpty $ reverse $ Search.split " . " bs
  let parsedMaybeFunctions = NE.map parseFunction bsFunctionList
  sequence parsedMaybeFunctions

-- | Produce e.g. "text-2.0.2:Data.Text.Encoding.encodeUtf16BE" from an untyped 'Function'
renderFunction :: Function typeSig -> BS.ByteString
renderFunction fn =
  _function_package fn <> ":" <> _function_module fn <> "." <> _function_name fn

-- | Render a function's name (output of 'renderFunction') and its FROM and TO type.
renderTypedFunction
  :: TypedFunction
  -> (BS.ByteString, (FullyQualifiedType, FullyQualifiedType))
  -- ^ (output of 'renderFunction', (FROM type, TO type))
renderTypedFunction fn =
  ( renderFunction fn
  , (Json.functionType_arg sig, Json.functionType_ret sig)
  )
  where
    sig = _function_typeSig fn

-- | Parse e.g. "text-2.0.2:Data.Text.Encoding.encodeUtf16BE" to an untyped 'Function'
parseFunction :: BS.ByteString -> Maybe UntypedFunction
parseFunction bs =
  case BSC8.split ':' bs of
    [pkg, moduleAndName] -> do
      (moduleNameWithSuffix, name) <- spanEndNonEmpty (not . (== '.')) moduleAndName
      moduleName <- BS.stripSuffix "." moduleNameWithSuffix
      guard $ not (BS.null moduleName)
      pure $
        Function
          { _function_name = name
          , _function_module = moduleName
          , _function_package = pkg
          , _function_typeSig = ()
          }
    _ -> Nothing
  where
    -- A version of 'spanEnd' that does not return the empty ByteString
    spanEndNonEmpty f bs' =
      let (a, b) = BSC8.spanEnd f bs'
          result
            | BS.null a || BS.null b = Nothing
            | otherwise = Just (a, b)
      in result

-- | A typed 'Function'
type TypedFunction = Function (Json.FunctionType FullyQualifiedType)

-- | A untyped 'Function'
type UntypedFunction = Function ()

-- | A 'TypedFunction' with a pretty 'Show' instance
newtype PrettyTypedFunction = PrettyTypedFunction { unPrettyTypedFunction :: TypedFunction }
  deriving (Eq, Ord)

instance Show PrettyTypedFunction where
  show = prettyFunction . unPrettyTypedFunction
    where
      prettyFunction fun =
        bsToStr $ BS.concat $
          [_function_package fun
          , ":"
          , _function_module fun
          , "."
          , _function_name fun
          ] ++
            let sig = _function_typeSig fun
                arg = unFullyQualifiedType $ Json.functionType_arg sig
                ret = unFullyQualifiedType $ Json.functionType_ret sig
            in [" :: ", arg, " -> ", ret]

-- | E.g. "base-4.18.0.0:GHC.Ptr.Ptr zstd-0.1.3.0:Codec.Compression.Zstd.FFI.Types.DDict".
--   Guaranteed to not be a function type (ie. will not contain any function arrows).
newtype FullyQualifiedType = FullyQualifiedType { unFullyQualifiedType :: BS.ByteString }
  deriving (Eq, Ord, Show, Generic, IsString, NFData)

fqtPackage :: FullyQualifiedType -> BS.ByteString
fqtPackage = BS.takeWhile (/= toEnum (fromEnum ':')) . unFullyQualifiedType

textToFullyQualifiedType
  :: T.Text
  -> FullyQualifiedType
textToFullyQualifiedType =
  FullyQualifiedType . TE.encodeUtf8

fullyQualifiedTypeToText
  :: FullyQualifiedType
  -> T.Text
fullyQualifiedTypeToText =
  TE.decodeUtf8 . unFullyQualifiedType

instance Hashable FullyQualifiedType

declarationMapJsonToFunctions
  :: Json.DeclarationMapJson String
  -> [TypedFunction]
declarationMapJsonToFunctions dmj = concat $
  Map.toList moduleDeclarations <&> \(moduleName, nameMap) ->
    Map.toList nameMap <&> \(functionName, functionType) ->
      Function functionName moduleName package (FullyQualifiedType <$> functionType)
  where
    moduleDeclarations = Json.moduleDeclarations_map (Json.declarationMapJson_moduleDeclarations dmj')
    dmj' = Json.fmapDeclarationMapJson (BS.pack . UTF8.encode) dmj
    package = Json.declarationMapJson_package dmj'

instance DG.DirectedEdge TypedFunction FullyQualifiedType TypedFunction where
  fromNode = Json.functionType_arg . _function_typeSig
  toNode = Json.functionType_ret . _function_typeSig
  metaData = id

bsToStr :: BSC8.ByteString -> String
bsToStr = UTF8.decode . BS.unpack
