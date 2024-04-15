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
  , parseIdentifier, parseFunction
  , fqtPackage
  , textToFullyQualifiedType
  , fullyQualifiedTypeToText
  , declarationMapJsonToFunctions
  ) where

import qualified Json
import qualified Data.Graph.Digraph as DG
import Data.Hashable (Hashable, hashWithSalt)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Functor ((<&>))
import Data.String (IsString)
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Control.DeepSeq (NFData)
import qualified Types
import Data.Maybe (fromMaybe)

-- | A function that takes a single non-function argument and returns a non-function value.
--
-- Generic over type signature.
data Function typeSig = Function
  { _function_name :: T.Text -- ^ e.g. "pack"
  , _function_module :: T.Text -- ^ e.g. "Data.Text"
  , _function_package :: Types.FgPackage T.Text -- ^ e.g. @Types.FgPackage "text" "1.2.4.1"@
  , _function_typeSig :: typeSig
  } deriving (Eq, Show, Ord, Generic)

instance NFData a => NFData (Function a)

instance Functor Function where
  fmap f fn = fn { _function_typeSig = f $ _function_typeSig fn }

-- The 'packageNoVersion' function applied to the '_function_package' field.
functionPackageNoVersion
  :: Show typeSig
  => Function typeSig
  -> T.Text
functionPackageNoVersion =
  Types.fgPackageName . _function_package

-- | Render composed functions.
--
-- Function list is in order of application, e.g. @g . f@ is @[f, g]@.
renderComposedFunctions :: [Function typeSig] -> T.Text
renderComposedFunctions fnLst =
  let dispFunLine = T.concat . intersperse " . " . map renderFunction . reverse
  in dispFunLine fnLst

-- | Same as 'renderComposedFunctions' but returns a 'String'
renderComposedFunctionsStr :: [Function typeSig] -> String
renderComposedFunctionsStr =
  T.unpack . renderComposedFunctions

-- | Parse the output of 'renderComposedFunctions'
parseComposedFunctions :: T.Text -> Either String (NE.NonEmpty UntypedFunction)
parseComposedFunctions txt = do
  functionList <- maybe (Left "parseComposedFunctions: zero functions") Right $ NE.nonEmpty $ reverse $ T.splitOn " . " txt
  let parsedMaybeFunctions = NE.map parseFunction functionList
  sequence parsedMaybeFunctions


-- | Produce e.g. "text-2.0.2:Data.Text.Encoding.encodeUtf16BE" from an untyped 'Function'
renderFunction :: Function typeSig -> T.Text
renderFunction fn =
  Types.renderFgPackage (_function_package fn) <> ":" <> _function_module fn <> "." <> _function_name fn

-- | Render a function's name (output of 'renderFunction') and its FROM and TO type.
renderTypedFunction
  :: TypedFunction
  -> (T.Text, (FullyQualifiedType, FullyQualifiedType))
  -- ^ (output of 'renderFunction', (FROM type, TO type))
renderTypedFunction fn =
  ( renderFunction fn
  , (Json.functionType_arg sig, Json.functionType_ret sig)
  )
  where
    sig = _function_typeSig fn

-- | Parse e.g. "text-2.0.2:Data.Text.Encoding.encodeUtf16BE" to an untyped 'Function'
parseFunction :: T.Text -> Either String UntypedFunction
parseFunction bs = do
  (name, moduleName, pkg) <- parseIdentifier bs
  pure $ Function name moduleName pkg ()

-- | Parse e.g. "text-2.0.2:Data.Text.Encoding.encodeUtf16BE" to an untyped identifier.
--
--   TODO: only works for "simple" types, e.g. NOT types of the form @A B@, @(A, B)@ or @[A]@.
parseIdentifier
  :: T.Text
  -> Either String (T.Text, T.Text, Types.FgPackage T.Text)
  -- ^ (name, module name, package). E.g. ("encodeUtf16BE", "Data.Text.Encoding", "text-2.0.2")
parseIdentifier txt = do
  -- NOTE: 'Types.parsePprTyCon' is used because a type constructor and an identifer are of the same form (only different is lower/upper case first letter for the 'name')
  Types.FgTyCon name moduleName package <- Types.parsePprTyCon txt
  pure (name, moduleName, package)

-- | A typed 'Function'
type TypedFunction = Function (Json.FunctionType FullyQualifiedType)

-- | A untyped 'Function'
type UntypedFunction = Function ()

-- | A 'TypedFunction' with a pretty 'Show' instance
newtype PrettyTypedFunction = PrettyTypedFunction { unPrettyTypedFunction :: TypedFunction }
  deriving (Eq, Ord)

-- TODO: inlined here. use from 'dump-decls'.
renderFgPackage
  :: (Data.String.IsString text, Semigroup text)
  => Types.FgPackage text
  -> text
renderFgPackage p =
  Types.fgPackageName p <> "-" <> Types.fgPackageVersion p

instance Show PrettyTypedFunction where
  show = prettyFunction . unPrettyTypedFunction
    where
      prettyFunction fun =
        T.unpack $ T.concat $
          [ renderFgPackage $ _function_package fun
          , ":"
          , _function_module fun
          , "."
          , _function_name fun
          ] ++
            let sig = _function_typeSig fun
                arg = unFullyQualifiedType $ Json.functionType_arg sig
                ret = unFullyQualifiedType $ Json.functionType_ret sig
            in [" :: ", Types.renderFgTypeFgTyConQualified arg, " -> ", Types.renderFgTypeFgTyConQualified ret]

-- TODO: temporary wrapper
newtype FullyQualifiedType = FullyQualifiedType
  { unFullyQualifiedType :: Types.FgType (Types.FgTyCon T.Text) }
  deriving (Eq, Ord, Show, Generic, NFData)

-- | TODO: What's the meaning of this? What should it return for e.g. @base-4.18.0.0:Data.Either.Either text-2.0.2:Data.Text.Internal.Text base-4.18.0.0:GHC.Base.String@
fqtPackage :: FullyQualifiedType -> BS.ByteString
fqtPackage = error "TODO"
  -- BS.takeWhile (/= toEnum (fromEnum ':')) . unFullyQualifiedType

-- TODO
textToFullyQualifiedType
  :: T.Text
  -> FullyQualifiedType
textToFullyQualifiedType = error "TODO"
  -- FullyQualifiedType . fmap ((`FgType_TyConApp` []) . fmap TE.encodeUtf8) . Types.parsePprTyCon

fullyQualifiedTypeToText
  :: FullyQualifiedType
  -> T.Text
fullyQualifiedTypeToText =
  Types.renderFgTypeFgTyConQualified . unFullyQualifiedType

instance Hashable FullyQualifiedType

-- WIP
instance Hashable (Types.FgType (Types.FgTyCon T.Text)) where
  hashWithSalt = error "TODO"

declarationMapJsonToFunctions
  :: Json.DeclarationMapJson T.Text
  -> [TypedFunction]
declarationMapJsonToFunctions dmj = concat $
  Map.toList moduleDeclarations <&> \(moduleName, nameMap) ->
    Map.toList nameMap <&> \(functionName, typeInfo) ->
      Function functionName moduleName package (FullyQualifiedType <$> typeInfoToFunctionType typeInfo)
  where
    typeInfoToFunctionType ti = fromMaybe (Json.typeInfo_unexpanded ti) (Json.typeInfo_expanded ti)
    moduleDeclarations = Json.moduleDeclarations_map (Json.declarationMapJson_moduleDeclarations dmj)
    package = Json.declarationMapJson_package dmj

instance DG.DirectedEdge TypedFunction FullyQualifiedType TypedFunction where
  fromNode = Json.functionType_arg . _function_typeSig
  toNode = Json.functionType_ret . _function_typeSig
  metaData = id
