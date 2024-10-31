{-# LANGUAGE OverloadedStrings #-}

module FunGraph.Examples where

import FunGraph

all :: [(FullyQualifiedType, String)]
all =
  [ string
  , strictByteString
  , strictText
  , lazyText
  ]

strictByteString :: (FullyQualifiedType, String)
strictByteString =
  (parsePprTyConSingleton "bytestring-0.11.4.0:Data.ByteString.Internal.Type.ByteString", "strict ByteString")

strictText :: (FullyQualifiedType, String)
strictText =
  (parsePprTyConSingleton "text-2.0.2:Data.Text.Internal.Text", "strict Text")

lazyText :: (FullyQualifiedType, String)
lazyText =
  (parsePprTyConSingleton "text-2.0.2:Data.Text.Internal.Lazy.Text", "lazy Text")

string :: (FullyQualifiedType, String)
string =
  (str, "String")
  where
    str = FunGraph.parsePprTyConMulti $
      FunGraph.FgType_List $
        FunGraph.FgType_TyConApp "ghc-prim-0.10.0:GHC.Types.Char" []
