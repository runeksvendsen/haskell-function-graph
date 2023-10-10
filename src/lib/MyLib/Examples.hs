{-# LANGUAGE OverloadedStrings #-}

module MyLib.Examples where

import MyLib

all :: [(FullyQualifiedType, String)]
all =
  [ string
  , strictByteString
  , lazyText
  ]

strictByteString :: (FullyQualifiedType, String)
strictByteString =
  ("bytestring-0.11.4.0:Data.ByteString.Internal.Type.ByteString", "strict ByteString")

lazyText :: (FullyQualifiedType, String)
lazyText =
  ("text-2.0.2:Data.Text.Internal.Lazy.Text", "lazy Text")

string :: (FullyQualifiedType, String)
string =
  ("[ghc-prim-0.10.0:GHC.Types.Char]", "String")
