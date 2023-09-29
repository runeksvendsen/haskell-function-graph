{-# LANGUAGE OverloadedStrings #-}

module MyLib.Examples where

import MyLib

strictByteString :: FullyQualifiedType
strictByteString = FullyQualifiedType
  "bytestring-0.11.4.0:Data.ByteString.Internal.Type.ByteString"

lazyText :: FullyQualifiedType
lazyText = FullyQualifiedType
  "text-2.0.2:Data.Text.Internal.Lazy.Text"

string :: FullyQualifiedType
string = "[ghc-prim-0.10.0:GHC.Types.Char]"

simpleQuery :: (FullyQualifiedType, FullyQualifiedType) -- ^ (src, dst)
simpleQuery =
  ( strictByteString
  , string
  )

simpleQuery2 :: (FullyQualifiedType, FullyQualifiedType) -- ^ (src, dst)
simpleQuery2 =
  ( lazyText
  , strictByteString
  )
