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

strictBytestring2String :: (FullyQualifiedType, FullyQualifiedType) -- ^ (src, dst)
strictBytestring2String =
  ( strictByteString
  , string
  )

lazyText2StrictBytestring :: (FullyQualifiedType, FullyQualifiedType) -- ^ (src, dst)
lazyText2StrictBytestring =
  ( lazyText
  , strictByteString
  )
