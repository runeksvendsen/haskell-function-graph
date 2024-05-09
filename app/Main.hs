{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified FunGraph
import qualified FunGraph.Util
import qualified System.Environment as Env
import qualified Control.Monad.ST as ST
import qualified Data.Graph.Digraph as DG
import qualified System.Timeout
import Control.Monad (when, forM_)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Set as Set
import qualified System.Console.ANSI as ANSI
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified FunGraph.Types as Types

buildConfig :: FunGraph.BuildConfig
buildConfig = FunGraph.emptyBuildConfig
  { FunGraph.buildConfig_excludeTypes = Set.fromList $ map Types.parsePprTyConSingleton
      [ "[ghc-prim-0.10.0:GHC.Types.Char]"
      , "[[ghc-prim-0.10.0:GHC.Types.Char]]"
      , "base-4.18.0.0:GHC.Real.Ratio ghc-bignum-1.3:GHC.Num.Integer.Integer"
      , "scientific-0.3.7.0:Data.Scientific.Scientific"
      , "bytestring-0.11.4.0:Data.ByteString.Builder.Internal.Builder"
      , "[base-4.18.0.0:GHC.Word.Word8]"
      , "[ghc-prim-0.10.0:GHC.Types.Bool]"
      , "base-4.18.0.0:GHC.Maybe.Maybe ghc-prim-0.10.0:GHC.Types.Bool"
      ]
  , FunGraph.buildConfig_excludeTypesUnqualified = Set.fromList
      [ "String"
      , "Char"
      , "FastString"
      , "ShortByteString"
      , "Bool"
      , "Bool#"
      , "Int"
      , "Int32"
      , "Int64"
      , "Word"
      , "Word8"
      , "Word16"
      , "Word32"
      , "Word64"
      , "Float"
      , "Double"
      , "Integer"
      , "ByteString"
      , "Text"
      , "ByteArray#"
      ]
  , FunGraph.buildConfig_excludeModulePatterns = Set.fromList
      [ "Foreign.C.Types" -- TODO: verify that this works (it seems to not work)
      , "Codec.CBOR"
      ]
  }

main :: IO ()
main = do
  [fileName, timoutMillisStr] <- Env.getArgs
  timoutMillis <- maybe (fail $ "Invalid timeout: " <> show timoutMillisStr) pure (readMaybe timoutMillisStr)
  FunGraph.withGraphFromFile (FunGraph.defaultBuildConfig <> buildConfig) fileName $ \graph -> do
    vertices <- ST.stToIO $ DG.vertexLabels graph
    forM_ vertices $ \src ->
      forM_ vertices $ \dst -> do
        res <- query timoutMillis graph src dst
        case res of
          [] -> pure ()
          (fns,_):_ -> when (length fns > 9) $ do
            putStrLn . ((show (length fns) <> " ") <>) $
              let fns' = map NE.head fns
              in mconcat
                [ color ANSI.Green $
                    T.unpack $
                      FunGraph.renderFullyQualifiedType src <> " -> " <> FunGraph.renderFullyQualifiedType dst
                , " : "
                , color ANSI.Red $ renderTypeSig fns'
                , " : "
                , color ANSI.Blue $ FunGraph.renderComposedFunctionsStr fns'
                ]
  where
    maxCount = 1

    renderTypeSig = T.unpack . FunGraph.Util.showTypeSig . FunGraph.Util.typedFunctionsPathTypes

    query timoutMillis graph src dst = fmap (fromMaybe []) $
      System.Timeout.timeout (timoutMillis * 1000) $
        ST.stToIO $
          fmap (fromMaybe $ error "no results") $
            FunGraph.runGraphAction graph $ FunGraph.queryTreeGA maxCount (src, dst)

    color :: ANSI.Color -> String -> String
    color color' str = concat
        [ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull color']
        , str
        , ANSI.setSGRCode [ANSI.Reset]
        ]
