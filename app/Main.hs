{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Main (main) where

import FunGraph.Types
import qualified Json
import qualified Types
import qualified Data.Text as T
import qualified Types.Doodle
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Foldable (forM_)
import qualified System.Console.ANSI as ANSI
import qualified System.Environment as Args
import qualified FunGraph.Build

main :: IO ()
main = do
  [fileName] <- Args.getArgs
  graphData <- either fail pure =<< FunGraph.Build.fileReadDeclarationMap fileName
  tmpPrintExtendedFunctions graphData

tmpPrintExtendedFunctions
  :: [Json.DeclarationMapJson T.Text]
  -> IO ()
tmpPrintExtendedFunctions declarationMapJsonLst =
  traceIt
    (Types.Doodle.extendFrom monoFuns polyFuns)
  where
    extract
      :: Json.DeclarationMapJson T.Text
      -> [(T.Text, Types.Doodle.SomeFunction)] -- (fully qualified name, type). e.g. ("Data.List.head", "[a] -> a")
    extract =
        concatMap
          (\(moduleName, nameToTypeMap) ->
            let assocs = Map.assocs nameToTypeMap
                funMetaData (funName, funType) = T.pack $ unlines
                  [ ("\t" <>) $ color ANSI.Red $ T.unpack $ moduleName <> "." <> funName
                  , "\t\t" <> " :: " <> color ANSI.Blue (T.unpack $ Types.Doodle.renderSomeFunctionType funType)
                  ]
            in map (\(funName, funType) -> (funMetaData (funName, funType), funType)) assocs
          )
      . Map.assocs
      . Json.moduleDeclarations_map
      . Json.declarationMapJson_moduleDeclarations

    someFunctions = concatMap extract declarationMapJsonLst

    monoFuns :: [(T.Text, Types.FunctionType (FgType (Types.FgTyCon T.Text)))]
    monoFuns =
      Data.Maybe.mapMaybe (traverse Types.Doodle.someFunctionMonomorphic) someFunctions

    polyFuns :: [(T.Text, Types.Doodle.FunctionTypeForall T.Text T.Text)]
    polyFuns =
      Data.Maybe.mapMaybe (traverse Types.Doodle.someFunctionPolymorphic) someFunctions

    traceIt
      :: [((T.Text, T.Text), Either String (Types.FunctionType (FgType (Types.FgTyCon T.Text))))]
      -> IO ()
    traceIt ftl = do
      putStrLn ""
      forM_ ftl $ \blah -> putStrLn $ mkTraceString blah

    mkTraceString ((monoName, polyName), eTypedFunction) =
      let resultType =
            either
              ("ERROR: " <>)
              (\ft -> ":: " <> color ANSI.Blue (T.unpack (renderFunctionType ft)))
              eTypedFunction
      in
      unlines
        [ "Extending from"
        , T.unpack monoName
        , "\t" <> "with"
        , T.unpack polyName
        , "\t" <> "resulting in"
        , "\t\t" <> resultType
        , ""
        ]

    color :: ANSI.Color -> String -> String
    color color' str = concat
      [ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull color']
      , str
      , ANSI.setSGRCode [ANSI.Reset]
      ]

    renderFunctionType
      :: Types.FunctionType (FgType (Types.FgTyCon T.Text))
      -> T.Text
    renderFunctionType ft =
      T.unwords
        [ Types.renderFgTypeFgTyConQualifiedNoPackage $ Types.functionType_arg ft
        , "->"
        , Types.renderFgTypeFgTyConQualifiedNoPackage $ Types.functionType_ret ft
        ]
