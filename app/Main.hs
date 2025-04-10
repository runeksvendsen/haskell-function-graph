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
import Control.Monad (when)
import qualified Data.Text.IO as TIO
import Data.Bifunctor (second)
import Data.Containers.ListUtils (nubOrdOn)
import Debug.Trace (trace)
import qualified Types.FunctionInfo as FunctionInfo

main :: IO ()
main = do
  [fileName] <- Args.getArgs
  graphData <- either fail pure =<< FunGraph.Build.fileReadDeclarationMap fileName
  tmpPrintForalls graphData

tmpPrintExtendedFunctions
  :: [Json.DeclarationMapJson T.Text]
  -> IO ()
tmpPrintExtendedFunctions declarationMapJsonLst =
  traceIt
    (Types.Doodle.extendFrom monoFuns (filter (not . isForallAToSomething . snd) polyFuns))
  where

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

    renderFunctionType
      :: Types.FunctionType (FgType (Types.FgTyCon T.Text))
      -> T.Text
    renderFunctionType ft =
      T.unwords
        [ Types.renderFgTypeFgTyConQualifiedNoPackage $ Types.functionType_arg ft
        , "->"
        , Types.renderFgTypeFgTyConQualifiedNoPackage $ Types.functionType_ret ft
        ]

tmpPrintForalls
  :: [Json.DeclarationMapJson T.Text]
  -> IO ()
tmpPrintForalls declarationMapJsonLst =
  traceIt polyFuns
  where
    someFunctions = concatMap extract declarationMapJsonLst

    polyFuns :: [(T.Text, Types.Doodle.FunctionTypeForall T.Text T.Text)]
    polyFuns =
      Data.Maybe.mapMaybe (traverse Types.Doodle.someFunctionPolymorphic) someFunctions

    traceIt
      :: [(T.Text, Types.Doodle.FunctionTypeForall T.Text T.Text)]
      -> IO ()
    traceIt ftl = do
      putStrLn ""
      forM_ ftl $ \(namePlusType, ftf) -> do
        when (isForallAToSomething ftf) $
          TIO.putStrLn $ namePlusType

extract
  :: Json.DeclarationMapJson T.Text
  -> [(T.Text, Types.Doodle.SomeFunction)] -- (fully qualified name, type). e.g. ("Data.List.head", "[a] -> a")
extract dmj =
  let pkg = fgPackageName $ Json.declarationMapJson_package dmj
  in concatMap
      (\(moduleName, nameToTypeMap) ->
        let assocs = map (fmap FunctionInfo.functionInfo_function) $
              nubOrdOn (FunctionInfo.functionInfo_unique . snd) $ Map.assocs nameToTypeMap
            mkFunMetaData (funName, funType) = T.pack $ unlines
              [ color ANSI.Red $ T.unpack $ "/" <> pkg <> ":" <> moduleName <> "." <> funName
              , "\t" <> " :: " <> color ANSI.Blue (T.unpack $ Types.Doodle.renderSomeFunctionType funType)
              ]
        in map (\(funName, funType) -> (mkFunMetaData (funName, funType), funType)) assocs
      )
  . Map.assocs
  . Json.moduleDeclarations_map
  . Json.declarationMapJson_moduleDeclarations
  $ dmj


color :: ANSI.Color -> String -> String
color color' str = concat
  [ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull color']
  , str
  , ANSI.setSGRCode [ANSI.Reset]
  ]

-- | Functions of type @a -> Something@, ie. taking all types as argument and returning whatever.
--
--  Problematic because they double the number of nodes in the graph.
isForallAToSomething
  :: Types.Doodle.FunctionTypeForall T.Text T.Text
  -> Bool
isForallAToSomething ftf =
  case Types.Doodle.ftf_arg ftf of
    FgType_TyConApp (Right _) [] -> True
    _ -> False
