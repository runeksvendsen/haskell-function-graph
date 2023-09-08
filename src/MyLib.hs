{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module MyLib
  ( main
  ) where

import qualified Json
import qualified Data.Graph.Digraph as DG
import qualified Data.Graph.BellmanFord as BF
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.ST (ST)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified System.Environment as Env
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Functor ((<&>))
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Control.Monad.ST as ST
import Data.String (IsString)
import Control.Monad (forM, forM_, unless)
import Debug.Trace (traceM, traceShow, traceShowId)
import Data.List (intersperse)
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.STRef as STM
import qualified Text.Show.Pretty as Pretty

queries :: [([FullyQualifiedType], [FullyQualifiedType])]
queries =
  [ ( ["[ghc-prim-0.10.0:GHC.Types.Char]"]
    , [ "bytestring-0.11.4.0:Data.ByteString.Internal.Type.ByteString"
      , "bytestring-0.11.4.0:Data.ByteString.Lazy.Internal.ByteString"
      , "text-2.0.2:Data.Text.Internal.Text"
      ]
    )
  , ( ["ghc-9.6.2:GHC.Core.TyCo.Rep.Type"]
    , ["ghc-9.6.2:GHC.Unit.Types.UnitId", "ghc-9.6.2:Language.Haskell.Syntax.Module.Name.ModuleName"]
    )
  ]

f :: BS.ByteString -> String
f = UTF8.decode . BS.unpack

simpleQuery :: (FullyQualifiedType, FullyQualifiedType)
simpleQuery =
  ( "bytestring-0.11.4.0:Data.ByteString.Internal.Type.ByteString"
  , "[ghc-prim-0.10.0:GHC.Types.Char]"
  )

main :: IO ()
main = do
  [fileName] <- Env.getArgs
  declarationMapJsonList <- either fail pure =<< A.eitherDecode <$> BSL.readFile fileName
  let buildGraph' = do
        graph <- buildGraph declarationMapJsonList
        vertexCount <- DG.vertexCount graph
        edgeCount <- DG.edgeCount graph
        traceM $ unwords ["Built graph with", show vertexCount, "vertices and", show edgeCount, "edges"]
        -- DEBUG
        let mk = FullyQualifiedType
        Just label <- DG.lookupVertex graph $ FullyQualifiedType "bytestring-0.11.4.0:Data.ByteString.Internal.Type.ByteString"
        edges <- DG.outgoingEdges graph label
        let edges' = filter ((== mk "[base-4.18.0.0:GHC.Word.Word8]") . DG.eTo) edges
        traceM $ Pretty.ppShow $ edges
        pure graph
  let _ = concat $ ST.runST $ do
        buildGraph' >>= runQueries
      res = map (map DG.eMeta) $ ST.runST $ do
        let (src, dst) = simpleQuery
            maxCount = 30
        traceM $ unwords
          [ "Finding the"
          , show maxCount
          , "first functions from"
          , UTF8.decode $ BS.unpack $ unFullyQualifiedType src
          , "to"
          , UTF8.decode $ BS.unpack $ unFullyQualifiedType dst
          ]
        buildGraph' >>= DG.freeze >>= queryAll f w src dst (\fns -> show (length fns) <> ": " <> disp fns) maxCount
  putStrLn $ unlines $ map disp res
  putStrLn $ "Got " <> show (length res) <> " results"
  where
    disp :: [Function] -> String
    disp fnLst =
      let dispFun fn =
            _function_package fn <> ":" <> _function_module fn <> "." <> _function_name fn
      in UTF8.decode $ BS.unpack $ BS.concat $ intersperse " . " $ map dispFun (reverse fnLst)

    f = (\w' _ -> w' + 1)
    w = 1

    excludeTypes = map FullyQualifiedType
      [
      ]

    isExcluded :: Function -> Bool
    isExcluded function =
      DG.fromNode function `elem` excludeTypes
        || DG.toNode function `elem` excludeTypes

    functionIdentity fn =
      ( _function_name fn
      , _function_module fn
      , _function_package fn
      )

    buildGraph
      :: [Json.DeclarationMapJson String]
      -> ST s (DG.Digraph s FullyQualifiedType Function)
    buildGraph =
      DG.fromEdges
        . nubOrdOn functionIdentity
        . filter (not . isExcluded)
        . concat
        . map declarationMapJsonToFunctions

    runQueries graph = do
        BF.runBF graph f w $ forM srcDstCombos $ \(src, destinations) ->
          queryBF src destinations
      where
        srcDstCombos = do
          (sources, destinations) <- queries
          src <- sources
          pure (src, destinations)


instance DG.HasWeight Function Double where
  weight = const 1

queryBF
  :: FullyQualifiedType -- ^ src
  -> [FullyQualifiedType] -- ^ destinations
  -> BF.BF s FullyQualifiedType Function
      [ ( BS.ByteString -- Human-readable function type (e.g. "[ghc-prim-0.10.0:GHC.Types.Char] -> ghc-9.6.2:GHC.Unit.Types.UnitId")
        , Maybe [Function] -- A path in the graph (if one exists)
        )
      ]
queryBF src destinations = do
  BF.bellmanFord src
  results <- mapM BF.pathTo destinations
  pure $ map (\(dst, res) -> (mkDescr dst, fmap (map DG.eMeta) res)) (zip destinations results)
  where
    mkDescr dst = unFullyQualifiedType src <> " -> " <> unFullyQualifiedType dst

-- | A function that takes a single non-function argument and returns a non-function value.
data Function = Function
  { _function_name :: BS.ByteString -- ^ e.g. "pack"
  , _function_module :: BS.ByteString -- ^ e.g. "Data.Text"
  , _function_package :: BS.ByteString -- ^ e.g. "text-1.2.4.1"
  , _function_typeSig :: Json.FunctionType FullyQualifiedType
  } deriving (Eq, Show, Ord)

-- | E.g. "base-4.18.0.0:GHC.Ptr.Ptr zstd-0.1.3.0:Codec.Compression.Zstd.FFI.Types.DDict".
--   Guaranteed to not be a function type (ie. will not contain any function arrows).
newtype FullyQualifiedType = FullyQualifiedType { unFullyQualifiedType :: BS.ByteString }
  deriving (Eq, Ord, Show, Generic, IsString)

instance Hashable FullyQualifiedType

declarationMapJsonToFunctions
  :: Json.DeclarationMapJson String
  -> [Function]
declarationMapJsonToFunctions dmj = concat $
  Map.toList moduleDeclarations <&> \(moduleName, nameMap) ->
    Map.toList nameMap <&> \(functionName, functionType) ->
      Function functionName moduleName package (FullyQualifiedType <$> functionType)
  where
    moduleDeclarations = Json.moduleDeclarations_map (Json.declarationMapJson_moduleDeclarations dmj')
    dmj' = Json.fmapDeclarationMapJson (BS.pack . UTF8.encode) dmj
    package = Json.declarationMapJson_package dmj'

instance DG.DirectedEdge Function FullyQualifiedType Function where
  fromNode = Json.functionType_arg . _function_typeSig
  toNode = Json.functionType_ret . _function_typeSig
  metaData = id

queryAll
  :: forall s v meta.
     ( Ord v
     , Hashable v
     , Show v
     , Show meta
     , Eq meta
     , DG.HasWeight meta Double
     )
  => (Double -> meta -> Double)
  -> Double
  -> v -- ^ src
  -> v -- ^ dst
  -> ([meta] -> String)
  -> Int -- ^ max number of results
  -> DG.IDigraph v meta
  -> ST.ST s [[DG.IdxEdge v meta]]
queryAll f w src dst disp maxCount graph = fmap (filter $ not . null) $ do
  resultRef <- STM.newSTRef (0, [])
  go resultRef graph
  reverse . snd <$> STM.readSTRef resultRef
  where
    getResult :: DG.Digraph s v meta -> ST s (Maybe [DG.IdxEdge v meta])
    getResult g = BF.runBF g f w $ BF.bellmanFord src >> BF.pathTo dst

    go resultRef ig = do
      let ifMissingResults action = do
            (count', _) <- STM.readSTRef resultRef
            unless (count' >= maxCount) action
      res <- DG.thaw ig >>= getResult
      case res of
        Nothing -> pure () -- no path
        Just [] -> pure () -- src == dst
        Just path -> do
          STM.modifySTRef' resultRef $ \(!count', !res') -> (count' + 1, path : res')
          traceM $ disp (map DG.eMeta path)
          forM_ path $ \edge -> do
            ifMissingResults $ do
              g' <- DG.thaw ig
              DG.removeEdge g' edge
              ig' <- DG.freeze g'
              go resultRef ig'
