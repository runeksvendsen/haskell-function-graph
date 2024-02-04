{-# LANGUAGE OverloadedStrings #-}
module FunGraph.Build
  ( fileReadDeclarationMap
  , withGraphFromFile
  , withFrozenGraphFromFile
  , buildGraph
  , buildGraphMut
  , FrozenGraph
  , Graph
  ) where

import FunGraph.Types
import qualified Json
import qualified Data.Graph.Digraph as DG
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.ST (ST)
import qualified Data.ByteString as BS
import qualified Control.Monad.ST as ST
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

type FrozenGraph = DG.IDigraph FullyQualifiedType (NE.NonEmpty TypedFunction)
type Graph s = DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction)

fileReadDeclarationMap
  :: FilePath
  -> IO (Either String [Json.DeclarationMapJson String])
fileReadDeclarationMap fileName = A.eitherDecode <$> BSL.readFile fileName

withGraphFromFile
  :: FilePath
  -> (Graph ST.RealWorld -> IO a)
  -> IO a
withGraphFromFile fileName f = do
  graphData <- either fail pure =<< fileReadDeclarationMap fileName
  graph <- ST.stToIO $ buildGraphMut graphData
  f graph

withFrozenGraphFromFile
  :: FilePath
  -> (FrozenGraph -> IO a)
  -> IO a
withFrozenGraphFromFile fileName f =
  withGraphFromFile fileName $ \mutGraph ->
    ST.stToIO (DG.freeze mutGraph) >>= f

-- | Build an immutable graph
buildGraph
  :: [Json.DeclarationMapJson String]
  -> ST s (DG.IDigraph FullyQualifiedType (NE.NonEmpty TypedFunction))
buildGraph declarationMapJsonList =
  buildGraphMut declarationMapJsonList >>= DG.freeze

-- | Build a mutable graph
buildGraphMut
  :: [Json.DeclarationMapJson String]
  -> ST s (DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction))
buildGraphMut =
  buildGraph'
  where
    excludeTypes = []

    -- TODO: no version number
    excludePackages =
      [ "basic-prelude-0.7.0"
      , "incipit-base-0.5.1.0"
      , "incipit-core-0.5.1.0"
      , "rebase-1.20"
      , "rerebase-1.20"
      , "rio-0.1.22.0"
      , "pa-prelude-0.1.0.0"
      , "shakers-0.0.50" -- NOTE: re-exports all of basic-prelude:BasicPrelude
      ]

    excludeModulePatterns =
      [ "Internal"
      ]

    isExcludedPackage = (`elem` excludePackages)

    isExcluded :: TypedFunction -> Bool
    isExcluded function =
      DG.fromNode function `elem` excludeTypes
        || DG.toNode function `elem` excludeTypes
        || any (`BS.isInfixOf` _function_module function) excludeModulePatterns

    functionIdentity fn =
      ( _function_name fn
      , _function_module fn
      , _function_package fn
      )

    buildGraph'
      :: [Json.DeclarationMapJson String]
      -> ST s (DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction))
    buildGraph' =
      DG.fromEdgesMulti
        . Set.fromList
        . nubOrdOn functionIdentity
        . filter (not . isExcluded)
        . concatMap declarationMapJsonToFunctions . filter (not . isExcludedPackage . Json.declarationMapJson_package)
