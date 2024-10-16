{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module FunGraph.Build
  ( fileReadDeclarationMap
  , withGraphFromFile
  , withFrozenGraphFromFile
  , buildGraphMut
  , defaultBuildConfig, emptyBuildConfig
  , BuildConfig
    , buildConfig_excludePackages
    , buildConfig_excludeTypes
    , buildConfig_excludeTypesUnqualified
    , buildConfig_excludeModulePatterns
  , FrozenGraph
  , Graph
  ) where

import FunGraph.Types
import qualified Json
import qualified Data.Graph.Digraph as DG
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.ST (ST)
import qualified Control.Monad.ST as ST
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Types
import qualified Data.Text as T

type FrozenGraph = DG.IDigraph FullyQualifiedType (NE.NonEmpty TypedFunction)
type Graph s = DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction)

fileReadDeclarationMap
  :: FilePath
  -> IO (Either String [Json.DeclarationMapJson T.Text])
fileReadDeclarationMap fileName = A.eitherDecode <$> BSL.readFile fileName

withGraphFromFile
  :: BuildConfig
  -> FilePath
  -> (Graph ST.RealWorld -> IO a)
  -> IO a
withGraphFromFile cfg fileName f = do
  graphData <- either fail pure =<< fileReadDeclarationMap fileName
  graph <- ST.stToIO $ buildGraphMut cfg graphData
  f graph

withFrozenGraphFromFile
  :: BuildConfig
  -> FilePath
  -> (FrozenGraph -> IO a)
  -> IO a
withFrozenGraphFromFile cfg fileName f =
  withGraphFromFile cfg fileName $ \mutGraph ->
    ST.stToIO (DG.freeze mutGraph) >>= f

-- | Build a mutable graph
buildGraphMut
  :: BuildConfig
  -> [Json.DeclarationMapJson T.Text]
  -> ST s (DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction))
buildGraphMut cfg =
  buildGraph'
  where
    excludeTypes = buildConfig_excludeTypes cfg
    excludePackages = buildConfig_excludePackages cfg
    excludeModulePatterns = buildConfig_excludeModulePatterns cfg
    excludeTypesUnqualified = buildConfig_excludeTypesUnqualified cfg

    isExcludedPackage =
      (`Set.member` excludePackages) . Types.fgPackageName . Json.declarationMapJson_package

    isExcludedFunction :: TypedFunction -> Bool
    isExcludedFunction function =
      let fnTypes = [DG.fromNode function, DG.toNode function]
          fnTypesUnqualified = Set.fromList $ map renderFullyQualifiedType fnTypes
          intersect types cfgTypes =
            -- 'cfgTypes' is the first argument to (&&) to speed up evaluation with 'emptyBuildConfig'
            not (Set.null cfgTypes) && not (Set.null types) && not (Set.null $ Set.intersection types cfgTypes)
      in Set.fromList fnTypes `intersect` excludeTypes
        || fnTypesUnqualified `intersect` excludeTypesUnqualified
        || any (`T.isInfixOf` _function_module function) excludeModulePatterns

    functionIdentity fn =
      ( _function_name fn
      , _function_module fn
      , _function_package fn
      )

    buildGraph'
      :: [Json.DeclarationMapJson T.Text]
      -> ST s (DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction))
    buildGraph' =
      DG.fromEdgesMulti
        . Set.fromList
        . nubOrdOn functionIdentity -- remove duplicates
        . filter (not . isExcludedFunction) -- remove excluded functions
        . concatMap declarationMapJsonToFunctions
        . filter (not . isExcludedPackage) -- remove excluded packages

-- | Excludes various preludes and internal modules
defaultBuildConfig :: BuildConfig
defaultBuildConfig = BuildConfig
  { buildConfig_excludePackages = Set.fromList
      [ "basic-prelude"
      , "incipit-base"
      , "incipit-core"
      , "rebase"
      , "rerebase"
      , "rio"
      , "pa-prelude"
      , "shakers" -- NOTE: re-exports all of basic-prelude:BasicPrelude
      ]
  , buildConfig_excludeTypes = mempty
  , buildConfig_excludeTypesUnqualified = mempty
  , buildConfig_excludeModulePatterns = Set.fromList
      [ "Internal"
      ]
  }

-- | No exclusions
emptyBuildConfig :: BuildConfig
emptyBuildConfig = mempty

-- TODO: add "exclude loops"
data BuildConfig = BuildConfig
  { buildConfig_excludePackages :: Set.Set T.Text
  -- ^ Set of package names (without version postfix)
  , buildConfig_excludeTypes :: Set.Set FullyQualifiedType
  -- ^ Exclude function if either src or dst type matches this
  , buildConfig_excludeTypesUnqualified :: Set.Set T.Text
  -- ^ Only the type name, e.g. @Text@, @String@, @ByteArray#@
  , buildConfig_excludeModulePatterns :: Set.Set T.Text
  -- ^ Exclude if '_function_module' 'BS.isInfixOf'
  } deriving (Eq, Show)

instance Semigroup BuildConfig where
  bc1 <> bc2 =
    let concat' f = f bc1 <> f bc2
    in BuildConfig
      { buildConfig_excludePackages =
          concat' buildConfig_excludePackages
      , buildConfig_excludeTypes =
          concat' buildConfig_excludeTypes
      , buildConfig_excludeTypesUnqualified =
          concat' buildConfig_excludeTypesUnqualified
      , buildConfig_excludeModulePatterns =
          concat' buildConfig_excludeModulePatterns
      }

instance Monoid BuildConfig where
  mempty = BuildConfig
    { buildConfig_excludePackages = mempty
    , buildConfig_excludeTypes = mempty
    , buildConfig_excludeTypesUnqualified = mempty
    , buildConfig_excludeModulePatterns = mempty
    }
