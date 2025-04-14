{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
-- | Query the function graph.
--
--   TODO: Prioritize functions from _existing_ dependencies. Ie. take a list of dependencies for which functions are prioritized higher.
module FunGraph
  ( -- * Queries
    queryPathsGA, queryTreeGA, queryTreeAndPathsGA
  , queryTreeTimeoutIO, queryTreeTimeoutIOTrace
  , GraphAction, runGraphAction, runGraphActionTrace, GraphActionError(..)
    -- * Conversions
  , queryResultTreeToPaths, queryResultTreeToPathsStream
  , spTreeToPaths, spTreePathsCount
  , renderComposedFunctions, renderComposedFunctionsStr, parseComposedFunctions
  , renderFunction, parseFunction
    -- * Types
  , Function(..), TypedFunction, UntypedFunction, PrettyTypedFunction, functionPackageNoVersion, renderFunctionPackage
  , FullyQualifiedType, renderFullyQualifiedType, fullyQualifiedTypeToText
  -- * Re-exports
  , module Types
  , module Export
  , Json.FunctionType
  , DG.IDigraph, DG.Digraph
  , NE.NonEmpty
  , DG.freeze, DG.thaw
  ) where

import FunGraph.Types as Types
import FunGraph.Util
import FunGraph.Build as Export
import qualified Json
import qualified Data.Graph.Digraph as DG
import qualified Data.Graph.Dijkstra as Dijkstra
import Control.Monad.ST (ST, RealWorld)
import Data.Functor (void, (<&>))
import Data.List (foldl', sortOn, intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Debug.Trace (traceM)
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Types
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as ET
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Time
import qualified Streaming as S
import qualified Control.Monad.ST as ST
import qualified Streaming.Prelude as S
import qualified Streaming.Prelude.Extras

-- | Convert a shortest path tree into a list of shortests paths.
--
-- The 'NonEmpty' represents one or more edges between the same two nodes.
spTreeToPaths :: [NE.NonEmpty edge] -> [[edge]]
spTreeToPaths lst = do
  foldl' folder [] lst
  where
    folder :: [[a]] -> NE.NonEmpty a -> [[a]]
    folder [] ne = map (: []) $ NE.toList ne
    folder prefixes ne = concat $ map (\newEdge -> map (++ [newEdge]) prefixes) (NE.toList ne)

-- | How many shortest paths does the input shortest path tree contain?
--
--   This is the length of the outer list returned by 'spTreeToPaths' for the given input.
spTreePathsCount :: [NE.NonEmpty edge] -> Int
spTreePathsCount = do
  product . map NE.length

functionWeight :: (FullyQualifiedType, Maybe FullyQualifiedType) -> TypedFunction -> Double
functionWeight (src, mDst) function
  -- NOTE: We want to prioritize a function from the same package as the src/dst type -- e.g. a function from the "text" package in case we want to go to/from the "Text" type.
  -- TODO: but what what about e.g. "Maybe Text" (where "Maybe" is defined in "base" and "Text" in "text")?
  | srcPkg == fnPkg || mDstPkg == Just fnPkg = 0.5
  | otherwise = 1
  where
    fnPkg = [_function_package function]
    (srcPkg, mDstPkg) = (fqtPackage src, fqtPackage <$> mDst)
    -- TODO: 'fqtPackage' ignores the package of the type constructors for list, tuples and unit. So e.g. both "[Char]" and "Char" will return only the package for "Char".

queryTreeAndPathsGA
  :: ( v ~ FullyQualifiedType
     )
  => Int -- max count
  -> (v, Maybe v)
  -- ^ (src, dst)
  -> GraphAction s v (NE.NonEmpty TypedFunction)
       ([([NE.NonEmpty TypedFunction], Double)], [([TypedFunction], Double)])
  -- ^ ((Tree, Paths), weight)
queryTreeAndPathsGA maxCount srcDst =
  queryTreeGA maxCount srcDst <&> \tree ->
      (tree, queryResultTreeToPaths maxCount srcDst tree)

queryPathsGA
  :: ( v ~ FullyQualifiedType
     )
  => Int
  -> (v, Maybe v) -- ^ (src, dst)
  -> GraphAction s v (NE.NonEmpty TypedFunction) [([TypedFunction], Double)]
queryPathsGA maxCount (src, mDst) =
  queryResultTreeToPaths maxCount (src, mDst) <$> queryTreeGA maxCount (src, mDst)

-- | Convert the "shortest path"-tree produced by 'queryTreeGA'
--   to a list of shortest paths.
--
--   TODO: avoid sorting
queryResultTreeToPaths
  :: Int -- ^ maxCount
  -> (FullyQualifiedType, Maybe FullyQualifiedType) -- ^ (src, dst)
  -> [([NE.NonEmpty TypedFunction], Double)] -- ^ Output of 'queryTreeGA'
  -> [([TypedFunction], Double)]
queryResultTreeToPaths maxCount (src, mDst) res = take maxCount $
    sortOn (sortOnFun . fst)
    $ concatMap (\(nePath, weight) -> map (,weight) . spTreeToPaths $ nePath )
      res
  where
    sortOnFun path =
      ( length path
      , sum $ map (functionWeight (src, mDst)) path
      , if allFromSamePackage path then 0 else 1 :: Int
      )

    allFromSamePackage :: [TypedFunction] -> Bool
    allFromSamePackage = allEq . map _function_package

    allEq :: Eq a => [a] -> Bool
    allEq [] = True
    allEq (x:xs) = all (x ==) xs

-- TMP
queryResultTreeToPathsStream
  :: Monad m
  => S.Stream (S.Of ([NE.NonEmpty edge], Double)) m r
  -> S.Stream (S.Of ([edge], Double)) m r
queryResultTreeToPathsStream res =
    S.concat
    $ S.map (\(nePath, weight) -> map (,weight) . spTreeToPaths $ nePath )
      res

queryTreeTimeoutIO
  :: ( v ~ FullyQualifiedType
     , meta ~ NE.NonEmpty TypedFunction
     )
  => DG.Digraph RealWorld v meta
  -> Data.Time.NominalDiffTime
  -> Int
  -> (v, Maybe v)
  -> ExceptT (GraphActionError v) IO
      (S.Stream
        (S.Of ([NE.NonEmpty TypedFunction], Double))
        IO
        (Maybe ())
      )
queryTreeTimeoutIO g =
  queryTreeTimeoutIO' g Dijkstra.runDijkstra

queryTreeTimeoutIOTrace
  :: ( v ~ FullyQualifiedType
     , meta ~ NE.NonEmpty TypedFunction
     )
  => (String -> ST RealWorld ()) -- ^ Trace function
  -> DG.Digraph RealWorld v meta
  -> Data.Time.NominalDiffTime
  -> Int
  -> (v, Maybe v)
  -> ExceptT (GraphActionError v) IO
      (S.Stream (S.Of ([meta], Double)) IO (Maybe ()) )
queryTreeTimeoutIOTrace traceFun g =
  queryTreeTimeoutIO' g $ Dijkstra.runDijkstraTraceGeneric (>>= traceFunDebugGeneric traceFun)

queryTreeTimeoutIO'
  :: forall v meta.
     ( v ~ FullyQualifiedType
     , meta ~ NE.NonEmpty TypedFunction
     )
  => DG.Digraph RealWorld v meta
  -> (forall a. DG.Digraph RealWorld v meta -> (Double -> meta -> Double) -> Double -> Dijkstra.Dijkstra RealWorld v meta a -> ST RealWorld a)
  -> Data.Time.NominalDiffTime
  -> Int
  -> (v, Maybe v)
  -> ExceptT (GraphActionError v) IO
      (S.Stream
        (S.Of ([NE.NonEmpty TypedFunction], Double))
        IO
        (Maybe ()) -- 'Nothing' means "timeout", 'Just' means "no timeout"
      )
queryTreeTimeoutIO' graph runner timeout maxCount (src, mDst) = do
  srcVid <- lookupVertex src
  mDstVid <- traverse lookupVertex mDst
  let timeoutMicros = ceiling $ Data.Time.nominalDiffTimeToSeconds timeout * 1e6
      stream =
        Streaming.Prelude.Extras.timeoutStream timeoutMicros $
          S.take maxCount $
          S.hoistUnexposed runner' $ -- NOTE: Using 'S.hoist' doesn't work, but this does. I don't know why.
            Dijkstra.dijkstraShortestPathsLevelsStream maxCount 10 (srcVid, mDstVid) -- TODO: factor out "level" arg
      mapStream
        :: S.Of ([DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)], Double) a
        -> S.Of ([NE.NonEmpty TypedFunction], Double) a
      mapStream = first $ first (map (removeNonMin . DG.eMeta))
  pure $ S.maps mapStream stream
  where
    lookupVertex :: v -> ExceptT (GraphActionError v) IO DG.VertexId
    lookupVertex v = do
      mVid <- ET.lift $ ST.stToIO $ DG.lookupVertex graph v
      maybe
        (ET.throwError $ GraphActionError_NoSuchVertex v)
        pure
        mVid

    runner' :: forall a. Dijkstra.Dijkstra RealWorld v meta a -> IO a
    runner' = ST.stToIO . runner graph weightCombine initialWeight

    -- | Remove all edges whose 'functionWeight' is greater than the minimum 'functionWeight'
    removeNonMin :: NE.NonEmpty TypedFunction -> NE.NonEmpty TypedFunction
    removeNonMin functions =
      let minWeight = weightCombine 0 functions
          filterFun functionNE = functionWeight (src, mDst) functionNE == minWeight
      in NE.fromList $ NE.filter filterFun functions

    initialWeight :: Double
    initialWeight = 0

    weightCombine
      :: Double
      -> NE.NonEmpty TypedFunction
      -> Double
    weightCombine w functions =
      w + edgeWeightNE
      where
        edgeWeightNE =
          minimum $ map (functionWeight (src, mDst)) (NE.toList functions)

queryTreeGA
  :: ( v ~ FullyQualifiedType
     , meta ~ NE.NonEmpty TypedFunction
     )
  => Int
  -> (v, Maybe v)
  -> GraphAction s v meta [([meta], Double)]
queryTreeGA maxCount (src, mDst) =
  GraphAction weightCombine initialWeight $ do
    srcVid <- lookupVertex src
    mDstVid <- traverse lookupVertex mDst
    res <- ET.lift $ Dijkstra.dijkstraShortestPathsLevels maxCount 1 (srcVid, mDstVid) -- TODO: factor out "level" arg
    pure $ map (first (map (removeNonMin . DG.eMeta))) res
  where
    lookupVertex v = do
      mVid <- ET.lift $ do
        g <- Dijkstra.getGraph
        ET.lift $ DG.lookupVertex g v
      maybe
        (ET.throwError $ GraphActionError_NoSuchVertex v)
        pure
        mVid

    -- | Remove all edges whose 'functionWeight' is greater than the minimum 'functionWeight'
    removeNonMin :: NE.NonEmpty TypedFunction -> NE.NonEmpty TypedFunction
    removeNonMin functions =
      let minWeight = weightCombine 0 functions
          filterFun functionNE = functionWeight (src, mDst) functionNE == minWeight
      in NE.fromList $ NE.filter filterFun functions

    initialWeight :: Double
    initialWeight = 0

    weightCombine
      :: Double
      -> NE.NonEmpty TypedFunction
      -> Double
    weightCombine w functions =
      w + edgeWeightNE
      where
        edgeWeightNE =
          minimum $ map (functionWeight (src, mDst)) (NE.toList functions)

traceFunDebugGeneric
  :: (String -> ST s ()) -- ^ Trace function
  -> Dijkstra.TraceEvent FullyQualifiedType (NE.NonEmpty TypedFunction) Double
  -> ST s ()
traceFunDebugGeneric traceFun = \case
    Dijkstra.TraceEvent_Init srcVertex _ -> traceFun . T.unpack $ T.unwords
      [ "Starting Bellman-Ford for source vertex"
      , renderFullyQualifiedType (fst srcVertex)
      ]

    Dijkstra.TraceEvent_Push edge weight pathTo ->
      maybe (pure ()) traceFun (traceInterestingPush edge weight pathTo)

    Dijkstra.TraceEvent_Pop v weight pathTo ->
      maybe (pure ()) traceFun (traceInterestingPop v weight pathTo)

    Dijkstra.TraceEvent_FoundPath number weight path -> traceFun $ unwords
        [ "Found path no."
        , show number
        , "with length"
        , show $ length path
        , "and weight"
        , show weight <> "."
        , "Paths:\n"
        , let allPaths = spTreeToPaths (map DG.eMeta path)
              renderTypeSig = T.unpack . showTypeSig . typedFunctionsPathTypes
          in unlines $ map (\fn -> "\t" <> renderComposedFunctionsStr fn <> " :: " <> renderTypeSig fn) allPaths
        ]

    Dijkstra.TraceEvent_Done srcVertex -> traceFun . T.unpack $ T.unwords
      [ "Terminating Bellman-Ford for source vertex"
      , renderFullyQualifiedType (fst srcVertex)
      ]

    _ -> pure ()
  where
    interestingVertices = Set.fromList $ map parsePprTyConSingleton
      [ "bytestring-0.11.4.0:Data.ByteString.Lazy.Internal.ByteString"
      , "bytestring-0.11.4.0:Data.ByteString.Internal.Type.ByteString"
      , "text-2.0.2:Data.Text.Internal.Lazy.Text"
      , "text-2.0.2:Data.Text.Internal.Text"
      ]

    parsePackageWithVersion' pkg =
      either (error $ "BUG: parsePackageWithVersion': " <> show pkg) id (Types.parsePackageWithVersion pkg)

    interestingFunctions = Set.fromList
      [ Function
          { _function_name = "toStrict"
          , _function_module = "Data.ByteString"
          , _function_package = parsePackageWithVersion' "bytestring-0.11.4.0"
          , _function_typeSig = Json.FunctionType () ()
          }
      , Function
          { _function_name = "encodeUtf16LE"
          , _function_module = "Data.Text.Lazy.Encoding"
          , _function_package = parsePackageWithVersion' "text-2.0.2"
          , _function_typeSig = Json.FunctionType () ()
          }
      ]

    isInterestingPath
      :: [DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)] -- ^ in correct order!
      -> (Maybe [[TypedFunction]], [FullyQualifiedType])
    isInterestingPath pathTo =
      let interestingPath =
            map (NE.filter (\function -> fmap void function `Set.member` interestingFunctions) . DG.eMeta) pathTo
          pathTypes = idxEdgePathTypes pathTo
      in if all null interestingPath
          then (Nothing, pathTypes)
          else (Just interestingPath, pathTypes)

    showInterestingPath :: (Maybe [[TypedFunction]], [FullyQualifiedType]) -> String
    showInterestingPath (mFunctions, types) =
      let mkFunctionsStr =
            intercalate " -> " . map (maybe "uninteresting" (T.unpack . renderFunction) . listToMaybe)
          mkFinalString str
            | null types = "no path"
            | otherwise = str
      in mkFinalString $ "(" <> maybe "_" mkFunctionsStr mFunctions <> " :: " <> T.unpack (showTypeSig types) <> ")"

    traceInterestingPush
      :: DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)
      -> Double
      -> [DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)]
      -> Maybe String
    traceInterestingPush edge' weight pathTo = do
      let interestingPath@(mInterestingPath, _) = isInterestingPath (reverse pathTo)
          interestingEdge@(mInterestingEdge, _) = isInterestingPath [edge']
      -- trace either an interesting pathTo or an interesting edge
      void $ mInterestingPath <|> mInterestingEdge
      Just $ unwords
          [ "Queued vertex with prio"
          , show weight
          , "to"
          , T.unpack $ renderFullyQualifiedType $ DG.eTo edge'
          , "through edge"
          , showInterestingPath interestingEdge <> "."
          , "Path to 'from':"
          , showInterestingPath interestingPath
          ]

    traceInterestingPop v weight pathTo = do
      let interestingPath = isInterestingPath (reverse pathTo)
      if v `Set.member` interestingVertices
        then Just $ unwords
          [ "Popped vertex with prio"
          , show weight <> ":"
          , T.unpack $ renderFullyQualifiedType v <> "."
          , "Path to vertex:"
          , showInterestingPath interestingPath
          ]
        else Nothing

traceFunDebug
  :: Dijkstra.TraceEvent FullyQualifiedType (NE.NonEmpty TypedFunction) Double
  -> ST s ()
traceFunDebug =
  traceFunDebugGeneric traceM

data GraphActionError v
  = GraphActionError_NoSuchVertex v
      deriving (Eq, Show, Ord, Generic)

instance NFData a => NFData (GraphActionError a)

-- | A computation on a graph, e.g. a shortest path algorithm
data GraphAction s v meta a = GraphAction
  { graphActionWeightCombine :: Double -> meta -> Double -- ^ Weight combination function
  , graphActionInitialWeight :: Double -- ^ Initial weight
  , graphActionAction :: ExceptT (GraphActionError v) (Dijkstra.Dijkstra s v meta) a
  }

instance Functor (GraphAction s v meta) where
  fmap f ga = ga{ graphActionAction = fmap f (graphActionAction ga)}

-- | Run a 'GraphAction'
runGraphAction
  :: forall s v meta a.
     DG.Digraph s v meta
  -> GraphAction s v meta a
  -> ST s (Either (GraphActionError v) a)
runGraphAction dg ga =
  Dijkstra.runDijkstra
    dg
    (graphActionWeightCombine ga)
    (graphActionInitialWeight ga)
    (ET.runExceptT $ graphActionAction ga)

-- | Run a 'GraphAction' with tracing
runGraphActionTrace
  :: forall s v meta a.
     ( v ~ FullyQualifiedType
     , meta ~ NE.NonEmpty TypedFunction
     )
  => DG.Digraph s v meta
  -> GraphAction s v meta a
  -> ST s (Either (GraphActionError v) a)
runGraphActionTrace dg ga =
  Dijkstra.runDijkstraTraceGeneric (>>= traceFunDebug)
    dg
    (graphActionWeightCombine ga)
    (graphActionInitialWeight ga)
    (ET.runExceptT $ graphActionAction ga)
