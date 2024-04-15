{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Query the function graph.
--
--   TODO: Prioritize functions from _existing_ dependencies. Ie. take a list of dependencies for which functions are prioritized higher.
module FunGraph
  ( -- * Queries
    runQueryAll, runQueryTree, runQueryAllST, runQueryTreeST, runQuery, runQueryTrace
    -- * Conversions
  , queryResultTreeToPaths
  , spTreeToPaths, spTreePathsCount
  , renderComposedFunctions, renderComposedFunctionsStr, parseComposedFunctions
  , renderFunction, parseFunction, renderTypedFunction
    -- * Types
  , Function(..), TypedFunction, UntypedFunction, PrettyTypedFunction, functionPackageNoVersion
  , FullyQualifiedType(..), textToFullyQualifiedType, fullyQualifiedTypeToText
  -- * Re-exports
  , module Export
  , Json.FunctionType
  , DG.IDigraph, DG.Digraph
  , NE.NonEmpty
  , DG.freeze, DG.thaw
  ) where

import FunGraph.Types
import FunGraph.Util
import FunGraph.Build as Export
import qualified Json
import qualified Data.Graph.Digraph as DG
import qualified Data.Graph.Dijkstra as Dijkstra
import Control.Monad.ST (ST)
import Data.Functor (void)
import qualified Control.Monad.ST as ST
import Data.List (foldl', sortOn, intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Maybe (listToMaybe, fromMaybe)
import Debug.Trace (traceM)
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Types

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

functionWeight :: (FullyQualifiedType, FullyQualifiedType) -> TypedFunction -> Double
functionWeight (src, dst) function
  -- NOTE: We want to prioritize a function from the same package as the src/dst type -- e.g. a function from the "text" package in case we want to go to/from the "Text" type.
  -- TODO: but what what about e.g. "Maybe Text" (where "Maybe" is defined in "base" and "Text" in "text")?
  | srcPkg == fnPkg || dstPkg == fnPkg = 0.5
  | otherwise = 1
  where
    fnPkg = NE.singleton $ _function_package function
    (srcPkg, dstPkg) = (fqtPackage src, fqtPackage dst) -- TODO: 'fqtPackage' is broken

-- | Run 'runQueryAllST'
runQueryAll
  :: Int
  -> (FullyQualifiedType, FullyQualifiedType)
  -> DG.IDigraph FullyQualifiedType (NE.NonEmpty TypedFunction)
  -> [([TypedFunction], Double)]
runQueryAll maxCount (src, dst) graph =
  ST.runST $ do
    g <- DG.thaw graph
    runQueryAllST (Dijkstra.runDijkstra g) maxCount (src, dst)

-- | Run 'runQueryTreeST'
runQueryTree
  :: Int
  -> (FullyQualifiedType, FullyQualifiedType)
  -> DG.IDigraph FullyQualifiedType (NE.NonEmpty TypedFunction)
  -> [([NE.NonEmpty TypedFunction], Double)]
runQueryTree maxCount (src, dst) graph =
  ST.runST $ do
    g <- DG.thaw graph
    runQueryTreeST (Dijkstra.runDijkstra g) maxCount (src, dst)

-- | Passed to 'runQueryAllST' to run without tracing
runQuery
  :: DG.Digraph s v meta
  -> (Double -> meta -> Double)
  -> Double
  -> Dijkstra.Dijkstra s v meta a
  -> ST s a
runQuery = Dijkstra.runDijkstra

-- | Passed to 'runQueryAllST' to run with tracing turned on
runQueryTrace
  :: DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction)
  -> (Double -> NE.NonEmpty TypedFunction -> Double)
  -> Double
  -> Dijkstra.Dijkstra s FullyQualifiedType (NE.NonEmpty TypedFunction) a
  -> ST s a
runQueryTrace = Dijkstra.runDijkstraTraceGeneric traceFunDebug

runQueryAllST
  :: ( v ~ FullyQualifiedType
     , meta ~ NE.NonEmpty TypedFunction
     )
  => (forall a. (Double -> meta -> Double) -> Double -> Dijkstra.Dijkstra s v meta a -> ST s a)
  -> Int
  -> (FullyQualifiedType, FullyQualifiedType) -- ^ (src, dst)
  -> ST s [([TypedFunction], Double)]
runQueryAllST runner maxCount (src, dst) =
  queryResultTreeToPaths (src, dst) <$> runQueryTreeST runner maxCount (src, dst)

-- | Convert the "shortest path"-tree produced by 'runQueryTreeST'
--   to a list of shortest paths.
--
--   TODO: avoid sorting
queryResultTreeToPaths
  :: (FullyQualifiedType, FullyQualifiedType)
  -> [([NE.NonEmpty TypedFunction], Double)] -- ^ (src, dst)
  -> [([TypedFunction], Double)]
queryResultTreeToPaths (src, dst) res =
    sortOn (sortOnFun . fst)
    $ concatMap (\(nePath, weight) -> map (,weight) . spTreeToPaths $ nePath )
      res
  where
    sortOnFun path =
      ( length path
      , sum $ map (functionWeight (src, dst)) path
      , if allFromSamePackage path then 0 else 1 :: Int
      )

    allFromSamePackage :: [TypedFunction] -> Bool
    allFromSamePackage = allEq . map _function_package

    allEq :: Eq a => [a] -> Bool
    allEq [] = True
    allEq (x:xs) = all (x ==) xs

runQueryTreeST
  :: ( v ~ FullyQualifiedType
     , meta ~ NE.NonEmpty TypedFunction
     )
  => (forall a. (Double -> meta -> Double) -> Double -> Dijkstra.Dijkstra s v meta a -> ST s a)
  -> Int
  -> (FullyQualifiedType, FullyQualifiedType)
  -> ST s [([NE.NonEmpty TypedFunction], Double)]
runQueryTreeST runner maxCount (src, dst) = do
  res <- runner weightCombine initialWeight $
    fromMaybe [] <$> Dijkstra.dijkstraShortestPathsLevels maxCount 1 (src, dst) -- TODO: factor out "level" arg
  pure $ map (first (map (removeNonMin . DG.eMeta))) res
  where
    -- | Remove all edges whose 'functionWeight' is greater than the minimum 'functionWeight'
    removeNonMin :: NE.NonEmpty TypedFunction -> NE.NonEmpty TypedFunction
    removeNonMin functions =
      let minWeight = weightCombine 0 functions
          filterFun functionNE = functionWeight (src, dst) functionNE == minWeight
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
          minimum $ map (functionWeight (src, dst)) (NE.toList functions)

traceFunDebug
  :: Dijkstra.TraceEvent FullyQualifiedType (NE.NonEmpty TypedFunction) Double
  -> ST s ()
traceFunDebug = \case
    Dijkstra.TraceEvent_Init srcVertex _ -> traceM . T.unpack $ T.unwords
      [ "Starting Bellman-Ford for source vertex"
      , Types.renderFgTypeFgTyConQualified (unFullyQualifiedType (fst srcVertex))
      ]

    Dijkstra.TraceEvent_Push edge weight pathTo ->
      maybe (pure ()) traceM (traceInterestingPush edge weight pathTo)

    Dijkstra.TraceEvent_Pop v weight pathTo ->
      maybe (pure ()) traceM (traceInterestingPop v weight pathTo)

    Dijkstra.TraceEvent_FoundPath number weight path -> traceM $ unwords
        [ "Found path no."
        , show number
        , "with length"
        , show $ length path
        , "and weight"
        , show weight <> "."
        , "Paths:\n"
        , let allPaths = spTreeToPaths (map DG.eMeta path)
              renderTypeSig = bsToStr . showTypeSig . typedFunctionsPathTypes
          in unlines $ map (\fn -> "\t" <> renderComposedFunctionsStr fn <> " :: " <> renderTypeSig fn) allPaths
        ]

    _ -> pure ()
  where
    interestingVertices = Set.fromList $ map (FullyQualifiedType . parsePprTyConSingleton)
      [ "bytestring-0.11.4.0:Data.ByteString.Lazy.Internal.ByteString"
      , "bytestring-0.11.4.0:Data.ByteString.Internal.Type.ByteString"
      , "text-2.0.2:Data.Text.Internal.Lazy.Text"
      , "text-2.0.2:Data.Text.Internal.Text"
      ]

    parsePprTyConSingleton txt =
      either (error $ "BUG: parsePprTyConSingleton: " <> show txt) id ((`Types.FgType_TyConApp` []) <$> Types.parsePprTyCon txt)

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
      in mkFinalString $ "(" <> maybe "_" mkFunctionsStr mFunctions <> " :: " <> bsToStr (showTypeSig types) <> ")"

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
          , T.unpack $ Types.renderFgTypeFgTyConQualified $ unFullyQualifiedType $ DG.eTo edge'
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
          , T.unpack $ Types.renderFgTypeFgTyConQualified (unFullyQualifiedType v) <> "."
          , "Path to vertex:"
          , showInterestingPath interestingPath
          ]
        else Nothing
