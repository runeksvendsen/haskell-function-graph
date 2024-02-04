{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module FunGraph
  ( fileReadDeclarationMap
  , withGraphFromFile, withFrozenGraphFromFile
  , buildGraph
  , buildGraphMut
  , runQueryAll, runQueryAllST, runQuery, runQueryTrace
  , spTreeToPaths, spTreePathsCount
  , renderComposedFunctions, renderComposedFunctionsStr, parseComposedFunctions
  , renderFunction, parseFunction, renderTypedFunction
  , Function(..), TypedFunction, UntypedFunction, PrettyTypedFunction, functionPackageNoVersion
  , FullyQualifiedType(..), textToFullyQualifiedType, fullyQualifiedTypeToText
  , Graph, FrozenGraph
  , bsToStr
  -- * Re-exports
  , Json.FunctionType
  , DG.IDigraph, DG.Digraph
  , NE.NonEmpty
  , DG.freeze, DG.thaw
  ) where

import FunGraph.Types
import FunGraph.Build
import qualified Json
import qualified Data.Graph.Digraph as DG
import qualified Data.Graph.Dijkstra as Dijkstra
import Control.Monad.ST (ST)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Functor (void)
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Control.Monad.ST as ST
import Data.List (foldl', sortOn, intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Maybe (listToMaybe, fromMaybe)
import Debug.Trace (traceM)
import Control.Applicative ((<|>))
import Data.Bifunctor (first)

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
  | srcPkg == fnPkg || dstPkg == fnPkg = 0.5
  | otherwise = 1
  where
    fnPkg = _function_package function
    (srcPkg, dstPkg) = (fqtPackage src, fqtPackage dst)

runQueryAll
  :: Int
  -> (FullyQualifiedType, FullyQualifiedType)
  -> DG.IDigraph FullyQualifiedType (NE.NonEmpty TypedFunction)
  -> [([TypedFunction], Double)]
runQueryAll maxCount (src, dst) graph =
  ST.runST $ do
    g <- DG.thaw graph
    runQueryAllST (Dijkstra.runDijkstra g) maxCount (src, dst)

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
  -> (FullyQualifiedType, FullyQualifiedType)
  -> ST s [([TypedFunction], Double)]
runQueryAllST runner maxCount (src, dst) = do
  res <- runner weightCombine initialWeight $
    fromMaybe [] <$> Dijkstra.dijkstraShortestPathsLevels maxCount 1 (src, dst)
  let res' :: [([NE.NonEmpty TypedFunction], Double)]
      res' = map (first (map DG.eMeta)) res

      removeNonMin :: NE.NonEmpty TypedFunction -> NE.NonEmpty TypedFunction
      removeNonMin functions =
        let minWeight = minimum $ map (functionWeight (src, dst)) (NE.toList functions)
            filterFun functionNE = functionWeight (src, dst) functionNE == minWeight
        in NE.fromList $ NE.filter filterFun functions

  pure
    $ sortOn (sortOnFun . fst)
    $ concat
    $ map (\(nePath, weight) -> map (,weight) . spTreeToPaths . map removeNonMin $ nePath )
      res'
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
    Dijkstra.TraceEvent_Init srcVertex _ -> traceM $ unwords
      [ "Starting Bellman-Ford for source vertex"
      , bsToStr (unFullyQualifiedType (fst srcVertex))
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
              renderTypeSig :: [TypedFunction] -> String
              renderTypeSig = showTypeSig . toPathTypes
                (Json.functionType_ret . _function_typeSig)
                (Json.functionType_arg . _function_typeSig)
          in unlines $ map (\fn -> "\t" <> renderComposedFunctionsStr fn <> " :: " <> renderTypeSig fn) allPaths
        ]

    _ -> pure ()
  where
    interestingVertices = Set.fromList $ map FullyQualifiedType
      [ "bytestring-0.11.4.0:Data.ByteString.Lazy.Internal.ByteString"
      , "bytestring-0.11.4.0:Data.ByteString.Internal.Type.ByteString"
      , "text-2.0.2:Data.Text.Internal.Lazy.Text"
      , "text-2.0.2:Data.Text.Internal.Text"
      ]

    interestingFunctions = Set.fromList
      [ Function
          { _function_name = "toStrict"
          , _function_module = "Data.ByteString"
          , _function_package = "bytestring-0.11.4.0"
          , _function_typeSig = Json.FunctionType () ()
          }
      , Function
          { _function_name = "encodeUtf16LE"
          , _function_module = "Data.Text.Lazy.Encoding"
          , _function_package = "text-2.0.2"
          , _function_typeSig = Json.FunctionType () ()
          }
      ]

    isInterestingPath
      :: [DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)] -- ^ in correct order!
      -> (Maybe [[TypedFunction]], [FullyQualifiedType])
    isInterestingPath pathTo =
      let interestingPath = map (NE.filter $ \function -> fmap void function `Set.member` interestingFunctions) (map DG.eMeta pathTo)
          pathTypes = toPathTypes DG.eTo DG.eFrom pathTo
      in if all null interestingPath
          then (Nothing, pathTypes)
          else (Just interestingPath, pathTypes)

    toPathTypes
      :: (a -> FullyQualifiedType)
      -> (a -> FullyQualifiedType)
      -> [a]
      -> [FullyQualifiedType]
    toPathTypes getTo getFrom = \case
      pathTo@(firstEdge:_) ->
        getFrom firstEdge : map getTo pathTo
      [] -> []

    showInterestingPath :: (Maybe [[TypedFunction]], [FullyQualifiedType]) -> String
    showInterestingPath (mFunctions, types) =
      let mkFunctionsStr =
            intercalate " -> " . map (maybe "uninteresting" (bsToStr . renderFunction) . listToMaybe)
          mkFinalString str
            | null types = "no path"
            | otherwise = str
      in mkFinalString $ "(" <> maybe "_" mkFunctionsStr mFunctions <> " :: " <> showTypeSig types <> ")"

    showTypeSig :: [FullyQualifiedType] -> String
    showTypeSig types =
      let typeBs = BS.intercalate " -> " $ map unFullyQualifiedType types
      in bsToStr typeBs

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
          , bsToStr $ unFullyQualifiedType $ DG.eTo edge'
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
          , bsToStr (unFullyQualifiedType v) <> "."
          , "Path to vertex:"
          , showInterestingPath interestingPath
          ]
        else Nothing

bsToStr :: BSC8.ByteString -> String
bsToStr = UTF8.decode . BS.unpack
