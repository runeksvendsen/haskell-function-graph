{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module MyLib
  ( fileReadDeclarationMap
  , withGraphFromFile, withFrozenGraphFromFile
  , buildGraph
  , buildGraphMut
  , runQueryAll, runQueryAllST
  , runPrintQueryAll
  , spTreeToPaths, spTreePathsCount
  , renderComposedFunctions, renderComposedFunctionsStr, parseComposedFunctions
  , renderFunction, parseFunction, renderTypedFunction
  , Function(..), TypedFunction, UntypedFunction, PrettyTypedFunction, functionPackageNoVersion
  , FullyQualifiedType(..), textToFullyQualifiedType, fullyQualifiedTypeToText
  , Graph, FrozenGraph
  -- * Re-exports
  , Json.FunctionType
  , DG.IDigraph, DG.Digraph
  , NE.NonEmpty
  , DG.freeze, DG.thaw
  ) where

import qualified Json
import qualified Data.Graph.Digraph as DG
import qualified Data.Graph.Dijkstra as Dijkstra
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.ST (ST)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Search as Search
import qualified Data.Map as Map
import Data.Functor ((<&>), void)
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Control.Monad.ST as ST
import Data.String (IsString)
import Control.Monad (guard)
import Data.List (intersperse, foldl', sortOn, intercalate)
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Control.DeepSeq (NFData)
import Debug.Trace (traceM)
import Control.Applicative ((<|>))

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
  graphData <- either fail pure =<< MyLib.fileReadDeclarationMap fileName
  graph <- ST.stToIO $ MyLib.buildGraphMut graphData
  f graph

withFrozenGraphFromFile
  :: FilePath
  -> (FrozenGraph -> IO a)
  -> IO a
withFrozenGraphFromFile fileName f =
  withGraphFromFile fileName $ \mutGraph ->
    ST.stToIO (DG.freeze mutGraph) >>= f

runPrintQueryAll
  :: Int
  -> [Json.DeclarationMapJson String]
  -> (FullyQualifiedType, FullyQualifiedType)
  -> IO ()
runPrintQueryAll maxCount declarationMapJsonList (src, dst) = do
  putStrLn $ unwords
    [ "Finding the"
    , show maxCount
    , "first paths from"
    , bsToStr $ unFullyQualifiedType src
    , "to"
    , bsToStr $ unFullyQualifiedType dst
    ]
  let !res = runQueryAll maxCount (src, dst) (ST.runST $ buildGraph declarationMapJsonList)
  putStrLn ""
  putStrLn $ disp' res
  putStrLn $ "Got " <> show (length res) <> " results"
  where
    disp' :: [([TypedFunction], Double)] -> String
    disp' = unlines . map (\(path, weight) -> show weight <> ": " <> renderComposedFunctionsStr path)

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
  ST.runST $ DG.thaw graph >>= runQueryAllST maxCount (src, dst)

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
    excludeTypes = map FullyQualifiedType
      [
      ]

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
        . concat
        . map declarationMapJsonToFunctions
        . filter (not . isExcludedPackage . Json.declarationMapJson_package)

runQueryAllST
  :: Int
  -> (FullyQualifiedType, FullyQualifiedType)
  -> DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction)
  -> ST s [([TypedFunction], Double)]
runQueryAllST maxCount (src, dst) graph = do
  res <- queryAllEvenFaster traceFunDebug weightCombine' initialWeight src dst dispFun maxCount graph
  let res' :: [[NE.NonEmpty TypedFunction]]
      res' = map (map DG.eMeta . fst) res

      removeNonMin :: NE.NonEmpty TypedFunction -> NE.NonEmpty TypedFunction
      removeNonMin functions =
        let minWeight = minimum $ map (functionWeight (src, dst)) (NE.toList functions)
            filterFun functionNE = functionWeight (src, dst) functionNE == minWeight
        in NE.fromList $ NE.filter filterFun functions

  pure
    $ map (,0) -- TMP!
    $ sortOn sortOnFun
    $ concat
    $ map spTreeToPaths
    $ map (map removeNonMin)
    $ res'
  where
    debug = False

    weightCombine' = weightCombine (src, dst)

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

    dispFun fns =
      if debug
        then show (length fns) <> ":\n" <> disp' fns
        else ""

    disp' :: [NE.NonEmpty (Function typeSig)] -> String
    disp' =
      let sortFun fun = (_function_package fun, _function_module fun, _function_name fun)
      in unlines . map renderComposedFunctionsStr . sortOn (fmap sortFun . listToMaybe) . spTreeToPaths

-- TODO: why not 0?
initialWeight :: Double
initialWeight = 0

weightCombine
  :: (FullyQualifiedType, FullyQualifiedType)
  -> Double
  -> NE.NonEmpty TypedFunction
  -> Double
weightCombine (src, dst) w functions =
  w + edgeWeightNE
  where
    edgeWeightNE =
      minimum $ map (functionWeight (src, dst)) (NE.toList functions)

-- | A function that takes a single non-function argument and returns a non-function value.
--
-- Generic over type signature.
data Function typeSig = Function
  { _function_name :: BS.ByteString -- ^ e.g. "pack"
  , _function_module :: BS.ByteString -- ^ e.g. "Data.Text"
  , _function_package :: BS.ByteString -- ^ e.g. "text-1.2.4.1"
  , _function_typeSig :: typeSig
  } deriving (Eq, Show, Ord, Generic)

instance NFData a => NFData (Function a)

instance Functor Function where
  fmap f fn = fn { _function_typeSig = f $ _function_typeSig fn }

functionPackageNoVersion
  :: Show typeSig
  => Function typeSig
  -> BS.ByteString
functionPackageNoVersion fn =
  case Search.split "-" (_function_package fn) of
    lst | length lst > 1 ->
      BS.concat $ intersperse "-" (init lst)
    _ ->
      error $ "Invalid package name for Function: " <> show fn

-- | Render composed functions.
--
-- Function list is in order of application, e.g. @g . f@ is @[f, g]@.
renderComposedFunctions :: [Function typeSig] -> BS.ByteString
renderComposedFunctions fnLst =
  let dispFunLine = BS.concat . intersperse " . " . map renderFunction . reverse
  in dispFunLine fnLst

-- | Same as 'renderComposedFunctions' but returns a 'String'
renderComposedFunctionsStr :: [Function typeSig] -> String
renderComposedFunctionsStr =
  bsToStr . renderComposedFunctions

-- | Parse the output of 'renderComposedFunctions'
parseComposedFunctions :: BS.ByteString -> Maybe (NE.NonEmpty UntypedFunction)
parseComposedFunctions bs = do
  bsFunctionList <- NE.nonEmpty $ reverse $ Search.split " . " bs
  let parsedMaybeFunctions = NE.map parseFunction bsFunctionList
  sequence parsedMaybeFunctions

-- | Produce e.g. "text-2.0.2:Data.Text.Encoding.encodeUtf16BE" from an untyped 'Function'
renderFunction :: Function typeSig -> BS.ByteString
renderFunction fn =
  _function_package fn <> ":" <> _function_module fn <> "." <> _function_name fn

-- | Render a function's name (output of 'renderFunction') and its FROM and TO type.
renderTypedFunction
  :: TypedFunction
  -> (BS.ByteString, (FullyQualifiedType, FullyQualifiedType))
  -- ^ (output of 'renderFunction', (FROM type, TO type))
renderTypedFunction fn =
  ( renderFunction fn
  , (Json.functionType_arg sig, Json.functionType_ret sig)
  )
  where
    sig = _function_typeSig fn

-- | Parse e.g. "text-2.0.2:Data.Text.Encoding.encodeUtf16BE" to an untyped 'Function'
parseFunction :: BS.ByteString -> Maybe UntypedFunction
parseFunction bs =
  case BSC8.split ':' bs of
    [pkg, moduleAndName] -> do
      (moduleNameWithSuffix, name) <- spanEndNonEmpty (not . (== '.')) moduleAndName
      moduleName <- BS.stripSuffix "." moduleNameWithSuffix
      guard $ not (BS.null moduleName)
      pure $
        Function
          { _function_name = name
          , _function_module = moduleName
          , _function_package = pkg
          , _function_typeSig = ()
          }
    _ -> Nothing
  where
    -- A version of 'spanEnd' that does not return the empty ByteString
    spanEndNonEmpty f bs' =
      let (a, b) = BSC8.spanEnd f bs'
          result
            | BS.null a || BS.null b = Nothing
            | otherwise = Just (a, b)
      in result

-- | A typed 'Function'
type TypedFunction = Function (Json.FunctionType FullyQualifiedType)

-- | A untyped 'Function'
type UntypedFunction = Function ()

-- | A 'TypedFunction' with a pretty 'Show' instance
newtype PrettyTypedFunction = PrettyTypedFunction { unPrettyTypedFunction :: TypedFunction }
  deriving (Eq, Ord)

instance Show PrettyTypedFunction where
  show = prettyFunction . unPrettyTypedFunction
    where
      prettyFunction fun =
        bsToStr $ BS.concat $
          [_function_package fun
          , ":"
          , _function_module fun
          , "."
          , _function_name fun
          ] ++
            let sig = _function_typeSig fun
                arg = unFullyQualifiedType $ Json.functionType_arg sig
                ret = unFullyQualifiedType $ Json.functionType_ret sig
            in [" :: ", arg, " -> ", ret]

-- | E.g. "base-4.18.0.0:GHC.Ptr.Ptr zstd-0.1.3.0:Codec.Compression.Zstd.FFI.Types.DDict".
--   Guaranteed to not be a function type (ie. will not contain any function arrows).
newtype FullyQualifiedType = FullyQualifiedType { unFullyQualifiedType :: BS.ByteString }
  deriving (Eq, Ord, Show, Generic, IsString, NFData)

fqtPackage :: FullyQualifiedType -> BS.ByteString
fqtPackage = BS.takeWhile (/= toEnum (fromEnum ':')) . unFullyQualifiedType

textToFullyQualifiedType
  :: T.Text
  -> FullyQualifiedType
textToFullyQualifiedType =
  FullyQualifiedType . TE.encodeUtf8

fullyQualifiedTypeToText
  :: FullyQualifiedType
  -> T.Text
fullyQualifiedTypeToText =
  TE.decodeUtf8 . unFullyQualifiedType

instance Hashable FullyQualifiedType

declarationMapJsonToFunctions
  :: Json.DeclarationMapJson String
  -> [TypedFunction]
declarationMapJsonToFunctions dmj = concat $
  Map.toList moduleDeclarations <&> \(moduleName, nameMap) ->
    Map.toList nameMap <&> \(functionName, functionType) ->
      Function functionName moduleName package (FullyQualifiedType <$> functionType)
  where
    moduleDeclarations = Json.moduleDeclarations_map (Json.declarationMapJson_moduleDeclarations dmj')
    dmj' = Json.fmapDeclarationMapJson (BS.pack . UTF8.encode) dmj
    package = Json.declarationMapJson_package dmj'

instance DG.DirectedEdge TypedFunction FullyQualifiedType TypedFunction where
  fromNode = Json.functionType_arg . _function_typeSig
  toNode = Json.functionType_ret . _function_typeSig
  metaData = id

-- | Uses 'Dijkstra.dijkstraKShortestPaths'
queryAllEvenFaster
  :: forall s v meta.
     ( Ord v
     , Hashable v
     , Show v
     , Show meta
     , Eq meta
     )
  => (Dijkstra.TraceEvent v (NE.NonEmpty meta) Double -> ST s ())
  -> (Double -> NE.NonEmpty meta -> Double)
  -> Double
  -> v -- ^ src
  -> v -- ^ dst
  -> ([NE.NonEmpty meta] -> String)
  -> Int -- ^ max number of results
  -> DG.Digraph s v (NE.NonEmpty meta)
  -> ST.ST s [([DG.IdxEdge v (NE.NonEmpty meta)], Double)]
queryAllEvenFaster traceFun f w src dst disp maxCount g = do
  Dijkstra.runDijkstraTraceGeneric traceFun g f w $
    fromMaybe [] <$> Dijkstra.dijkstraShortestPathsLevels (maxCount * 100) 2 (src, dst)

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
          in unlines $ map (("\t" <>) . renderComposedFunctionsStr) allPaths -- TODO: why wrong order?
        ]

    -- Dijkstra.TraceEvent_Relax edge weight ->
    --   maybe (pure ()) traceM (traceInterestingRelax edge weight)

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
      :: [DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)]
      -> (Maybe [[TypedFunction]], [FullyQualifiedType])
    isInterestingPath pathTo =
      let pathTo' = reverse pathTo
          interestingPath = map (NE.filter $ \function -> fmap void function `Set.member` interestingFunctions) (map DG.eMeta pathTo')
          pathTypes = maybe id (\idxEdge -> (DG.eFrom idxEdge :)) (listToMaybe pathTo) $ map DG.eTo pathTo'
      in if all null interestingPath
          then (Nothing, pathTypes)
          else (Just interestingPath, pathTypes)

    showInterestingPath :: (Maybe [[TypedFunction]], [FullyQualifiedType]) -> String
    showInterestingPath (mFunctions, types) =
      let mkFunctionsStr =
            intercalate " -> " . map (maybe "uninteresting" (bsToStr . renderFunction) . listToMaybe)
          typeBs = BS.intercalate " -> " $ map unFullyQualifiedType types
          mkFinalString str
            | null types = "no path"
            | otherwise = str
      in mkFinalString $ "(" <> maybe "_" mkFunctionsStr mFunctions <> " :: " <> bsToStr typeBs <> ")"

    traceInterestingPush
      :: DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)
      -> Double
      -> [DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)]
      -> Maybe String
    traceInterestingPush edge' weight pathTo = do
      let interestingPath@(mInterestingPath, _) = isInterestingPath pathTo
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
      let interestingPath@(mInterestingPath, _) = isInterestingPath pathTo
      if v `Set.member` interestingVertices
        then Just $ unwords
          [ "Popped vertex with prio"
          , show weight <> ":"
          , bsToStr (unFullyQualifiedType v) <> "."
          , "Path to vertex:"
          , showInterestingPath interestingPath
          ]
        else Nothing

    -- traceInterestingRelax edge weight = do
    --   let interestingPath@(mInterestingPath, _) = isInterestingPath [edge]
    --   void mInterestingPath
    --   Just $ unwords
    --     [ "Relaxing edge", showInterestingPath interestingPath <> "."
    --     , "Current 'distTo' for"
    --     , showIndexedVertex (DG.eTo edge, DG.eToIdx edge)
    --     , "is"
    --     , show distToTo <> "."
    --     ]

bsToStr :: BSC8.ByteString -> String
bsToStr = UTF8.decode . BS.unpack
