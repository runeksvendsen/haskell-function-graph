{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module MyLib
  ( fileReadDeclarationMap
  , withGraphFromFile
  , buildGraph
  , buildGraphMut
  , runQueryAll
  , runPrintQueryAll
  , runQuerySingleResult, runQuerySingleResultST
  , spTreeToPaths, spTreePathsCount
  , renderComposedFunctions, renderComposedFunctionsStr, parseComposedFunctions
  , renderFunction, parseFunction, renderTypedFunction
  , Function(..), TypedFunction, UntypedFunction, functionPackageNoVersion
  , FullyQualifiedType(..), textToFullyQualifiedType, fullyQualifiedTypeToText
  , Graph
  -- * Re-exports
  , Json.FunctionType
  , DG.IDigraph
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
import Data.Functor ((<&>))
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Control.Monad.ST as ST
import Data.String (IsString)
import Control.Monad (forM_, unless, guard)
import Debug.Trace (traceM)
import Data.List (intersperse, foldl', sortOn, subsequences)
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.STRef as STM
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Control.DeepSeq (NFData)

type Graph = DG.IDigraph FullyQualifiedType (NE.NonEmpty TypedFunction)

fileReadDeclarationMap
  :: FilePath
  -> IO (Either String [Json.DeclarationMapJson String])
fileReadDeclarationMap fileName = A.eitherDecode <$> BSL.readFile fileName

withGraphFromFile
  :: FilePath
  -> (Graph -> IO a)
  -> IO a
withGraphFromFile fileName f = do
  graphData <- either fail pure =<< MyLib.fileReadDeclarationMap fileName
  let !graph = ST.runST $ MyLib.buildGraph graphData
  f graph

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
    , UTF8.decode $ BS.unpack $ unFullyQualifiedType src
    , "to"
    , UTF8.decode $ BS.unpack $ unFullyQualifiedType dst
    ]
  let !res = runQueryAll maxCount (src, dst) (ST.runST $ buildGraph declarationMapJsonList)
  putStrLn ""
  putStrLn $ disp' res
  putStrLn $ "Got " <> show (length $ concat res) <> " results"
  where
    disp' = unlines . map renderComposedFunctionsStr

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
  -> (DG.IDigraph FullyQualifiedType (NE.NonEmpty TypedFunction))
  -> [[TypedFunction]]
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
  -> (DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction))
  -> ST s [[TypedFunction]]
runQueryAllST maxCount (src, dst) graph = do
  res <- queryAll weightCombine' initialWeight src dst dispFun maxCount graph
  let res' = map (map DG.eMeta) res
  pure
    $ sortOn sortOnFun
    $ concat
    $ map spTreeToPaths res'
  where
    debug = False

    weightCombine' = weightCombine (src, dst)

    sortOnFun path =
      -- (sum weights, length path, allFunctionsFromSamePackage)
      ( sum $ map (functionWeight (src, dst)) path
      , length path
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

runQuerySingleResult
  :: (FullyQualifiedType, FullyQualifiedType)
  -> DG.IDigraph FullyQualifiedType (NE.NonEmpty TypedFunction)
  -> Maybe [DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)]
runQuerySingleResult (src, dst) g = ST.runST $ do
  g' <- DG.thaw g
  runQuerySingleResultST (src, dst) g'

runQuerySingleResultST
  :: (FullyQualifiedType, FullyQualifiedType)
  -> DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction)
  -> ST s (Maybe [DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)])
runQuerySingleResultST (src, dst) =
  querySingleResult (weightCombine (src, dst)) initialWeight src dst

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
  UTF8.decode . BS.unpack . renderComposedFunctions

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

queryAll
  :: forall s v meta.
     ( Ord v
     , Hashable v
     , Show v
     , Show meta
     , Eq meta
     )
  => (Double -> NE.NonEmpty meta -> Double)
  -> Double
  -> v -- ^ src
  -> v -- ^ dst
  -> ([NE.NonEmpty meta] -> String)
  -> Int -- ^ max number of results
  -> DG.Digraph s v (NE.NonEmpty meta)
  -> ST.ST s [[DG.IdxEdge v (NE.NonEmpty meta)]]
queryAll f w src dst disp maxCount graph = fmap (filter $ not . null) $ do
  resultRef <- STM.newSTRef ((0, []), mempty)
  go resultRef
  reverse . snd . fst <$> STM.readSTRef resultRef
  where
    getResult = querySingleResult f w src dst graph

    go resultRef = do
      let whenMissingResults action = do
            ((count', _), _) <- STM.readSTRef resultRef
            unless (count' >= maxCount) action

          accumulateResult res = do
            STM.modifySTRef' resultRef $ \a@((!count', !res'), resultIds) ->
              let pathId' = pathId res
              in if not $ Set.member pathId' resultIds
                then ((count' + 1, res : res'), Set.insert pathId' resultIds)
                else a

            let traceStr = disp (map DG.eMeta res)
            unless (null traceStr) $ traceM traceStr

      mPath <- getResult
      case mPath of
        Nothing -> pure () -- no path
        Just [] -> pure () -- src == dst
        Just path -> do
          accumulateResult path

          forM_ (nonEmptySubsequences path) $ \edges -> do
            whenMissingResults $ do
              forM_ edges (DG.removeEdge graph)
              mRes <- getResult
              forM_ mRes accumulateResult
              forM_ edges (DG.insertEdge graph)

          whenMissingResults $ do
            forM_ path (DG.removeEdge graph)
            go resultRef

    nonEmptySubsequences = tail . subsequences

    pathId = \case
      [] -> []
      path -> DG.eFromIdx (head path) : map DG.eToIdx path

querySingleResult
  :: ( Ord v
     , Hashable v
     , Show v
     , Show meta
     , Eq meta
     )
  => (Double -> NE.NonEmpty meta -> Double)
  -> Double
  -> v
  -> v
  -> DG.Digraph s v (NE.NonEmpty meta)
  -> ST s (Maybe [DG.IdxEdge v (NE.NonEmpty meta)])
querySingleResult f w src dst g =
  Dijkstra.runDijkstra g f w $
    Dijkstra.dijkstraSourceSink (src, dst) >> Dijkstra.pathTo dst

-- ### Specialization

type Vertex = FullyQualifiedType
type Meta = TypedFunction

{-# SPECIALISE
  queryAll
    :: (Double -> NE.NonEmpty Meta -> Double)
    -> Double
    -> Vertex
    -> Vertex
    -> ([NE.NonEmpty Meta] -> String)
    -> Int
    -> DG.Digraph s Vertex (NE.NonEmpty Meta)
    -> ST.ST s [[DG.IdxEdge Vertex (NE.NonEmpty Meta)]] #-}

{-# SPECIALISE
  querySingleResult
    :: (Double -> NE.NonEmpty Meta -> Double)
    -> Double
    -> Vertex
    -> Vertex
    -> DG.Digraph s Vertex (NE.NonEmpty Meta)
    -> ST s (Maybe [DG.IdxEdge Vertex (NE.NonEmpty Meta)]) #-}
