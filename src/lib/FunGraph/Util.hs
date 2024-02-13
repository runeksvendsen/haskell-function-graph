{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module FunGraph.Util
  ( bsToStr
  , typedFunctionsPathTypes
  , idxEdgePathTypes
  , showTypeSig
  , graphFromQueryResult
  , graphToDot
  , graphToDotGraphviz
  , graphVizRender
  -- * Re-exports
  , Data.GraphViz.Commands.GraphvizCommand(..)
  , Data.GraphViz.Commands.GraphvizOutput(..)
  )
  where

import FunGraph.Types
import qualified Json
import qualified Data.Graph.Digraph as DG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Control.Monad.ST (ST)
import qualified Data.Set as Set
import qualified Data.GraphViz.Types
import qualified Data.GraphViz.Types.Generalised
import qualified Data.GraphViz.Commands
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

-- | Convert sequence of adjacent edges to vertices moved through
toPathTypes
  :: (edge -> FullyQualifiedType) -- ^ get the edge's @to@/@dst@ vertex
  -> (edge -> FullyQualifiedType) -- ^ get the edge's @from@/@src@ vertex
  -> [edge] -- ^ list of edges e1...eN where the destination of edge eN is the source of edge e(N+1)
  -> [FullyQualifiedType] -- ^ list of vertices passed through
toPathTypes getTo getFrom = \case
  pathTo@(firstEdge:_) ->
    getFrom firstEdge : map getTo pathTo
  [] -> []

-- | 'toPathTypes' specialized to 'TypedFunction'
typedFunctionsPathTypes :: [TypedFunction] -> [FullyQualifiedType]
typedFunctionsPathTypes = toPathTypes
  (Json.functionType_ret . _function_typeSig)
  (Json.functionType_arg . _function_typeSig)

-- | 'toPathTypes' specialized to 'DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)'
idxEdgePathTypes
  :: [DG.IdxEdge FullyQualifiedType (NE.NonEmpty TypedFunction)]
  -> [FullyQualifiedType]
idxEdgePathTypes = toPathTypes DG.eTo DG.eFrom

-- | Show vertices moved through
showTypeSig :: [FullyQualifiedType] -> BS.ByteString
showTypeSig =
  BS.intercalate " -> " . map unFullyQualifiedType

bsToStr :: BSC8.ByteString -> String
bsToStr = UTF8.decode . BS.unpack

graphToDot
  :: BSC8.ByteString
  -> DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction)
  -> ST s LT.Text
graphToDot name =
  let bsToLT = LT.fromStrict . TE.decodeUtf8
  in DG.graphToDotMulti
    (bsToLT . unFullyQualifiedType)
    (bsToLT . _function_name . DG.eMeta)
    (bsToLT name)

graphToDotGraphviz
  :: BSC8.ByteString
  -> DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction)
  -> ST s (Data.GraphViz.Types.Generalised.DotGraph LT.Text)
graphToDotGraphviz name =
  fmap Data.GraphViz.Types.parseDotGraph . graphToDot name

graphVizRender
  :: Data.GraphViz.Commands.GraphvizCommand -- ^ Layout
  -> Data.GraphViz.Commands.GraphvizOutput -- ^ Output format
  -> Data.GraphViz.Types.Generalised.DotGraph LT.Text -- ^ Actual graph
  -> IO T.Text
graphVizRender graphvizCommand graphvizOutput g =
  Data.GraphViz.Commands.graphvizWithHandle
    graphvizCommand
    g
    graphvizOutput
    extractFromHandle
  where
    extractFromHandle = TIO.hGetContents

-- | Build a graph from the output of 'FunGraph.runQueryTreeST'
graphFromQueryResult
  :: [([NE.NonEmpty TypedFunction], Double)]
  -> ST s (DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction))
graphFromQueryResult res = do
  DG.fromEdgesMulti $ resultToEdgeSet res
  where
    resultToEdgeSet =
      Set.fromList . concatMap NE.toList . concatMap fst
