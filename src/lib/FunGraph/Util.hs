{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module FunGraph.Util
  ( bsToStr
  , typedFunctionsPathTypes
  , idxEdgePathTypes
  , showTypeSig
  , graphFromQueryResult
  , graphToDot
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
import qualified Lucid
import qualified Data.Map.Strict as Map
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
showTypeSig :: [FullyQualifiedType] -> T.Text
showTypeSig =
  T.intercalate " -> " . map renderFullyQualifiedType

bsToStr :: BSC8.ByteString -> String
bsToStr = UTF8.decode . BS.unpack

graphToDot
  :: BSC8.ByteString
  -> DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction)
  -> ST s LT.Text
graphToDot name =
  let bsToLT = LT.fromStrict . TE.decodeUtf8
      -- use 'lucid' to escape what needs escaping
      htmlString html = DG.DotString_Raw $ "< " <> Lucid.renderText html <> " >"
      vertexAttributes fqt = Map.fromList
        [ ( "label"
          ,  htmlString (Lucid.b_ $ Lucid.toHtml $ renderFullyQualifiedTypeNoPackage fqt)
          )
        , ( "tooltip"
          , DG.DotString_DoubleQuoted $ LT.fromStrict . renderFullyQualifiedType $ fqt
          )
        ]

      edgeAttributes fn = Map.fromList
        [ ( "label"
          , DG.DotString_DoubleQuoted $ LT.fromStrict $ _function_name fn
          )
        , ( "labeltooltip"
          , DG.DotString_DoubleQuoted $ LT.pack $ show $ PrettyTypedFunction fn
          )
        ]

  in DG.graphToDotMulti
    vertexAttributes
    (edgeAttributes . DG.eMeta)
    (DG.DotString_DoubleQuoted $ bsToLT name)

-- | Build a graph from the output of 'FunGraph.runQueryTreeST'
graphFromQueryResult
  :: [([NE.NonEmpty TypedFunction], Double)]
  -> ST s (DG.Digraph s FullyQualifiedType (NE.NonEmpty TypedFunction))
graphFromQueryResult res = do
  DG.fromEdgesMulti $ resultToEdgeSet res
  where
    resultToEdgeSet =
      Set.fromList . concatMap NE.toList . concatMap fst
