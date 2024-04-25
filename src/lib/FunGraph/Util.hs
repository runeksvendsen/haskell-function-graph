{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module FunGraph.Util
  ( bsToStr
  , typedFunctionsPathTypes
  , idxEdgePathTypes
  , showTypeSig
  , graphFromQueryResult
  , graphToDot
  , putStrFlush
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
import qualified Lucid.Base as Lucid
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Types
import qualified System.IO

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
      -- NOTE: We want to link each type constructor ('Types.FgTyCon') using 'Types.tgTyConHackageSrcUrl', so that e.g. "Maybe Int" will link "Maybe" and "Int" to its own URL. A DOT graph "HTML string" does not support <a> tags, so instead we use a table where the <td> tag contains an href attribute. Lastly, to avoid a border around each <td> element, we set the <table> "border" attribute to zero.
      renderFqtUrlTable fqt = tableNoBorder $ Lucid.tr_ $
        renderFullyQualifiedTypeGeneric
          (Lucid.td_ [] . Lucid.toHtml)
          (\tycon ->
              tdLink
                (Types.tgTyConHackageSrcUrl tycon)
                (Lucid.toHtml $ Types.renderFgTyConQualifiedNoPackage tycon)
          )
          fqt
      tdLink :: T.Text -> Lucid.Html () -> Lucid.Html ()
      tdLink url = Lucid.td_ [Lucid.href_ url, Lucid.target_ "_blank"]
      tableNoBorder = Lucid.table_
        [ Lucid.makeAttribute "BORDER" "0"
        , Lucid.makeAttribute "CELLSPACING" "0"
        ]
      vertexAttributes fqt = Map.fromList
        [ ( "label"
          ,  htmlString (renderFqtUrlTable fqt)
          )
        , ( "tooltip"
          , DG.DotString_DoubleQuoted $ LT.fromStrict . renderFullyQualifiedType $ fqt
          )
        ]

      edgeAttributes fn =
        let edgeToolTip = DG.DotString_DoubleQuoted $ LT.pack $ show $ PrettyTypedFunction fn
        in Map.fromList
          [ ( "label"
            , htmlString $ tableNoBorder $ Lucid.tr_ $ tdLink
                (functionToHackageDocsUrl fn)
                (Lucid.toHtml $ _function_name fn)
            )
          , ( "tooltip", edgeToolTip)
          , ( "labeltooltip", edgeToolTip)
          , ( "edgetooltip", edgeToolTip)
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

-- | Print a string, without trailing newline, to 'System.IO.stdout' and flush the handle afterwards.
--   This makes sure the string is actually printed, instead of waiting for a subsequent newline character.
putStrFlush :: String -> IO ()
putStrFlush str =
  putStr str >> System.IO.hFlush System.IO.stdout