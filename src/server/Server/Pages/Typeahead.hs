{-# LANGUAGE OverloadedStrings #-}
module Server.Pages.Typeahead
( mkHandler
)
where

import qualified Data.PrioTrie
import Lucid
import Control.Monad (forM_, forM)
import qualified Data.Text as T
import Servant.Server
import qualified FunGraph
import Control.Monad.ST (stToIO)
import qualified Data.Graph.Digraph as DG
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import Control.Monad.Except (throwError)
import qualified Data.Text.Encoding as TE
import qualified Control.Exception as Ex
import qualified Control.DeepSeq
import qualified Control.Monad.ST as ST

mkHandler
  :: Maybe Word -- ^ Limit the number of typeahead suggestions. NB: Must be greater than zero if present.
  -> FunGraph.Graph ST.RealWorld
  -> IO ( Maybe T.Text -> Maybe T.Text -> Handler (Html ())
        , Html ()
        ) -- ^ (handler, initial suggestions)
mkHandler mLimit graph = do
  mPrioTrie <- mkPrioTrie mLimit graph
  prioTrie <- maybe (fail "empty input graph in Typeahead handler") pure mPrioTrie
  let initialSuggestions = suggestions prioTrie ""
  pure (handler prioTrie, initialSuggestions)

mkPrioTrie
  :: Maybe Word -- ^ Limit the number of typeahead suggestions. NB: Must be greater than zero if present.
  -> FunGraph.Graph ST.RealWorld
  -> IO (Maybe (Data.PrioTrie.PrioTrie Word FunGraph.FullyQualifiedType))
     -- ^ 'Nothing' if the input graph is empty
mkPrioTrie mLimit graph = do
  countList <- stToIO $ do
    (outMap, inMap) <- DG.outgoingIncomingCount graph -- TODO: instead, count the number of different packages that export functions which operate on a given type (to avoid a single package exporting a large number of functions operating on a type being rated highly). Use 'DG.foldEdges' to implement this.
    let countMap = Map.unionWith (+) outMap inMap
    mapM (traverse (lookupVertexId graph) . swap) (Map.toList countMap)
  let mTypeaheadData :: Maybe (NE.NonEmpty (BS.ByteString, (Word, FunGraph.FullyQualifiedType)))
      mTypeaheadData = NE.nonEmpty $ countList <&> \(count, fqt) ->
        (TE.encodeUtf8 $ FunGraph.renderFullyQualifiedTypeUnqualified fqt, (count, fqt))
  forM mTypeaheadData $ \typeaheadData ->
  -- typeaheadData <- maybe (fail "Empty graph in Typeahead handler") pure mTypeaheadData
    Ex.evaluate $ Control.DeepSeq.force $
      -- I believe deeply evaluating the PrioTrie is necessary because its very purpose is pre-computing stuff, ie. _not_ postponing evaluation until its result is demanded. For example, the sorted lists inside the PrioTrie must not be thunks, because that would mean we postpone sorting until a request demands it (which introduces unwanted latency for the first request that forces evaluation of a particular sorted list).
      Data.PrioTrie.fromList limit typeaheadData
  where
    lookupVertexId g vid = do
      DG.lookupVertexId g vid >>=
        maybe (fail $ "BUG: VertexId not found: " <> show vid) pure

    limit = maybe id (\l -> NE.fromList . NE.take (fromIntegral l)) mLimit

handler
  :: Data.PrioTrie.PrioTrie Word FunGraph.FullyQualifiedType
  -> Maybe T.Text
  -> Maybe T.Text
  -> Handler (Html ())
handler prioTrie mSrc mDst =
  case (mSrc, mDst) of
    (Just src, Nothing) -> pure $ suggestions prioTrie src
    (Nothing, Just dst) -> pure $ suggestions prioTrie dst
    (_, _) -> throwError $ err400 { errBody = "Missing 'src' or 'dst' query param" }

suggestions
  :: Data.PrioTrie.PrioTrie Word FunGraph.FullyQualifiedType
  -> T.Text
  -> Html ()
suggestions prioTrie prefix = do
  forM_ mSuggestions $ \suggestionsLst ->
    forM_ suggestionsLst $ \(_, fqt) ->
      option_
        [ value_ $ FunGraph.renderFullyQualifiedType fqt
        , label_ $ FunGraph.renderFullyQualifiedTypeNoPackage fqt
        ]
        ""
  where
    mSuggestions = Data.PrioTrie.prefixLookup prioTrie (TE.encodeUtf8 prefix)
