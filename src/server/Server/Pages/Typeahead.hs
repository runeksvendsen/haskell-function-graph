{-# LANGUAGE OverloadedStrings #-}
module Server.Pages.Typeahead
( mkHandler, HandlerType
, suggestionOption_
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
import qualified Data.Set as Set
import qualified Data.Foldable

-- ^ /Typeahead/ handler type
type HandlerType
  =  Maybe T.Text
  -> Maybe T.Text
  -> Handler (Html ())

mkHandler
  :: Maybe Word -- ^ Limit the number of typeahead suggestions. NB: Must be greater than zero if present.
  -> FunGraph.Graph ST.RealWorld
  -> IO ( HandlerType, Html ()) -- ^ (handler, initial suggestions)
mkHandler mLimit graph = do
  mPrioTrie <- mkPrioTrie mLimit graph
  prioTrie <- maybe (fail "empty input graph in Typeahead handler") pure mPrioTrie
  let initialSuggestions = suggestions prioTrie ""
  pure (handler prioTrie, initialSuggestions)

-- | For each vertex (type), count the number of different packages that export a function which operates on this vertex (type).
calculatePriorities
  :: DG.Digraph s FunGraph.FullyQualifiedType (NE.NonEmpty FunGraph.TypedFunction)
  -> ST.ST s (Map.Map DG.VertexId Word)
calculatePriorities graph =
    fmap (fromIntegral . Set.size) <$> DG.foldEdges
      graph
      (\srcVid dstVid edge map' -> pure $ addPackagesToSet srcVid edge (addPackagesToSet dstVid edge map'))
      Map.empty
  where
    addPackagesToSet
      :: DG.VertexId
      -> DG.IdxEdge FunGraph.FullyQualifiedType (NE.NonEmpty FunGraph.TypedFunction)
      -> Map.Map DG.VertexId (Set.Set (FunGraph.FgPackage T.Text))
      -> Map.Map DG.VertexId (Set.Set (FunGraph.FgPackage T.Text))
    addPackagesToSet vid edge map' =
      let f :: DG.IdxEdge FunGraph.FullyQualifiedType (NE.NonEmpty FunGraph.TypedFunction) -> Set.Set (FunGraph.FgPackage T.Text)
          f = Set.fromList . Data.Foldable.toList . NE.map FunGraph._function_package . DG.eMeta
      in Map.alter (Just . maybe (f edge) (<> f edge)) vid map'

mkPrioTrie
  :: Maybe Word -- ^ Limit the number of typeahead suggestions. NB: Must be greater than zero if present.
  -> FunGraph.Graph ST.RealWorld
  -> IO (Maybe (Data.PrioTrie.PrioTrie Word FunGraph.FullyQualifiedType))
     -- ^ 'Nothing' if the input graph is empty
mkPrioTrie mLimit graph = do
  priorityList <- stToIO $ do
    priorityMap <- calculatePriorities graph
    mapM (traverse (lookupVertexId graph) . swap) (Map.toList priorityMap)
  let mTypeaheadData :: Maybe (NE.NonEmpty (BS.ByteString, (Word, FunGraph.FullyQualifiedType)))
      mTypeaheadData = NE.nonEmpty $ priorityList <&> \(count, fqt) ->
        (TE.encodeUtf8 $ FunGraph.renderFullyQualifiedTypeUnqualified fqt, (count, fqt))
  forM mTypeaheadData $ \typeaheadData ->
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
    forM_ suggestionsLst $ \(_, fqt) -> suggestionOption_ [] fqt
  where
    mSuggestions = Data.PrioTrie.prefixLookup prioTrie (TE.encodeUtf8 prefix)

suggestionOption_ :: [Attribute] -> FunGraph.FullyQualifiedType -> Html ()
suggestionOption_ extraAttrs fqt =
  option_
    ([ value_ $ FunGraph.renderFullyQualifiedType fqt
    , label_ $ FunGraph.renderFullyQualifiedTypeNoPackage fqt
    ] ++ extraAttrs)
    ""
