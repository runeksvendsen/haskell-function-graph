{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Server.Pages.Search
( handler, HandlerType
, SearchEnv, createSearchEnv
, SearchConfig(..), defaultSearchConfig
  -- * Testing/benchmarking
, mkResultAttribute
)
where

import Lucid
import qualified Data.Text as T
import Servant.Server
import qualified FunGraph
import Data.List (intersperse)
import Data.Containers.ListUtils (nubOrd)
import qualified FunGraph.Util as Util
import qualified Server.GraphViz
import qualified Control.Monad.ST as ST
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.HashMap.Strict as HM
import qualified Data.Graph.Digraph as DG
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TLE
import Server.Api (HxBoosted, NoGraph (NoGraph))
import Data.String (fromString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Bifunctor (bimap)
import qualified Control.Monad.Except as ET
import Server.HtmlStream
import qualified Streaming.Prelude as S
import qualified Data.BalancedStream
import Control.Monad (when)
import qualified Data.Time.Clock
import qualified Control.Monad.ST
import qualified Data.List.NonEmpty as NE

-- | Things we want to precompute when creating the handler
data SearchEnv = SearchEnv
  { searchEnvGraph :: !(FunGraph.Graph ST.RealWorld)
  , searchEnvVertexLookup :: T.Text -> Maybe FunGraph.FullyQualifiedType
  }

-- | Search options
data SearchConfig = SearchConfig
  { searchConfigTimeout :: !Data.Time.Clock.NominalDiffTime
  -- ^ Cancel a query after it has run for this long.
  -- Necessary because the worst case running time for a query is huge.
  , searchConfigTrace :: !(Maybe (String -> Control.Monad.ST.ST Control.Monad.ST.RealWorld ()))
  -- ^ Optionally print tracing information for each search query
  }

defaultSearchConfig :: SearchConfig
defaultSearchConfig = SearchConfig
  { searchConfigTimeout = 0.1
  , searchConfigTrace = Nothing
  }

createSearchEnv
  :: FunGraph.Graph ST.RealWorld
  -> IO SearchEnv
createSearchEnv graph = do
  vertices <- ST.stToIO $ DG.vertexLabels graph
  let hm = HM.fromList $ map (\fqt -> (FunGraph.renderFullyQualifiedType fqt, fqt)) vertices
  pure $ SearchEnv
    { searchEnvGraph = graph
    , searchEnvVertexLookup = (`HM.lookup` hm)
    }

-- ^ /Search/ handler type
type HandlerType ret
  =  Maybe HxBoosted -- ^ 'HX-Boosted' header. 'Just' if present and 'Nothing' if not present.
  -> Maybe T.Text -- ^ src
  -> Maybe T.Text -- ^ dst
  -> Maybe Word -- ^ max number of results
  -> Maybe NoGraph -- ^ if 'Just' then don't draw a graph
  -> ET.ExceptT (NE.NonEmpty ValidationError) Handler ret

data ValidationError
  = ValidationError_NoSuchVertex T.Text
  | ValidationError_MissingSrcVertex
  | ValidationError_MissingDstVertex

handler
  :: SearchConfig
  -> SearchEnv
  -> HandlerType (HtmlStream IO (), (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType)) -- ^ (html, (src, dst))
handler cfg searchEnv _ mSrc mDst mMaxCount mNoGraph =
  case (mSrc, mDst) of

    page cfg searchEnv src dst (fromMaybe defaultLimit mMaxCount) mNoGraph
  throwError $ err400 { errBody = "Missing 'src' and/or 'dst' query param" }
  where
    defaultLimit = 100 -- TODO: add as HTML input field

type StreamElem = ([FunGraph.NonEmpty FunGraph.TypedFunction], Double)

page
  :: SearchConfig
  -> SearchEnv
  -> T.Text
  -> T.Text
  -> Word
  -> Maybe NoGraph
  -> ET.ExceptT (NE.NonEmpty ValidationError) Handler (HtmlStream IO (), (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType)) -- ^ (html, (src, dst))
page cfg (SearchEnv graph lookupVertex) srcTxt dstTxt maxCount' mNoGraph = do
  src <- lookupVertexM srcTxt
  dst <- lookupVertexM dstTxt
  eQueryResultStream <- liftIO $ query' (src, dst)
  queryResultStream <- either
    (ET.lift . internalError . mkMissingVertexError (src, dst))
    pure
    eQueryResultStream
  let queryResultStreamWithAccum
        :: S.Stream (S.Of StreamElem) IO (Bool, [StreamElem]) -- Return value: (timedOut, streamed elements)
      queryResultStreamWithAccum =
        Data.BalancedStream.appendStreamAccum
          (\mTimeout accum -> pure (Data.Maybe.isNothing mTimeout, accum))
          queryResultStream
  let queryResultPaths = S.map fst $
        FunGraph.queryResultTreeToPathsStream queryResultStreamWithAccum
  let resultsTable :: HtmlStream IO (Bool, [StreamElem])
      resultsTable = do
        let mkTable
              :: Monad m
              => HtmlStream m a
              -> HtmlStream m a
            mkTable rows =
              streamTagBalancedM "table" $ do
                streamTagBalancedM "tbody" $ do
                  streamHtml $ thead_ $
                    tr_ $ do
                      td_ "Function composition"
                      td_ "Dependencies"
                  rows
            mkTableRow :: ([FunGraph.TypedFunction], Word) -> Html ()
            mkTableRow (result, resultNumber) =
                tr_ $ do
                  td_ $ renderResult (result, resultNumber)
                  td_ $
                    mconcat $
                      intersperse ", " $
                        map mkPackageLink (nubOrd $ map FunGraph._function_package result)
            queryResultPathsWithResultNumber =
              S.zip queryResultPaths (S.enumFrom 1)
            renderTableWithRows
              :: Bool -- Denotes whether this the first result
              -> S.Stream (S.Of ([FunGraph.TypedFunction], Word)) IO a
              -> HtmlStream IO a
            renderTableWithRows isFirstResult s = do
              ET.lift (S.next s) >>= \case
                Left r -> return r
                Right (result, s') -> do
                  let tableRow = streamHtml (mkTableRow result) >> renderTableWithRows False s'
                  if isFirstResult then mkTable tableRow else tableRow
        renderTableWithRows True queryResultPathsWithResultNumber
      resultGraph accum =
        if null accum then mempty else ET.lift (mkGraph accum) >>= streamHtml
  pure $ (, (src, dst)) $ do
    (timedOut, accum) <- resultsTable
    when timedOut $
      streamHtml timedOutText
    when (null accum) $
      streamHtml $ noResultsText (src, dst)
    resultGraph accum
  where
    maxCount = fromIntegral maxCount'

    mkErrorText = p_ [style_ "color:red"]
    internalError errText =
      throwError $ err500 { errBody = "Internal error: " <> errText }

    mkMissingVertexError (src, dst) (FunGraph.GraphActionError_NoSuchVertex v) = BSL.unwords
      [ "Query returned 'no such vertex' error for vertex:"
      , fromString $ show v
      , "but we have the vertices right here:"
      , fromString (show $ bimap FunGraph.renderFullyQualifiedType FunGraph.renderFullyQualifiedType (src, dst)) <> "."
      , "Please report bug at https://github.com/runeksvendsen/haskell-function-graph/issues."
      ]

    mkGraph
      :: ET.MonadIO m
      => [StreamElem]
      -> m (Html ())
    mkGraph queryResult = case mNoGraph of
      Just NoGraph -> pure mempty
      Nothing -> do
        resultGraphE <- liftIO $ renderResultGraphIO queryResult
        either
          (\err -> liftIO $ putStrLn $ "ERROR: Failed to render result graph: " <> err)
          (const $ pure ())
          resultGraphE
        pure $ do
          h3_ "Result graph"
          let addSvgElemId = T.replace "<svg " ("<svg id=\"" <> svgGraphId <> "\" ") -- hacky way to add an "id" attribute to the SVG graph
              svgGraphId = "graph"
          either
            (const $ mkErrorText "Failed to render result graph")
            (toHtmlRaw . addSvgElemId) -- 'toHtmlRaw' because 'resultGraph' contains tags we don't want escaped
            resultGraphE
          openSvgInNewWindowBtn svgGraphId

    noResultsText :: (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType) -> Html ()
    noResultsText (src, dst) =
      mkErrorText $ mconcat
        [ "No results found. No path from "
        , mono $ toHtml $ FunGraph.renderFullyQualifiedType src
        , " to "
        , mono $ toHtml $ FunGraph.renderFullyQualifiedType dst
        , "."
        ]

    timedOutText :: Html ()
    timedOutText =
      mkErrorText $ mconcat
        [ "Query timed out! Query did not terminate within the time limit of "
        , toHtml $ show timeout
        ]

    mkPackageLink fnPkg =
      a_
        [ href_ $ "https://hackage.haskell.org/package/" <> FunGraph.renderFgPackage fnPkg
        , target_ "_blank"
        ]
        (mono $ toHtml $ FunGraph.fgPackageName fnPkg)

    -- TODO: don't throw 404; display HTML message and populate suggestions
    lookupVertexM txt =
      maybe
        (throwError $ ValidationError_NoSuchVertex txt)
        pure
        (lookupVertex txt)

    queryTreeTimeoutIO =
      maybe
        FunGraph.queryTreeTimeoutIO
        FunGraph.queryTreeTimeoutIOTrace
        (searchConfigTrace cfg)

    query' srcDst =
      ET.runExceptT $
          queryTreeTimeoutIO
            graph
            timeout
            maxCount
            srcDst

    timeout = searchConfigTimeout cfg

    renderResultGraphIO queryResult =
      let
        resultDotGraph =
          Util.graphFromQueryResult queryResult
            >>= Util.graphToDot ""
      in ST.stToIO resultDotGraph
        >>= Server.GraphViz.renderDotGraph

    renderResult :: ([FunGraph.TypedFunction], Word) -> Html ()
    renderResult (fns, resultNumber) =
      let renderSingleFn fn =
            let (fromTy, toTy) = FunGraph.typedFunctionFromToTypes fn
                typeSig = T.unwords
                  [ FunGraph.renderFunction fn
                  , "::"
                  , FunGraph.fullyQualifiedTypeToText fromTy
                  , "->"
                  , FunGraph.fullyQualifiedTypeToText toTy
                  ]
                functionNameWithLink :: Html ()
                functionNameWithLink = a_
                  [href_ $ FunGraph.functionToHackageDocsUrl fn, target_ "_blank"]
                  (mono $ toHtml $ FunGraph.renderFunctionNoPackage fn)
            in functionNameWithLink `with` [title_ typeSig]
      in div_ [mkResultAttribute (T.pack $ show resultNumber)] $
          mconcat $ intersperse (mono " . ") $ map renderSingleFn (reverse fns)

    mono =
      let style = style_ $ T.intercalate "; " $
            [ "font-family: monospace, monospace"
            , "background-color: rgb(200, 200, 200)"
            ]
      in span_ [style]

-- | Exported for benchmarking purposes: marks the actual results in the HTML so we can benchmark e.g. "time to first result"
mkResultAttribute :: T.Text -> Attribute
mkResultAttribute = data_ "result-number"

openSvgInNewWindowBtn :: T.Text -> Html ()
openSvgInNewWindowBtn svgGraphId = do
  button_
    [ id_ btnId
    , style_ "visibility:hidden; display:none;" -- Hide the button if JS is disabled
    ]
    "Open graph in new window"
  -- Source: https://stackoverflow.com/a/64512427/700597
  toHtmlRaw $ T.unlines
    [ "<script>"
    , " // make button visible in case JS is enabled"
    , " var btnElem = document.getElementById(\"" <> btnId <> "\");"
    , " btnElem.style.display = \"block\";"
    , " btnElem.style.visibility = \"visible\";"
    , " // open SVG in new window on click"
    , " btnElem.onclick = (evt) => {"
    , "  const svg = document.getElementById(\"" <> svgGraphId <> "\");"
    , "  const as_text = new XMLSerializer().serializeToString(svg);"
    , "  const blob = new Blob([as_text], { type: \"image/svg+xml\" });"
    , "  const url = URL.createObjectURL(blob);"
    , "  const win = open(url);"
    , "  win.onload = (evt) => URL.revokeObjectURL(url);"
    , " };"
    , "</script>"
    ]
  where
    btnId = "open_svg_in_new_window"
