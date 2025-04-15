{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Server.Pages.Search
( handler, HandlerType
, SearchEnv, createSearchEnv
, SearchConfig(..), defaultSearchConfig
  -- * Validation
, VertexInputField -- TODO: how to export kind only?
, InputFieldValidationError(..)
, ValidationError
, renderHandlerError
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
import Server.Api (HxBoosted (HxBoosted), NoGraph (NoGraph), Search)
import Data.String (fromString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Bifunctor (bimap)
import qualified Control.Monad.Except as ET
import Server.HtmlStream
import qualified Streaming.Prelude as S
import qualified Data.BalancedStream
import Control.Monad (when, forM)
import qualified Data.Time.Clock
import qualified Control.Monad.ST
import Data.These (These (..))
import qualified Data.List.NonEmpty as NE
import Data.Functor ((<&>))
import qualified Lucid.Servant
import Data.Data (Proxy (Proxy))
import qualified Server.Pages.Typeahead
import qualified Types

-- | Things we want to precompute when creating the handler
data SearchEnv = SearchEnv
  { searchEnvGraph :: !(FunGraph.Graph ST.RealWorld)
  , searchEnvVertexLookup :: !(T.Text -> Maybe FunGraph.FullyQualifiedType)
  , searchEnvRootHandler :: !((HtmlStream IO (), (Maybe T.Text, Maybe T.Text)) -> HtmlStream IO ())
    -- ^ Needed in case the 'HX-Boosted' header is not present, in which case a full page is returned
  , searchEnvTypeSuggestion :: !(T.Text -> Maybe (NE.NonEmpty FunGraph.FullyQualifiedType))
    -- ^ Find suggestions for the "no such vertex"-error
  }

-- | Search options
data SearchConfig = SearchConfig
  { searchConfigTimeout :: !Data.Time.Clock.NominalDiffTime
  -- ^ Cancel a query after it has run for this long.
  -- Necessary because the worst case running time for a query is huge.
  , searchConfigTrace :: !(Maybe (String -> Control.Monad.ST.ST Control.Monad.ST.RealWorld ()))
  -- ^ Optionally print tracing information for each search query
  , searchConfigSuggestionLimit :: !Word
  -- ^ Display at most this many suggestions in the "no such vertex"-error message.
  --
  -- Must be greater than or equal to 1.
  }

defaultSearchConfig :: SearchConfig
defaultSearchConfig = SearchConfig
  { searchConfigTimeout = 0.1
  , searchConfigTrace = Nothing
  , searchConfigSuggestionLimit = 10
  }

createSearchEnv
  :: ((HtmlStream IO (), (Maybe T.Text, Maybe T.Text)) -> HtmlStream IO ())
  -> FunGraph.Graph ST.RealWorld
  -> (T.Text -> Maybe (NE.NonEmpty FunGraph.FullyQualifiedType))
  -> IO SearchEnv
createSearchEnv mkRootHandler graph typeSuggestion = do
  vertices <- ST.stToIO $ DG.vertexLabels graph
  let hm = HM.fromList $ map (\fqt -> (FunGraph.renderFullyQualifiedType fqt, fqt)) vertices
  pure $ SearchEnv
    { searchEnvGraph = graph
    , searchEnvVertexLookup = (`HM.lookup` hm)
    , searchEnvRootHandler = mkRootHandler
    , searchEnvTypeSuggestion = typeSuggestion
    }

-- ^ /Search/ handler type
type HandlerType ret
  =  Maybe HxBoosted -- ^ 'HX-Boosted' header. 'Just' if present and 'Nothing' if not present.
  -> Maybe T.Text -- ^ src
  -> Maybe T.Text -- ^ dst
  -> Maybe Word -- ^ max number of results
  -> Maybe NoGraph -- ^ if 'Just' then don't draw a graph
  -> Handler ret

type ValidationError =
  (These (InputFieldValidationError 'Src) (InputFieldValidationError 'Dst))

renderHandlerError
  :: SearchEnv
  -> These (InputFieldValidationError 'Src) (InputFieldValidationError 'Dst)
  -> Html ()
renderHandlerError searchEnv = \case
  This err -> renderError "source" err
  That err -> renderError "target" err
  These errSrc errTgt -> do
    renderError "source" errSrc
    renderError "target" errTgt
  where
    renderError :: T.Text -> InputFieldValidationError inputField -> Html ()
    renderError inputName = \case
      InputFieldValidationError_MissingVertex ->
        mkErrorText . toHtml $ "Missing " <> inputName <> " vertex"
      InputFieldValidationError_NoSuchVertex input mkHref ->
        let mSuggestions = searchEnvTypeSuggestion searchEnv input
            renderType fqt =
              mkMonoText $ toHtml $ FunGraph.renderFullyQualifiedType fqt
            mkSuggestion fqt =
              li_ $ a_ [mkHref fqt] (renderType fqt)
        in mkErrorText $
          unwordsHtml $
            [ toHtml $ "No exact match for " <> inputName <> " vertex"
            , mkMonoText (toHtml input) <> "."
            ] ++ fromMaybe []
            ( mSuggestions <&> \suggestions ->
                [ "Did you mean one of the following?"
                , ul_ . mconcat $ map mkSuggestion (NE.toList suggestions)
                ]
            )

-- | Full handler (considers 'HX-Boosted' header)
handler
  :: SearchConfig
  -> SearchEnv
  -> HandlerType (HtmlStream IO ())
handler cfg searchEnv mHxBoosted mSrc mDst mMaxCount mNoGraph = do
  eResult <- handler' cfg searchEnv mHxBoosted mSrc mDst mMaxCount mNoGraph
  -- either the actual result or a human-readable error
  let searchResultHtml = case eResult of
        Right searchResultStream ->
          searchResultStream
        Left err ->
          Server.HtmlStream.streamHtml $ renderHandlerError searchEnv err
  pure $ case mHxBoosted of
    Just HxBoosted ->
      searchResultHtml
    Nothing -> do
      searchEnvRootHandler searchEnv (searchResultHtml, (mSrc, mDst))

-- | Pure Search handler (doesn't consider 'HX-Boosted' header)
handler'
  :: SearchConfig
  -> SearchEnv
  -> HandlerType (Either ValidationError (HtmlStream IO ()))
handler' cfg searchEnv _ mSrc mDst mMaxCount mNoGraph =
  forM validatedVertices $ \(src, mDst') -> do
    page cfg searchEnv src mDst' (fromMaybe defaultLimit mMaxCount) mNoGraph
  where
    defaultLimit = 100 -- TODO: add as HTML input field

    validatedVertices
      :: Either
          (These (InputFieldValidationError 'Src) (InputFieldValidationError 'Dst))
            (FunGraph.FullyQualifiedType, Maybe FunGraph.FullyQualifiedType)
    validatedVertices =
      let render = Server.Pages.Typeahead.renderSearchValue
          mDst_ = if mDst == Just "" then Nothing else mDst
          resSrc = vertexIsPresent mSrc >>= validateVertex (\src -> mkHref' (Just $ render src) mDst)
          mResDst = traverse (validateVertex (\dst -> mkHref' mSrc (Just $ render dst))) mDst_
      in case (resSrc, mResDst) of
        (Right src, Right mDst') -> Right (src, mDst')
        (Left err, Right _) -> Left $ This err
        (Right _, Left err) -> Left $ That err
        (Left err1, Left err2) -> Left $ These err1 err2

    vertexIsPresent
      :: Maybe T.Text
      -> Either (InputFieldValidationError inputField) T.Text
    vertexIsPresent Nothing = Left InputFieldValidationError_MissingVertex
    vertexIsPresent (Just vertex) = Right vertex

    validateVertex
      :: (FunGraph.FullyQualifiedType -> Attribute)
      -> T.Text
      -> Either (InputFieldValidationError inputField) FunGraph.FullyQualifiedType
    validateVertex mkHref txt =
      let lookupVertex = searchEnvVertexLookup searchEnv
      in maybe
        (Left $ InputFieldValidationError_NoSuchVertex txt mkHref)
        Right
        (lookupVertex txt)

    mkHref' :: Maybe T.Text -> Maybe T.Text -> Attribute -- src -> dst -> attr
    mkHref' =
      let api = Proxy :: Proxy Server.Api.Search
          mkLink = Lucid.Servant.safeHref_ "/" api api
      in \mSrc' mDst' -> mkLink mSrc' mDst' mMaxCount mNoGraph

type StreamElem = ([FunGraph.NonEmpty FunGraph.TypedFunction], Double)

page
  :: SearchConfig
  -> SearchEnv
  -> FunGraph.FullyQualifiedType
  -> Maybe FunGraph.FullyQualifiedType
  -> Word
  -> Maybe NoGraph
  -> Handler (HtmlStream IO ())
page cfg searchEnv src mDst maxCount' mNoGraph = do
  eQueryResultStream <- liftIO $ query' (src, mDst)
  queryResultStream <- either
    (internalError . missingVertexError)
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
                      when (isNothing mDst) $
                        td_ "Result type"
                      td_ "Dependencies"
                  rows
            mkTableRow :: ([FunGraph.TypedFunction], Word) -> Html ()
            mkTableRow (result, resultNumber) =
                tr_ $ do
                  td_ $ renderResult (result, resultNumber)
                  when (isNothing mDst) $
                    td_ $ mkMonoText $ toHtml $
                      maybe
                        ""
                        mkTypeLink
                        (FunGraph.retComposedFunctions result)
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
  pure $ do
    (timedOut, accum) <- resultsTable
    when timedOut $
      streamHtml timedOutText
    when (null accum) $
      streamHtml noResultsText
    resultGraph accum
  where
    maxCount = fromIntegral maxCount'

    internalError errText =
      throwError $ err500 { errBody = "Internal error: " <> errText }

    missingVertexError (FunGraph.GraphActionError_NoSuchVertex v) = BSL.unwords
      [ "Query returned 'no such vertex' error for vertex:"
      , fromString $ show v
      , "but we have the vertices right here:"
      , fromString (show $ bimap FunGraph.renderFullyQualifiedType (fmap FunGraph.renderFullyQualifiedType) (src, mDst)) <> "."
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

    noResultsText =
      let mkToVertexText dst =
            [ " to "
            , mkMonoText $ toHtml $ FunGraph.renderFullyQualifiedType dst
            ]
      in mkErrorText $ mconcat $
        [ "No results found. No path from "
        , mkMonoText $ toHtml $ FunGraph.renderFullyQualifiedType src
        ] ++ maybe [] mkToVertexText mDst ++ ["."]

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
        (mkMonoText $ toHtml $ FunGraph.fgPackageName fnPkg)

    mkTypeLink = mkMonoText .
      FunGraph.renderFullyQualifiedTypeGeneric
        toHtml
        (\tycon ->
            a_
              [href_ $ Types.tgTyConHackageSrcUrl tycon, target_ "_blank"]
              (toHtml $ Types.fgTyConName tycon)
        )

    queryTreeTimeoutIO =
      maybe
        FunGraph.queryTreeTimeoutIO
        FunGraph.queryTreeTimeoutIOTrace
        (searchConfigTrace cfg)

    query' srcDst =
      ET.runExceptT $
          queryTreeTimeoutIO
            (searchEnvGraph searchEnv)
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
                  (mkMonoText $ toHtml $ FunGraph.renderFunctionNoPackage fn)
            in functionNameWithLink `with` [title_ typeSig]
      in div_ [mkResultAttribute (T.pack $ show resultNumber)] $
          mconcat $ intersperse (mkMonoText " . ") $ map renderSingleFn (reverse fns)

mkErrorText :: Html () -> Html ()
mkErrorText = p_ [style_ "color:red"]

mkMonoText :: Html () -> Html ()
mkMonoText =
  let style = style_ $ T.intercalate "; "
        [ "font-family: monospace, monospace"
        , "background-color: rgb(200, 200, 200)"
        ]
  in span_ [style]

unwordsHtml
  :: [Html ()]
  -> Html ()
unwordsHtml =
  mconcat . intersperse (toHtml (" " :: T.Text))

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

data VertexInputField
  = Src
  | Dst

data InputFieldValidationError (inputField :: VertexInputField)
  = InputFieldValidationError_MissingVertex
  | InputFieldValidationError_NoSuchVertex
      T.Text
      -- ^ the unknown vertex name
      (FunGraph.FullyQualifiedType -> Attribute)
      -- ^ construct a link (as an 'href' attribute) to the /search page from a suggestion
