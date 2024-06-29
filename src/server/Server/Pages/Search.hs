{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Pages.Search
( page
, handler, HandlerType
, SearchEnv, createSearchEnv
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
import Data.Maybe (fromMaybe)
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
import qualified Control.Concurrent as ConcTmp -- WIP

-- | Things we want to precompute when creating the handler
data SearchEnv = SearchEnv
  { searchEnvGraph :: !(FunGraph.Graph ST.RealWorld)
  , searchEnvVertexLookup :: T.Text -> Maybe FunGraph.FullyQualifiedType
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
  -> Handler ret

handler
  :: SearchEnv
  -> HandlerType (HtmlStream IO (), (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType)) -- ^ (html, (src, dst))
handler searchEnv _ (Just src) (Just dst) mMaxCount mNoGraph =
  let defaultLimit = 100 -- TODO: add as HTML input field
  in do
    page searchEnv src dst (fromMaybe defaultLimit mMaxCount) mNoGraph
handler _ _ _ _ _ _ =
  throwError $ err400 { errBody = "Missing 'src' and/or 'dst' query param" }

page
  :: SearchEnv
  -> T.Text
  -> T.Text
  -> Word
  -> Maybe NoGraph
  -> Handler (HtmlStream IO (), (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType)) -- ^ (html, (src, dst))
page (SearchEnv graph lookupVertex) srcTxt dstTxt maxCount' mNoGraph = do
  src <- lookupVertexM srcTxt
  dst <- lookupVertexM dstTxt
  eQueryResultStream <- liftIO $ query' (src, dst)
  queryResultStream <- either
    (internalError . mkMissingVertexError (src, dst))
    pure
    eQueryResultStream
  queryResultStreamWithAccum <- liftIO $ Data.BalancedStream.returnStreamAccum
    (S.take maxCount queryResultStream)
  let queryResultPaths =
        FunGraph.queryResultTreeToPathsStream queryResultStreamWithAccum
  let resultsTable :: HtmlStream IO [([FunGraph.NonEmpty FunGraph.TypedFunction], Double)]
      resultsTable = do
        streamTagBalancedM "table" $ do
          streamHtml $ thead_ $
            tr_ $ do
              td_ "Function composition"
              td_ "Dependencies"
          streamTagBalancedM "tbody" $ do
            let f :: [FunGraph.TypedFunction] -> Html ()
                f result =
                    tr_ $ do
                      td_ $ renderResult result
                      td_ $
                        mconcat $
                          intersperse ", " $
                            map mkPackageLink (nubOrd $ map FunGraph._function_package result)
            liftStream $ S.map (f . fst) queryResultPaths
      resultGraph accum =
        ET.lift (mkGraph accum) >>= streamHtml
  -- WIP: check timeout
  -- let resultHtml = if S.maps null queryResultPaths then streamHtml $ noResultsText (src, dst) else resultHtml'
  pure (resultsTable >>= resultGraph, (src, dst))
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
      => [([FunGraph.NonEmpty FunGraph.TypedFunction], Double)]
      -> m (Html ())
    mkGraph queryResult = liftIO (ConcTmp.threadDelay 5000000) >> case mNoGraph of -- WIP: debug
      Just NoGraph -> pure mempty
      Nothing -> do
        resultGraphE <- liftIO $ renderResultGraphIO queryResult
        either
          (\err -> liftIO $ putStrLn $ "ERROR: Failed to render result graph: " <> err)
          (const $ pure ())
          resultGraphE
        pure $ do
          h3_ "Result graph"
          either
            (const $ mkErrorText "Failed to render result graph")
            toHtmlRaw -- 'toHtmlRaw' because 'resultGraph' contains tags we don't want escaped
            resultGraphE
          openSvgInNewWindowBtn

    noResultsText :: (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType) -> Html ()
    noResultsText (src, dst) =
      mkErrorText $ mconcat
        [ "No results found. No path from "
        , mono $ toHtml $ FunGraph.renderFullyQualifiedType src
        , " to "
        , mono $ toHtml $ FunGraph.renderFullyQualifiedType dst
        , "."
        ]

    mkPackageLink fnPkg =
      a_
        [ href_ $ "https://hackage.haskell.org/package/" <> FunGraph.renderFgPackage fnPkg
        , target_ "_blank"
        ]
        (mono $ toHtml $ FunGraph.fgPackageName fnPkg)

    lookupVertexM txt =
      maybe
        (throwError $ err404 { errBody = "Type not found: " <> TLE.encodeUtf8 (LT.fromStrict txt) })
        pure
        (lookupVertex txt)

    query' srcDst =
      ET.runExceptT $
          FunGraph.queryTreeTimeoutIO
            graph
            0.1 -- WIP: don't hardcode
            maxCount
            srcDst

    renderResultGraphIO queryResult =
      let
        resultDotGraph =
          Util.graphFromQueryResult queryResult
            >>= Util.graphToDot ""
      in ST.stToIO resultDotGraph
        >>= Server.GraphViz.renderDotGraph

    renderResult :: [FunGraph.TypedFunction] -> Html ()
    renderResult fns =
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
      in mconcat $ intersperse (mono " . ") $ map renderSingleFn (reverse fns)

    mono =
      let style = style_ $ T.intercalate "; " $
            [ "font-family: monospace, monospace"
            , "background-color: rgb(200, 200, 200)"
            ]
      in span_ [style]

openSvgInNewWindowBtn :: Html ()
openSvgInNewWindowBtn = do
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
    , "  const svg = document.querySelector(\"svg\");"
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
