{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Pages.Search
( page
, handler
, SearchEnv, createSearchEnv
)
where

import Lucid
import Control.Monad (forM_)
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

-- | Things we want to precompute when creating the handler
data SearchEnv = SearchEnv
  { searchEnvGraph :: FunGraph.Graph ST.RealWorld
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

handler :: SearchEnv -> Maybe T.Text -> Maybe T.Text -> Maybe Word -> Handler (Html ())
handler searchEnv (Just src) (Just dst) mMaxCount =
  let defaultLimit = 100 -- TODO: add as HTML input field
  in page searchEnv src dst (fromMaybe defaultLimit mMaxCount)
handler _ _ _ _ =
  throwError $ err400 { errBody = "Missing 'src' and/or 'dst' query param" }

page :: SearchEnv -> T.Text -> T.Text -> Word -> Handler (Html ())
page (SearchEnv graph lookupVertex) srcTxt dstTxt maxCount = do
  src <- lookupVertexM srcTxt
  dst <- lookupVertexM dstTxt
  resultGraphE <- liftIO $ renderResultGraphIO (src, dst)
  results <- liftIO $ ST.stToIO $ getResults (src, dst)
  pure $ if null results then noResultsText (src, dst) else do
    table_ $ do
      thead_ $
        tr_ $ do
          td_ "Function composition"
          td_ "Dependencies"
      tbody_ $
        forM_ (map fst results) $ \result ->
          tr_ $ do
            td_ $ renderResult result
            td_ $
              mconcat $
                intersperse ", " $
                  map mkPackageLink (nubOrd $ map FunGraph._function_package result)
    h2_ "Result graph"
    either
      (const $ plain "Failed to render result graph")
      toHtmlRaw -- 'toHtmlRaw' because 'resultGraph' contains tags we don't want escaped
      resultGraphE
  where
    noResultsText :: (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType) -> Html ()
    noResultsText (src, dst) =
      p_ [style_ "color:red"] $ mconcat
        [ "No results found. No path from "
        , mono $ toHtml $ FunGraph.renderFullyQualifiedType src
        , " to "
        , mono $ toHtml $ FunGraph.renderFullyQualifiedType dst
        , "."
        ]

    mkPackageLink fnPkg =
      a_
        [href_ $ "https://hackage.haskell.org/package/" <> FunGraph.renderFgPackage fnPkg]
        (mono $ toHtml $ FunGraph.fgPackageName fnPkg)

    lookupVertexM txt =
      maybe
        (throwError $ err404 { errBody = "Type not found: " <> TLE.encodeUtf8 (LT.fromStrict txt) })
        pure
        (lookupVertex txt)

    getResults srcDst =
      take (fromIntegral maxCount) .
        FunGraph.queryResultTreeToPaths srcDst <$> query srcDst

    renderResultGraphIO srcDst =
      ST.stToIO (resultDotGraph srcDst)
        >>= Server.GraphViz.renderDotGraph

    resultDotGraph srcDst =
      query srcDst
        >>= Util.graphFromQueryResult
        >>= Util.graphToDot ""

    query srcDst =
      FunGraph.runQueryTree
        (fromIntegral maxCount)
        srcDst
        graph

    renderResult :: [FunGraph.TypedFunction] -> Html ()
    renderResult fns =
      let nameWithTypeLst = map FunGraph.renderTypedFunction (reverse fns)
          renderSingleFn (name, (fromTy, toTy)) =
            let typeSig = T.unwords $
                  [ "::"
                  , FunGraph.fullyQualifiedTypeToText fromTy
                  , "->"
                  , FunGraph.fullyQualifiedTypeToText toTy
                  ]
            in mono (toHtml name) `with` [title_ typeSig]
      in mconcat $ intersperse (mono " . ") $ map renderSingleFn nameWithTypeLst

    mono =
      let style = style_ $ T.intercalate "; " $
            [ "font-family: monospace, monospace"
            , "background-color: rgb(200, 200, 200)"
            ]
      in span_ [style]

plain :: Monad m => T.Text -> HtmlT m ()
plain = toHtml
