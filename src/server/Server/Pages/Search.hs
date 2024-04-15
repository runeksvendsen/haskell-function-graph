{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Pages.Search
( page
, handler
)
where

import Lucid
import Control.Monad (forM_)
import qualified Data.Text as T
import Servant.Server
import qualified FunGraph
import Data.List (intersperse)
import qualified Data.Text.Encoding as TE
import Data.Containers.ListUtils (nubOrd)
import qualified FunGraph.Util as Util
import qualified Server.GraphViz
import qualified Control.Monad.ST as ST
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Data.Maybe (fromMaybe)

handler :: FunGraph.FrozenGraph -> Maybe T.Text -> Maybe T.Text -> Maybe Word -> Handler (Html ())
handler graph (Just src) (Just dst) mMaxCount =
  let defaultLimit = 100 -- TODO: add as HTML input field
  in page graph src dst (fromMaybe defaultLimit mMaxCount)
handler _ _ _ _ =
  throwError $ err400 { errBody = "Missing 'src' and/or 'dst' query param" }

page :: FunGraph.FrozenGraph -> T.Text -> T.Text -> Word -> Handler (Html ())
page graph src dst maxCount = do
  resultGraphE <- liftIO renderResultGraphIO
  pure $ do
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
                  map (mono . toHtml . TE.decodeUtf8) $
                    nubOrd $
                      map FunGraph.functionPackageNoVersion result
    h2_ "Result graph"
    either
      (const $ plain "Failed to render result graph")
      toHtmlRaw -- 'toHtmlRaw' because 'resultGraph' contains tags we don't want escaped
      resultGraphE
  where
    srcDst = -- WIP: doesn't work, cf. 'textToFullyQualifiedType'
      (FunGraph.textToFullyQualifiedType src, FunGraph.textToFullyQualifiedType dst)

    results =
      take (fromIntegral maxCount) $
        FunGraph.queryResultTreeToPaths srcDst query

    renderResultGraphIO =
      ST.stToIO resultDotGraph
        >>= Server.GraphViz.renderDotGraph

    resultDotGraph =
      Util.graphFromQueryResult query
        >>= Util.graphToDot ""

    query =
      FunGraph.runQueryTree -- TODO: use 'runQueryTreeST' to avoid 'thaw' on every request
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
