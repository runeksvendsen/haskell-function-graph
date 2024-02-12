{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Pages.Search
( page
)
where

import Lucid
import Control.Monad (forM_)
import qualified Data.Text as T
import Servant.Server (Handler)
import qualified FunGraph
import Data.List (intersperse)
import qualified Data.Text.Encoding as TE
import Data.Containers.ListUtils (nubOrd)
import qualified FunGraph.Util as Util
import qualified Control.Monad.ST as ST
import Control.Monad.IO.Class (liftIO)

page :: FunGraph.FrozenGraph -> T.Text -> T.Text -> Word -> Handler (Html ())
page graph src dst maxCount = do
  resultGraph <- liftIO renderResultGraphIO
  pure $ do
    p_ $ "Hi there, you entered src=" <> mono (toHtml src) <> ", dst=" <> mono (toHtml dst)
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
    toHtmlRaw resultGraph -- 'toHtmlRaw' because 'resultGraph' contains tags we don't want escaped
  where
    srcDst =
      (FunGraph.textToFullyQualifiedType src, FunGraph.textToFullyQualifiedType dst)

    results =
      take (fromIntegral maxCount) $
        FunGraph.queryResultTreeToPaths srcDst query

    renderResultGraphIO =
      ST.stToIO resultDotGraph
        >>= Util.graphVizRender Util.Dot Util.Svg

    resultDotGraph =
      Util.graphFromQueryResult query
        >>= Util.graphToDotGraphviz ""

    query =
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
