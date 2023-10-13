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
import qualified Server.Monad as MyLib
import qualified MyLib
import Data.List (intersperse)
import qualified Data.Text.Encoding as TE
import Data.Containers.ListUtils (nubOrd)

page :: MyLib.Graph -> T.Text -> T.Text -> Handler (Html ())
page graph src dst = pure $ do
  p_ $ "Hi there, you entered src=" <> mono (toHtml src) <> ", dst=" <> mono (toHtml dst)
  table_ $ do
    thead_ $
      tr_ $ do
        td_ "Function composition"
        td_ "Dependencies"
    tbody_ $
      forM_ results $ \result ->
        tr_ $ do
          td_ $ renderResult result
          td_ $
            mconcat $
              intersperse ", " $
                map (mono . toHtml . TE.decodeUtf8) $
                  nubOrd $
                    map MyLib.functionPackageNoVersion result
  where
    maxCount = 20

    results =
      take maxCount $ MyLib.runQueryAll
        maxCount
        (MyLib.textToFullyQualifiedType src, MyLib.textToFullyQualifiedType dst)
        graph

    renderResult :: [MyLib.TypedFunction] -> Html ()
    renderResult fns =
      let nameWithTypeLst = map MyLib.renderTypedFunction (reverse fns)
          renderSingleFn (name, (fromTy, toTy)) =
            let typeSig = T.unwords $
                  [ "::"
                  , MyLib.fullyQualifiedTypeToText fromTy
                  , "->"
                  , MyLib.fullyQualifiedTypeToText toTy
                  ]
            in mono (toHtml name) `with` [title_ typeSig]
      in mconcat $ intersperse (mono " . ") $ map renderSingleFn nameWithTypeLst

    mono =
      let style = style_ $ T.intercalate "; " $
            [ "font-family: monospace, monospace"
            , "background-color: rgb(200, 200, 200)"
            ]
      in span_ [style]
