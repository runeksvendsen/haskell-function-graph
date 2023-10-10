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

page :: MyLib.Graph -> T.Text -> T.Text -> Handler (Html ())
page graph src dst = pure $ do
  p_ $ "Hi there, you entered src=" <> mono (toHtml src) <> ", dst=" <> mono (toHtml dst)
  forM_ (map MyLib.renderComposedFunctions results) $ \result -> do
    mono (toHtml result)
    br_ []
  where
    mono = span_ [style_ "font-family: monospace, monospace; background-color: rgb(200, 200, 200);"]
    maxCount = 20
    results = take maxCount $ MyLib.runQueryAll maxCount (MyLib.textToFullyQualifiedType src, MyLib.textToFullyQualifiedType dst) graph
