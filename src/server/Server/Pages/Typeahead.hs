{-# LANGUAGE OverloadedStrings #-}

module Server.Pages.Typeahead
( handler
)
where

import Lucid
import Control.Monad (forM_)
import qualified Data.Text as T
import Servant.Server
import qualified FunGraph

handler
  :: FunGraph.FrozenGraph
  -> Maybe T.Text
  -> Maybe T.Text
  -> Handler (Html ())
handler graph mSrc mDst = pure $
  case (mSrc, mDst) of
    (Just src, Nothing) -> suggestions src
    (Nothing, Just dst) -> suggestions dst
    (_, _) -> error "TODO"

suggestions :: T.Text -> Html ()
suggestions prefix = do
  datalist_ [id_ "type_suggestions"] $ -- TODO: don't hardcode "id" from Server.Pages.Root.suggestions
    forM_ (map (T.pack . (: [])) ['a'..'z']) $ \postfix ->
      option_ [value_ $ prefix <> postfix] ""
