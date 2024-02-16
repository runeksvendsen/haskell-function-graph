{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Server.Pages.Root
( handler
)
where

import Lucid
import Lucid.Htmx
import qualified Data.Text as T
import Servant.Server (Handler)
import qualified Server.Pages.Search as Search
import qualified FunGraph.Examples as Examples
import qualified FunGraph
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

handler :: Html () -> FunGraph.FrozenGraph -> Maybe T.Text -> Maybe T.Text -> Maybe Word -> Handler (Html ())
handler appendToHead graph mSrc mDst mLimit =
  case (mSrc, mDst) of
    (Just src, Just dst) -> do
      searchPage <- Search.page graph src dst (fromMaybe defaultLimit mLimit)
      pure $ page' <> searchPage -- NOTE: The 'hxTarget_' trick doesn't seem to work so this is done for now instead
    _ -> pure page'
  where
    page' = page appendToHead

    defaultLimit = 100 -- TODO: add as input field

page :: Html () -> Html ()
page appendToHead = doctypehtml_ $ do
  head_ $ do
    title_ "Haskell Function Graph"
    appendToHead
  body_ $ do
    div_ [id_ "header"] "Search for compositions of functions"
    let targetId = "search_result"
    form targetId
    h3_ "Results"
    div_ [id_ targetId] ""

form :: T.Text -> Html ()
form targetId = do
  h3_ "Search"
  p_ "Find compositions of functions that take the FROM type as input and returns a value of the TO type."
  form_ [hxGet_ "/search", hxTarget_ ("#" <> targetId)] $ do
    label_ [for_ "src"] "FROM type: "
    input_ [name_ "src", id_ "src", list_ "type_suggestions", placeholder_ "FROM type"]
    suggestions "type_suggestions"
    label_ [for_ "dst"] "TO type: "
    input_ [name_ "dst", id_ "dst", list_ "type_suggestions", placeholder_ "TO type"]
    suggestions "type_suggestions"
    button_ [] "Search"

suggestions :: T.Text -> Html ()
suggestions id' = do
  datalist_ [id_ id'] $ do
    forM_ Examples.all $ \example ->
       option_ [value_ $ FunGraph.fullyQualifiedTypeToText (fst example)] ""
