{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Server.Pages.Root
( page
)
where

import Lucid
import Lucid.Htmx
import qualified Data.Text as T
import qualified FunGraph.Examples as Examples
import qualified FunGraph
import Control.Monad (forM_)

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
