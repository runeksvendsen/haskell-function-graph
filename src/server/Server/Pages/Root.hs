{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Server.Pages.Root
( page
)
where

import Lucid
import Lucid.Htmx
import qualified Data.Text as T

page
  :: Html () -- ^ Append to 'head' element
  -> Html () -- ^ Initial typeahead suggestions (sequence of 'option' elements)
  -> Html ()
page appendToHead initialSuggestions = doctypehtml_ $ do
  head_ $ do
    title_ "Haskell Function Graph"
    appendToHead
  body_ $ do
    div_ [id_ "header"] "Search for compositions of functions"
    let targetId = "search_result"
    form targetId initialSuggestions
    h3_ "Results"
    div_ [id_ targetId] ""

form :: T.Text -> Html () -> Html ()
form targetId initialSuggestions = do
  h3_ "Search"
  p_ "Find compositions of functions that take the FROM type as input and returns a value of the TO type."
  form_ [hxGet_ "/search", hxTarget_ ("#" <> targetId)] $ do
    (srcInput, dstInput) <- mkTypeaheadInputs initialSuggestions
    label_ [for_ "src"] "FROM type: "
    srcInput
    label_ [for_ "dst"] "TO type: "
    dstInput
    button_ [] "Search"

mkTypeaheadInputs
  :: Html ()
  -> Html (Html (), Html ())
mkTypeaheadInputs initialSuggestions = do
  script_ "function checkUserKeydown(event) { return event instanceof KeyboardEvent }"
  pure
    ( mkInput "src" [placeholder_ "FROM type"]
    , mkInput "dst" [placeholder_ "TO type"]
    )
  where
    typeSuggestionsBaseId = "type_suggestions"

    mkInput id' attrs = do
      let typeSuggestionsId = typeSuggestionsBaseId <> "_" <> id'
      mkSuggestions typeSuggestionsId initialSuggestions
      input_ $ attrs ++
        [ name_ id'
        , id_ id'
        , type_ "search"
        , list_ typeSuggestionsId
        , hxGet_ "/typeahead" -- TODO: use something type-safe
        , hxTarget_ $ "#" <> typeSuggestionsId
        , hxTrigger_ "keyup[checkUserKeydown.call(this, event)] changed delay:25ms"
        ]

mkSuggestions :: T.Text -> Html () -> Html ()
mkSuggestions id' =
  datalist_ [id_ id']
