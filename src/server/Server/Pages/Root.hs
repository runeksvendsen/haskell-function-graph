{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Server.Pages.Root
( page
, HandlerType
)
where

import Lucid
import Lucid.Htmx
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

type HandlerType = Html ()

page
  :: Html ()
  -- ^ Append to 'head' element
  -> Html ()
  -- ^ Initial typeahead suggestions (sequence of 'option' elements)
  -> (Html (), (Maybe T.Text, Maybe T.Text))
  -- ^ Search result HTML and maybe the entered (src, dst).
  --
  --   If the user pastes a "/search?..." link into the browser then we want to display
  --   the root page with the search results included, as well as "src" and "dst" filled in.
  -> Html ()
page appendToHead initialSuggestions (searchResult, mSrcDst) = doctypehtml_ $ do
  head_ $ do
    title_ "Haskell Function Graph"
    appendToHead
  body_ $ do
    h1_ "Search for compositions of functions"
    let targetId = "search_result"
    form targetId initialSuggestions mSrcDst
    h3_ "Results"
    div_ [id_ targetId] searchResult

form
  :: T.Text -- ^ targetId
  -> Html ()  -- ^ Initial suggestions
  -> (Maybe T.Text, Maybe T.Text) -- ^ Initial values for (src, dst)
  -> Html ()
form targetId initialSuggestions mSrcDst = do
  h3_ "Search"
  p_ "Find compositions of functions that take the FROM type as input and returns a value of the TO type."
  form_
    [ action_ "/search"
    , method_ "get"
    , role_ "search"
    , hxBoost_ "true"
    , hxTarget_ ("#" <> targetId)
    , hxPushUrl_ "true"
    ] $ do
      (srcInput, dstInput) <- mkTypeaheadInputs initialSuggestions mSrcDst
      label_ [for_ "src"] "FROM type: "
      srcInput
      label_ [for_ "dst"] "TO type: "
      dstInput
      button_ [] "Search"

mkTypeaheadInputs
  :: Html ()
  -> (Maybe T.Text, Maybe T.Text) -- ^ Initial values for (src, dst)
  -> Html (Html (), Html ())
mkTypeaheadInputs initialSuggestions (mSrc, mDst) = do
  script_ "function checkUserKeydown(event) { return event instanceof KeyboardEvent }"
  pure
    ( mkInput "src" [placeholder_ "FROM type"] mSrc
    , mkInput "dst" [placeholder_ "TO type"] mDst
    )
  where
    typeSuggestionsBaseId = "type_suggestions"

    mkInput id' attrs mInitialValue = do
      let typeSuggestionsId = typeSuggestionsBaseId <> "_" <> id'
      mkSuggestions typeSuggestionsId initialSuggestions
      input_ $ attrs ++
        [ name_ id'
        , id_ id'
        , value_ $ fromMaybe "" mInitialValue
        , type_ "search"
        , list_ typeSuggestionsId
        , hxGet_ "/typeahead" -- TODO: use something type-safe
        , hxTarget_ $ "#" <> typeSuggestionsId
        , hxTrigger_ "keyup[checkUserKeydown.call(this, event)] changed delay:25ms"
        , hxPushUrl_ "false"
        ]

mkSuggestions :: T.Text -> Html () -> Html ()
mkSuggestions id' =
  datalist_ [id_ id']
