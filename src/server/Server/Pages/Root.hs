{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Pages.Root
( page
, HandlerType
)
where

import Lucid
import Lucid.Htmx
import qualified Data.Text as T
import qualified FunGraph

type HandlerType = Html ()

page
  :: Html ()
  -- ^ Append to 'head' element
  -> Html ()
  -- ^ Initial typeahead suggestions (sequence of 'option' elements)
  -> (Html (), Maybe (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType))
  -- ^ Search result HTML and maybe the entered (src, dst).
  --
  --   If the user pastes a "/search?..." link into the browser then we want to display
  --   the root page with the search results included, as well as "src" and "dst" filled in.
  -> Html ()
page appendToHead initialSuggestions (searchResult, mSrcDst) = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      title_ "Haskell Function Graph"
      appendToHead
    body_  $ do
      h1_ "Search for compositions of functions"
      let targetId = "search_result"
      form targetId initialSuggestions mSrcDst
      h3_ "Results"
      div_ [id_ targetId] searchResult

form
  :: T.Text -- ^ targetId
  -> Html ()  -- ^ Initial suggestions
  -> Maybe (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType) -- ^ Initial values for (src, dst)
  -> Html ()
form targetId initialSuggestions mSrcDst = do
  h2_ "Search"
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
  -> Maybe (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType) -- ^ Initial values for (src, dst)
  -> Html (Html (), Html ())
mkTypeaheadInputs initialSuggestions mSrcDst = do
  script_ "function checkUserKeydown(event) { return event instanceof KeyboardEvent }"
  pure
    ( mkInput "src" [] (fst <$> mSrcDst)
    , mkInput "dst" [] (snd <$> mSrcDst)
    )
  where
    mkInput id' attrs mInitialValue = do
      let inputId = id' <> "_" <> "input"
      mkSuggestions id' mInitialValue initialSuggestions
      input_ $ attrs ++
        [ name_ inputId
        , id_ inputId
        , type_ "search"
        , placeholder_ "enter an unqualified type name, e.g. Text, and select the qualified name above"
        , list_ id'
        , hxGet_ "/typeahead" -- TODO: use something type-safe
        , hxTarget_ $ "#" <> id'
        , hxTrigger_ "keyup[checkUserKeydown.call(this, event)] changed delay:25ms"
        , hxPushUrl_ "false"
        ]

mkSuggestions
  :: T.Text -- ^ id
  -> Maybe FunGraph.FullyQualifiedType -- ^ initial value
  -> Html ()
  -> Html ()
mkSuggestions id' mFqt suggestions =
  select_
    [ id_ id'
    , name_ id'
    , required_ ""
    ] $ maybe mempty selectedSuggestion mFqt <> suggestions
  where
    selectedSuggestion :: FunGraph.FullyQualifiedType -> Html ()
    selectedSuggestion fqt =
      -- TODO: fix below so it agrees with Server.Pages.Typeahead.suggestions
      option_
        [ value_ $ FunGraph.renderFullyQualifiedType fqt
        , label_ $ FunGraph.renderFullyQualifiedTypeNoPackage fqt
        , selected_ ""
        ]
        ""
