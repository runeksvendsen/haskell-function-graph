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
import Server.HtmlStream
import qualified Data.Text as T
import qualified FunGraph
import qualified Server.Pages.Typeahead

type HandlerType = HtmlStream IO ()

page
  :: Monad m
  => Html ()
  -- ^ Append to 'head' element
  -> Html ()
  -- ^ Initial typeahead suggestions (sequence of 'option' elements)
  -> (HtmlStream m (), Maybe (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType))
  -- ^ Search result HTML and maybe the entered (src, dst).
  --
  --   If the user pastes a "/search?..." link into the browser then we want to display
  --   the root page with the search results included, as well as "src" and "dst" filled in.
  -> HtmlStream m ()
page appendToHead initialSuggestions (searchResult, mSrcDst) = do
  streamHtml doctype_
  streamTagBalancedAttr "html" [lang_ "en"]
  streamHtml $
    head_ $ do
      title_ "Haskell Function Graph"
      appendToHead
  streamTagBalancedM "body" $ do
    let targetId = "search_result"
    streamHtml $ do
      h1_ "Search for compositions of functions"
      form targetId initialSuggestions mSrcDst
      h3_ "Results"
    streamTagBalancedAttrM "div" [id_ targetId]
      searchResult

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
    ] $ maybe mempty (Server.Pages.Typeahead.suggestionOption_ [selected_ ""]) mFqt <> suggestions
