{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

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
  -- ^ Append to 'body' content
  -> Html ()
  -- ^ Initial typeahead suggestions (sequence of 'option' elements)
  -> (HtmlStream m (), Maybe (FunGraph.FullyQualifiedType, FunGraph.FullyQualifiedType))
  -- ^ Search result HTML and maybe the entered (src, dst).
  --
  --   If the user pastes a "/search?..." link into the browser then we want to display
  --   the root page with the search results included, as well as "src" and "dst" filled in.
  -> HtmlStream m ()
page appendToHead appendToBody initialSuggestions (searchResult, mSrcDst) = do
  streamHtml doctype_
  streamTagBalancedAttr "html" [lang_ "en"]
  streamHtml $
    head_ $ do
      title_ "Haskell Function Graph"
      appendToHead
  streamTagBalancedAttrM "body" [hxExt_ "chunked-transfer"] $ do -- Necessary because HTMX breaks "chunked" Transfer-Encoding. See https://github.com/bigskysoftware/htmx/issues/1911
    let targetId = "search_result"
    streamHtml $ do
      h1_ "Search for compositions of functions"
      form targetId initialSuggestions mSrcDst
      h3_ "Results"
    streamTagBalancedAttrM "div" [id_ targetId]
      searchResult
    streamHtml appendToBody

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
      button_ [] $ "Search" <> spinnerSvg -- TODO: Display spinner on the same line as "Search" text
  where
    -- Source: https://github.com/n3r4zzurr0/svg-spinners/blob/abfa05c49acf005b8b1e0ef8eb25a67a7057eb20/svg-smil/180-ring.svg
    spinnerSvg = toHtmlRaw @T.Text
      "<svg class=\"htmx-indicator\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"M12,4a8,8,0,0,1,7.89,6.7A1.53,1.53,0,0,0,21.38,12h0a1.5,1.5,0,0,0,1.48-1.75,11,11,0,0,0-21.72,0A1.5,1.5,0,0,0,2.62,12h0a1.53,1.53,0,0,0,1.49-1.3A8,8,0,0,1,12,4Z\"><animateTransform attributeName=\"transform\" type=\"rotate\" dur=\"0.75s\" values=\"0 12 12;360 12 12\" repeatCount=\"indefinite\"/></path></svg>"

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
