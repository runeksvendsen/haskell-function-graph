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
import Lucid.Base
import Lucid.Htmx
import qualified Data.Text as T
import Server.HtmlStream
import Data.Maybe (fromMaybe)

type HandlerType = HtmlStream IO ()

page
  :: Monad m
  => Html ()
  -- ^ Append to 'head' element
  -> Html ()
  -- ^ Append to 'body' content
  -> Html ()
  -- ^ Initial typeahead suggestions (sequence of 'option' elements)
  -> (HtmlStream m (), (Maybe T.Text, Maybe T.Text))
  -- ^ 1. Search result HTML
  --   2. Maybe the entered (src, dst). NOTE: 'T.Text' instead of 'FunGraph.FullyQualifiedType' because we want to support filling the input fields with arbitrary text (not just valid types).
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
  let searchResultId = "search_result"
  streamTagBalancedAttrM "body"
    [ hxExt_ "chunked-transfer" -- Necessary because HTMX breaks "chunked" Transfer-Encoding. See https://github.com/bigskysoftware/htmx/issues/1911
    , hxBoost_ "true"
    , hxTarget_ ("#" <> searchResultId)
    , hxPushUrl_ "true"
    ] $ do
      streamHtml $ do
        h1_ "Search for compositions of functions"
        form initialSuggestions mSrcDst
        h3_ "Results"
      streamTagBalancedAttrM "div" [id_ searchResultId]
        searchResult
      streamHtml appendToBody

form
  :: Html ()  -- ^ Initial suggestions
  -> (Maybe T.Text, Maybe T.Text) -- ^ Initial values for (src, dst)
  -> Html ()
form initialSuggestions mSrcDst = do
  h2_ "Search"
  p_ "Find compositions of functions that take the FROM type as input and returns a value of the TO type."
  form_
    [ action_ "/search"
    , method_ "get"
    , role_ "search"
    ] $ do
      (srcInput, dstInput) <- mkTypeaheadInputs initialSuggestions mSrcDst
      label_ [for_ "src"] "FROM type: "
      srcInput
      label_ [for_ "dst"] "TO type: "
      dstInput
      searchButton
  where
    searchButton =
      button_ $
        div_ [style_ "display: flex; align-items: center;"] $ do -- display "Search"-text and spinner on the same line
          span_ "Search"
          with
            spinnerSvg
            [ class_ "htmx-indicator" -- display only when a HTMX request is in progress
            , style_ $ T.intercalate ";"
                [ "margin-left: 8px" -- add space between "Search"-text and spinner
                , "opacity: 0" -- hide initially when JS is disabled
                ]
            ]

    spinnerSvg =
      let mkSvg = svg_
            [ width_ "24"
            , height_ "24"
            , makeAttribute "viewBox" "0 0 24 24"
            , xmlns_ "http://www.w3.org/2000/svg"
            ]
      in
      mkSvg $
        -- Source: https://github.com/n3r4zzurr0/svg-spinners/blob/abfa05c49acf005b8b1e0ef8eb25a67a7057eb20/svg-smil/180-ring.svg
        toHtmlRaw @T.Text "<path d=\"M12,4a8,8,0,0,1,7.89,6.7A1.53,1.53,0,0,0,21.38,12h0a1.5,1.5,0,0,0,1.48-1.75,11,11,0,0,0-21.72,0A1.5,1.5,0,0,0,2.62,12h0a1.53,1.53,0,0,0,1.49-1.3A8,8,0,0,1,12,4Z\"><animateTransform attributeName=\"transform\" type=\"rotate\" dur=\"0.75s\" values=\"0 12 12;360 12 12\" repeatCount=\"indefinite\"/></path>"

mkTypeaheadInputs
  :: Html ()
  -> (Maybe T.Text, Maybe T.Text) -- ^ Initial values for (src, dst)
  -> Html (Html (), Html ())
mkTypeaheadInputs initialSuggestions (mSrc, mDst) = do
  script_ $ "function " <> jsTriggerFunctionName <> "(event) { return event instanceof KeyboardEvent }"
  pure
    ( mkInput' "src" mSrc
    , mkInput' "dst" mDst
    )
  where
    mkInput' = mkInput jsTriggerFunctionName attrs initialSuggestions

    attrs =
      [ placeholder_ "enter an unqualified type name, e.g. Text, and select the qualified name above"
      ]

    jsTriggerFunctionName = "checkUserKeydown"

mkInput
  :: T.Text -- ^ Name of JS trigger function
  -> [Attribute] -- ^ Attributes
  -> Html () -- ^ Initial suggestions
  -> T.Text -- ^ input ID
  -> Maybe T.Text -- ^ initial value (optional)
  -> Html ()
mkInput jsTriggerFunctionName attrs initialSuggestions id' mInitialValue = do
  suggestions
  input_ $ attrs ++
    [ name_ id'
    , id_ id'
    , type_ "search"
    , value_ $ fromMaybe mempty mInitialValue
    , list_ suggestionsId
    , hxGet_ "/typeahead" -- get suggestions from here (TODO: use something type-safe)
    , hxTarget_ $ "#" <> suggestionsId -- put suggestions here
    , hxTrigger_ $ "keyup[" <> jsTriggerFunctionName <> ".call(this, event)] changed delay:25ms"
    , hxPushUrl_ "false"
    , autocomplete_ "off"
    , spellcheck_ "off"
    , required_ ""
    , autofocus_
    ]
  where
    suggestionsId = "list_" <> id'

    suggestions =
      datalist_
        [ id_ suggestionsId
        , name_ suggestionsId
        ] initialSuggestions
