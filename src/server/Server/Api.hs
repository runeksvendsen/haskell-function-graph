{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api where

import Servant.API
import Servant.HTML.Lucid
import Lucid (Html)
import qualified Data.Text as T

type Api = Root :<|> Search :<|> Typeahead

type Root
  =  Get '[HTML] (Html ())

type Search
  =  "search"
  :> Header "HX-Boosted" HxBoosted -- if present, we send only a fragment of a page, otherwise a whole page
  :> QueryParam "src" T.Text
  :> QueryParam "dst" T.Text
  :> QueryParam "limit" Word
  :> Get '[HTML] (Html ())

type Typeahead
  =  "typeahead"
  :> QueryParam "src" T.Text -- NB: matches input_ "name" in Server.Pages.Root.form
  :> QueryParam "dst" T.Text -- NB: matches input_ "name" in Server.Pages.Root.form
  :> Get '[HTML] (Html ())

-- | Signals that the @HX-Boosted@ HTTP header is set
data HxBoosted = HxBoosted
  deriving Show

instance FromHttpApiData HxBoosted where
  parseQueryParam _ = pure HxBoosted
