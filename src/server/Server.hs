{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: Find out why this query is so slow (45 seconds): Params: [("src","ghc-9.6.2:GHC.Data.StringBuffer.StringBuffer"),("src_input","String"),("dst","HStringTemplate-0.8.8:Text.StringTemplate.Base.StringTemplate [ghc-prim-0.10.0:GHC.Types.Char]"),("dst_input","String")]
module Server (main) where

import qualified Server.Pages.Root
import qualified Server.Pages.Search
import qualified Server.Pages.Typeahead
import Servant
import Network.Wai.Handler.Warp (run)
import Server.Api
import qualified FunGraph
import Lucid
import qualified Data.Text as T
import qualified Server.GraphViz
import qualified Network.Wai.Middleware.Servant.Errors as Errors
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified Control.Monad.ST as ST
import qualified Data.Graph.Digraph as DG
import qualified FunGraph.Util

main :: Html () -> Int -> FilePath -> IO ()
main appendToHead port graphDataFilename = do
  Server.GraphViz.healthCheck
  FunGraph.Util.putStrFlush "Building graph... "
  FunGraph.withGraphFromFile FunGraph.defaultBuildConfig graphDataFilename $ \graph -> do
    getGraphInfo graph >>= \graphInfo -> putStrLn $ "done. " <> graphInfo
    FunGraph.Util.putStrFlush "Initializing handlers... "
    handlers <- mkHandlers appendToHead graph
    putStrLn "done"
    putStrLn $ "Running server on " <> "http://localhost:" <> show port
    run port $ app handlers
  where
    getGraphInfo graph = ST.stToIO $ do
      vertexCount <- DG.vertexCount graph
      edgeCount <- DG.edgeCount graph
      pure $ "Vertex count: " <> show vertexCount <> ", edge count: " <> show edgeCount <> "."

mkHandlers
  :: Html ()
  -> FunGraph.Graph ST.RealWorld
  -> IO Handlers
mkHandlers appendToHead graph = do
  (typeaheadHandler, initalSuggestions) <-
    Server.Pages.Typeahead.mkHandler (Just typeaheadCountLimit) graph
  let mkRootHandler = Server.Pages.Root.page (htmx <> fixSvgWidth <> appendToHead <> bodyMargin) initalSuggestions
  searchEnv <- Server.Pages.Search.createSearchEnv graph
  pure $ Handlers
        (mkRootHandler ("", Nothing))
        (\mHxBoosted mSrc mDst mMaxCount -> do
            let runSearchHandler = Server.Pages.Search.handler searchEnv mHxBoosted mSrc mDst mMaxCount
            case mHxBoosted of
              Just HxBoosted -> do
                (searchResult, _) <- runSearchHandler
                pure searchResult
              Nothing -> do
                (searchResult, (src, dst)) <- runSearchHandler
                pure $ mkRootHandler (searchResult, Just (src, dst))
        )
        typeaheadHandler
  where
    typeaheadCountLimit = 25

    fixSvgWidth = style_ $ T.unlines
      [ "svg {"
      , "  max-width: 100%;"
      , "  height: 400%;"
      , "  display: block;"
      , "}"
      ]

    bodyMargin = style_ $ T.unlines
      [ "body {"
      , "  margin: 20px;"
      , "}"
      ]

    -- TODO: inline
    htmx = toHtmlRaw $ T.unlines
      [ "<script"
      , "  src=\"https://unpkg.com/htmx.org@1.9.4\""
      , "  integrity=\"sha384-zUfuhFKKZCbHTY6aRR46gxiqszMk5tcHjsVFxnUo8VMus4kHGVdIYVbOYYNlKmHV\""
      , "  crossorigin=\"anonymous\""
      , "></script>"
      ]

data Handlers = Handlers
  !Server.Pages.Root.HandlerType
  !Server.Pages.Search.HandlerType
  !Server.Pages.Typeahead.HandlerType

app :: Handlers -> Application
app (Handlers rootHandler searchHandler typeaheadHandler) =
  enableMiddleware $ serve myApi server
  where
    myApi :: Proxy Api
    myApi = Proxy

    server =
      pure rootHandler :<|>
      searchHandler :<|>
      typeaheadHandler

    enableMiddleware =
        RL.logStdoutDev
      . Errors.errorMw @JSON @'["error", "status"]
