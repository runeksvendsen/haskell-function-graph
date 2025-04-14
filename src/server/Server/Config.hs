module Server.Config
( ServerConfig(ServerConfig)
  , serverConfigPort
  , serverConfigGraphDataFilename
  , serverConfigAppendToHeadHtml
  , serverConfigSearchConfig
  , serverConfigBuildConfig
, mkDefaultServerConfig
, Server.Pages.Search.SearchConfig, Server.Pages.Search.defaultSearchConfig
, FunGraph.Build.BuildConfig, FunGraph.Build.defaultBuildConfig
)
where

import qualified Server.Pages.Search
import Lucid (Html)
import qualified FunGraph.Build
import qualified GHC.IO.Unsafe
import qualified System.Console.ANSI as ANSI

data ServerConfig = ServerConfig
  { serverConfigPort :: Int
  , serverConfigGraphDataFilename :: FilePath
  , serverConfigAppendToHeadHtml :: Html ()
  , serverConfigSearchConfig :: Server.Pages.Search.SearchConfig
  , serverConfigBuildConfig :: FunGraph.Build.BuildConfig
  }

mkDefaultServerConfig
  :: FilePath -- ^ Graph data file
  -> ServerConfig
mkDefaultServerConfig graphDataFilename = ServerConfig
  { serverConfigPort = 8080
  , serverConfigGraphDataFilename = graphDataFilename
  , serverConfigAppendToHeadHtml = mempty
  , serverConfigSearchConfig = Server.Pages.Search.defaultSearchConfig
  , serverConfigBuildConfig = FunGraph.Build.defaultBuildConfig
  }

traceFun
  :: Applicative f
  => String
  -> f ()
traceFun str =
  let color color' = concat
        [ ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Dull color']
        , str
        , ANSI.setSGRCode [ANSI.Reset]
        ]
  in pure $
    GHC.IO.Unsafe.unsafePerformIO $ do
      putStrLn $ color ANSI.Red
