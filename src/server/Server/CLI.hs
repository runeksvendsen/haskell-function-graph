module Server.CLI
( withArgs
)
where

import Options.Applicative
import Server.Config
import qualified Server.Pages.Search
import qualified Options.Applicative as Opt
import qualified GHC.IO.Unsafe
import qualified System.Console.ANSI as ANSI
import Data.Bool (bool)

withArgs
  :: (ServerConfig -> IO a)
  -> IO a
withArgs f = do
  args <- Opt.execParser opts
  f args

options :: Opt.Parser ServerConfig
options = ServerConfig
  <$> fmap fromIntegral parseServerConfigPort
  <*> parseServerConfigGraphDataFilename
  <*> pure (serverConfigAppendToHeadHtml defaultServerConfig)
  <*> parseServerConfigSearchConfig
  <*> parseServerConfigBuildConfig

opts :: Opt.ParserInfo ServerConfig
opts = info (helper <*> options) $
     fullDesc
  <> progDesc "Run function-graph web server"
  <> header "Haskell Function Graph"

parseServerConfigPort :: Opt.Parser Word
parseServerConfigPort = option auto $
     long "port"
  <> short 'p'
  <> metavar "PORT"
  <> help "Server listen port"
  <> value (fromIntegral $ serverConfigPort defaultServerConfig)
  <> showDefault

parseServerConfigGraphDataFilename :: Opt.Parser FilePath
parseServerConfigGraphDataFilename = strOption $
     long "graph-file"
  <> short 'g'
  <> metavar "GRAPH_FILE"
  <> help "Path to graph JSON file"

parseServerConfigSearchConfig :: Opt.Parser SearchConfig
parseServerConfigSearchConfig = Server.Pages.Search.SearchConfig
  <$> fmap realToFrac parseSearchConfigTimeout
  <*> (bool Nothing (Just traceFun) <$> parseSearchConfigTrace)
  <*> parseSearchConfigSuggestionLimit
  where
    parseSearchConfigTimeout :: Parser Double
    parseSearchConfigTimeout = option auto $
         long "search-timeout"
      <> metavar "TIMEOUT_SECONDS"
      <> help "Maximum number of seconds to spend on each search request"
      <> value (realToFrac $ Server.Pages.Search.searchConfigTimeout defaultSearchConfig)
      <> showDefault

    parseSearchConfigTrace = switch $
         long "trace"
      <> help "Trace search requests"

    parseSearchConfigSuggestionLimit = option auto $
         long "max-suggestions"
      <> metavar "MAX_SUGGESTIONS"
      <> help "Maximum number of suggestions to display for each 'type' input search field"
      <> value (Server.Pages.Search.searchConfigSuggestionLimit defaultSearchConfig)
      <> showDefault

-- TODO
parseServerConfigBuildConfig :: Opt.Parser BuildConfig
parseServerConfigBuildConfig =
  pure defaultBuildConfig

defaultServerConfig :: ServerConfig
defaultServerConfig =
  mkDefaultServerConfig ""

traceFun
  :: Applicative f
  => String
  -> f ()
traceFun string =
  let color color' = concat
        [ ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Dull color']
        , string
        , ANSI.setSGRCode [ANSI.Reset]
        ]
  in pure $
    GHC.IO.Unsafe.unsafePerformIO $ do
      putStrLn $ color ANSI.Red
