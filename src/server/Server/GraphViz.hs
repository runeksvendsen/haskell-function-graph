module Server.GraphViz
( renderDotGraph
, healthCheck
)
where

import qualified System.Process as Proc
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import System.Exit (ExitCode(ExitSuccess))
import qualified Control.Exception as Ex
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Text.Lazy.Encoding
import qualified Data.ByteString.Char8

dotExe :: FilePath
dotExe = "dot"

runDotExe
  :: [String] -- ^ arguments
  -> String -- ^ stdin
  -> IO (Either String (ExitCode, String, String)) -- ^ exitcode, stdout, stderr
runDotExe args stdin = do
  res@(exitCode, stdout, stderr) <- Proc.readProcessWithExitCode dotExe args stdin
  case exitCode of
    ExitSuccess -> pure $ Right res
    _ -> pure $ Left $ unwords
      [ "Executing"
      , "'" <> dotExe <> "'"
      , "with arguments"
      , "'" <> unwords args <> "'"
      , "failed with"
      , show exitCode <> "."
      , "Stdout:"
      , stdout <> "."
      , "Stderr:"
      , stderr <> "."
      ]

-- | Throw an exception in case of missing runtime dependencies.
--
--   Run when starting server to exit early in case of missing runtime dependencies.
healthCheck :: IO ()
healthCheck =
  runDotExe ["-V"] "" >>= either (Ex.throwIO . Ex.ErrorCall) (const $ pure ())

renderDotGraph
  :: LT.Text
  -> IO (Either String BS.ByteString)
renderDotGraph dot = do
  eRes <- runDotExe
    [ "-v" -- verbose
    , "-Tsvg" -- output format: SVG
    , "-Kdot" -- layout engine: dot
    ]
    (Data.ByteString.Lazy.Char8.unpack . Data.Text.Lazy.Encoding.encodeUtf8 $ dot)
  pure $ do
    (_, stdout, _) <- eRes
    pure $ Data.ByteString.Char8.pack stdout
