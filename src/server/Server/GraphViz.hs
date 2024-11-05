module Server.GraphViz
( renderDotGraph
, healthCheck
)
where

import qualified System.Process as Proc
import qualified Data.Text.Lazy as LT
import qualified System.Exit as Exit
import qualified Control.Exception as Ex
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Text.Lazy.Encoding
import Data.Bifunctor (first)
import qualified Data.Text as T

dotExe :: FilePath
dotExe = "dot"

runDotExe
  :: [String] -- ^ arguments
  -> String -- ^ stdin
  -> IO (Either String (Exit.ExitCode, String, String)) -- ^ exitcode, stdout, stderr
runDotExe args stdin = do
  eRes <- Ex.try $ Proc.readProcessWithExitCode dotExe args stdin
  pure $ do
    res@(exitCode, stdout, stderr) <- first showException eRes
    case exitCode of
      Exit.ExitSuccess -> Right res
      _ -> Left $ unwords
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
  where
    showException :: IOError -> String
    showException = show

-- | Throw an exception in case of missing runtime dependencies.
--
--   Run when starting server to exit early in case of missing runtime dependencies.
healthCheck
  :: (String -> IO ()) -- ^ Log 'String' without trailing newline
  -> IO ()
healthCheck logStr = do
  logStr $ "Checking if '" <> dotExe <> "' executable can be executed... "
  runDotExe ["-V"] "" >>= either handleError (const $ pure ())
  logStr "success\n"
  where
    handleError errStr = do
      logStr $ "FAIL. Is the executable '" <> dotExe <> "' on the PATH?\n"
      Exit.die errStr

renderDotGraph
  :: LT.Text
  -> IO (Either String T.Text)
renderDotGraph dot = do
  eRes <- runDotExe
    [ "-v" -- verbose
    , "-Tsvg" -- output format: SVG
    , "-Kdot" -- layout engine: dot
    ]
    (Data.ByteString.Lazy.Char8.unpack . Data.Text.Lazy.Encoding.encodeUtf8 $ dot)
  pure $ do
    (_, stdout, _) <- eRes
    pure $ T.pack stdout
