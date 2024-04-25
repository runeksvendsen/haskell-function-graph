module Server.GraphViz
( renderDotGraph
, healthCheck
)
where

import qualified System.Process as Proc
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified System.Exit as Exit
import qualified Control.Exception as Ex
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Text.Lazy.Encoding
import qualified Data.ByteString.Char8
import Data.Bifunctor (first)
import qualified System.IO
import qualified FunGraph.Util

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
healthCheck :: IO ()
healthCheck = do
  FunGraph.Util.putStrFlush $ "Checking if '" <> dotExe <> "' executable can be executed... "
  runDotExe ["-V"] "" >>= either handleError (const $ pure ())
  putStrLn "success"
  where
    handleError errStr = do
      putStrLn $ "FAIL. Is the executable '" <> dotExe <> "' on the PATH?"
      Exit.die errStr

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
