module Main (main) where

import qualified Server
import qualified System.Environment as Env
import Text.Read (readMaybe)

main :: IO ()
main = do
  argList <- Env.getArgs
  (port, graphDataFileName) <- case argList of
    [portStr, graphDataFileName] -> do
      port <- maybe (fail $ "invalid port number: " <> portStr) pure (readMaybe portStr)
      pure (port, graphDataFileName)
    _ -> fail "Expected exactly two arguments: (1) port number; (2) graph JSON data filename"
  putStrLn $ "Running server on " <> "http://localhost:" <> show port
  Server.main port graphDataFileName
