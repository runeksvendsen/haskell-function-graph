module Main (main) where

import qualified Server
import qualified System.Environment as Env
import Text.Read (readMaybe)

main :: IO ()
main = do
  argList <- Env.getArgs
  port <- case argList of
        [] -> pure 8080
        [portStr] -> maybe (fail $ "invalid port number: " <> portStr) pure (readMaybe portStr)
        _ -> fail "Expected at most one argument: port number"
  putStrLn $ "Running server on port " <> show port
  Server.main port
