{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Server
import qualified System.Environment as Env
import qualified Data.FileEmbed
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)
import Lucid

main :: IO ()
main = do
  argList <- Env.getArgs
  (port, graphDataFileName) <- case argList of
    [portStr, graphDataFileName] -> do
      port <- maybe (fail $ "invalid port number: " <> portStr) pure (readMaybe portStr)
      pure (port, graphDataFileName)
    _ -> fail "Expected exactly two arguments: (1) port number; (2) graph JSON data filename"
  Server.main Server.defaultSearchConfig appendToHead port graphDataFileName
  where
    appendToHead = style_ $ TE.decodeUtf8 cssBS
    cssBS = $(Data.FileEmbed.makeRelativeToProject "css/chota.css" >>= Data.FileEmbed.embedFile)
