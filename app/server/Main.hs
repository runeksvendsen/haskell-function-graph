{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Server
import qualified Server.CLI
import qualified Server.Config
import qualified Data.FileEmbed
import qualified Data.Text.Encoding as TE
import Lucid

main :: IO ()
main = do
  Server.CLI.withArgs $ \serverConfig ->
    Server.main $ serverConfig{Server.Config.serverConfigAppendToHeadHtml = appendToHead}
  where
    appendToHead = style_ $ TE.decodeUtf8 cssBS
    cssBS = $(Data.FileEmbed.makeRelativeToProject "css/chota.css" >>= Data.FileEmbed.embedFile)
