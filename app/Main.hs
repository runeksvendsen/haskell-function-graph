module Main (main) where

import qualified MyLib
import qualified MyLib.Examples
import qualified System.Environment as Env

main :: IO ()
main = do
  [fileName] <- Env.getArgs
  declarationMapJsonList <- either fail pure =<< MyLib.fileReadDeclarationMap fileName
  MyLib.runPrintQueryAll maxCount declarationMapJsonList (fst MyLib.Examples.strictByteString, fst MyLib.Examples.lazyText)
  where
    maxCount = 10
