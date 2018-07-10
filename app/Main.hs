module Main where

import Prelude hiding (writeFile)
import Data.ByteString (writeFile)
import System.Environment
import System.Exit

import Text.FromHTML

main :: IO ()
main = do
  args <- getArgs
  let infile = head args
  let outfile = args !! 1
  let format = (read $ args !! 2) :: ExportType
  html <- readFile infile
  case fromHTML format html of
    Just bs -> writeFile outfile bs
    Nothing -> do
      putStrLn "Couldn't transform that document..."
      exitFailure
