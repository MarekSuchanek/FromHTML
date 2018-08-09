module Main where

import           Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           System.Environment
import           System.Exit

import Text.FromHTML

main :: IO ()
main = do
  args <- getArgs
  let infile = head args
  let outfile = args !! 1
  let format = (read $ args !! 2) :: ExportType
  html <- readFile infile
  case fromHTML format html of
    Right res -> B.writeFile outfile res
    Left err -> do
      putStrLn "Couldn't transform that document:"
      C.putStrLn err
      exitFailure
