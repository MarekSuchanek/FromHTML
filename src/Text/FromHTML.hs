{-|
Module      : Text.FromHTML
Description : Simple library for transformation of HTML to other formats
Copyright   : (c) Marek Suchánek, 2018
License     : MIT
Maintainer  : marek.suchanek@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Simplified API for transformation of HTML to other formats with Pandoc
and wkhtmltopdf in Haskell code. It requires @wkhtmltopdf@ and @pandoc@
to be installed locally.
-}
module Text.FromHTML
   ( fromHTML
   , ExportType(..)
   ) where

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B

import           Control.Exception
import           GHC.IO.Encoding
import           System.Exit
import           System.Process.ByteString

-- | Allowed export types
data ExportType = HTML
                | LaTeX
                | RTF
                | RST
                | Markdown
                | AsciiDoc
                | Docx
                | ODT
                | DokuWiki
                | MediaWiki
                | EPUB2
                | EPUB3
                | PDF
                deriving (Show, Read, Enum, Bounded, Eq)

type Input = B.ByteString
type Output = B.ByteString
type Command = Input -> IO (Either Output Output)

str2BS :: String -> B.ByteString
str2BS = E.encodeUtf8 . T.pack

-- | Transform given HTML as String to selected format
fromHTML :: ExportType -> String -> IO (Either Output Output)
fromHTML HTML html = return $ Right (str2BS html)  -- HTML is already provided!
fromHTML PDF html = wkhtmltopdf (str2BS html)
fromHTML extp html = pandoc extp (str2BS html)

-- | Simple conversion of HTML to PDF using process wkhtmltopdf
wkhtmltopdf :: Command
wkhtmltopdf = perform "wkhtmltopdf" ["--quiet", "--encoding", "utf-8", "-", "-"]

-- | Simple conversion of HTML to some format using process pandoc
pandoc :: ExportType -> Command
pandoc expt = perform "pandoc" args
    where
        format = exportType2PD expt
        args = ["-s", "-f", "html", "-t", format, "-o", "-"]

-- | Perform process (catched IOException)
perform :: String -> [String] -> Command
perform cmd args input = catch (performUnsafe cmd args input)
        (\e -> do let err = show (e :: SomeException)
                  return . Left $ "Exception: " <> str2BS err)

-- | Perform process (no caching exceptions)
performUnsafe :: String -> [String] -> Command
performUnsafe cmd args input = do
    setLocaleEncoding utf8  -- don't know what was locales are there...
    (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args input
    case exitCode of
      ExitSuccess -> return $ Right stdout
      _           -> return . Left $ str2BS (show exitCode) <> ": " <> stderr

exportType2PD :: ExportType -> String
exportType2PD = map C.toLower . show
