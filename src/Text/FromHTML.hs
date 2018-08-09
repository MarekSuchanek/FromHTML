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
import           Data.Semigroup
import           GHC.IO.Handle
import           GHC.IO.Encoding
import           System.Exit
import           System.Process
import           System.IO.Unsafe

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

type Input = String
type Output = B.ByteString
type Command = Input -> IO (Either Output Output)
type Process = IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

str2BS :: String -> B.ByteString
str2BS = E.encodeUtf8 . T.pack

-- | Transform given HTML as String to selected format
fromHTML :: ExportType -> String -> Either Output Output
fromHTML HTML html = Right . str2BS $ html  -- HTML is already provided!
fromHTML PDF html = makePDF html
fromHTML extp html = makePD extp html

makePDF :: Input -> Either Output Output
makePDF html = unsafePerformIO $ wkhtmltopdf html

makePD :: ExportType -> Input -> Either Output Output
makePD expt html = unsafePerformIO $ pandoc expt html

-- | Simple conversion of HTML to PDF using process wkhtmltopdf
wkhtmltopdf :: Command
wkhtmltopdf = perform cprocess
    where
        opts = ["--quiet", "--encoding", "utf-8", "-", "-"]
        cprocess = procWith $ proc "wkhtmltopdf" opts

-- | Simple conversion of HTML to some format using process pandoc
pandoc :: ExportType -> Command
pandoc expt = perform cprocess
    where
        format = exportType2PD expt
        opts = ["-s", "-f", "html", "-t", format, "-o", "-"]
        cprocess = procWith $ proc "pandoc" opts

-- | Perform process (catched IOException)
perform :: CreateProcess -> Command
perform cprocess input = catch (performUnsafe cprocess input)
        (\e -> do let err = show (e :: IOException)
                  return . Left $ "IOException: " <> str2BS err)

-- | Perform process (no caching exceptions)
performUnsafe :: CreateProcess -> Command
performUnsafe cprocess input = do
    setLocaleEncoding utf8  -- don't know what was locales are there...
    (Just stdin, Just stdout, Just stderr, p) <- createProcess cprocess
    hPutStr stdin input >> hClose stdin
    exitCode <- waitForProcess p
    errors <- B.hGetContents stderr
    output <- B.hGetContents stdout
    case exitCode of
      ExitSuccess -> return $ Right output
      _           -> return . Left $ "Exit(" <> str2BS (show exitCode) <> "): " <> errors

procWith p = p { std_out = CreatePipe
               , std_in  = CreatePipe
               , std_err = CreatePipe
               }

exportType2PD :: ExportType -> String
exportType2PD = map C.toLower . show
