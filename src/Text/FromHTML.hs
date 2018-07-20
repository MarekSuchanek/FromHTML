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

--import Debug.Trace

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B

import           GHC.IO.Handle
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


-- | Helper function to translate Either to Maybe
eitherToMaybe :: Show a => Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _ = Nothing

-- Variant for debugging
-- eitherToMaybe :: Show a => Either a b -> Maybe b
-- eitherToMaybe (Right x) = Just x
-- eitherToMaybe (Left x) = traceShow x Nothing

str2BS :: String -> B.ByteString
str2BS = E.encodeUtf8 . T.pack

-- | Transform given HTML as String to selected format
fromHTML :: ExportType -> String -> Maybe B.ByteString
fromHTML HTML html = Just . str2BS $ html  -- HTML is already provided!
fromHTML PDF html = handleMaker $ makePDF html
fromHTML extp html = handleMaker $ makePD extp html


type Input = String
type Output = B.ByteString
type Command = Input -> IO (Either Output Output)
type Process = IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)


handleMaker :: Either Output Output -> Maybe B.ByteString
handleMaker (Right x) = Just x
handleMaker _ = Nothing

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

-- | Simple conversion of HTML to PDF using process wkhtmltopdf
pandoc :: ExportType -> Command
pandoc expt = perform cprocess
    where
        format = exportType2PD expt
        opts = ["-s", "-f", "html", "-t", format, "-o", "-"]
        cprocess = procWith $ proc "pandoc" opts


perform :: CreateProcess -> Command
perform cprocess input = do
    (Just stdin, Just stdout, Just stderr, p) <- createProcess cprocess
    hPutStr stdin input >> hClose stdin
    exitCode <- waitForProcess p
    case exitCode of
      ExitSuccess -> Right <$> B.hGetContents stdout
      _           -> Left <$> B.hGetContents stderr


procWith p = p { std_out = CreatePipe
               , std_in  = CreatePipe
               , std_err = CreatePipe
               }

exportType2PD :: ExportType -> String
exportType2PD = map C.toLower . show
