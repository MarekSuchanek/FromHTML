{-|
Module      : Text.FromHTML
Description : Simple library for transformation of HTML to other formats
Copyright   : (c) Marek Suchánek, 2018
License     : MIT
Maintainer  : marek.suchanek@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Simplified API for transformation of HTML to other formats with Pandoc
and wkhtmltopdf in Haskell code. It requires @wkhtmltopdf@ installed
locally (see <https://wkhtmltopdf.org wkhtmltopdf.org>).
-}
module Text.FromHTML
   ( fromHTML
   , ExportType(..)
   ) where

-- import Debug.Trace

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Writers as PandocWriters
import qualified Text.Pandoc.Error as PandocError
import qualified Text.Pandoc.PDF as PandocPDF

import           GHC.IO.Handle
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

-- | Type alias for Pure Pandoc writer
type Writer = (Pandoc.WriterOptions -> Pandoc.Pandoc -> Pandoc.PandocPure B.ByteString)


-- | Helper function to translate Either to Maybe
eitherToMaybe :: Show a => Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _ = Nothing

-- Variant for debugging
-- eitherToMaybe :: Show a => Either a b -> Maybe b
-- eitherToMaybe (Right x) = Just x
-- eitherToMaybe (Left x) = traceShow x Nothing

-- | Transform given HTML as String to selected format
fromHTML :: ExportType -> String -> Maybe B.ByteString
fromHTML HTML html = Just . E.encodeUtf8 . T.pack $ html  -- HTML is already provided!
fromHTML PDF html = writerHTML2PDF html
fromHTML extp html = case html2pd html of
                       Just pd -> eitherToMaybe . Pandoc.runPure $ actwriter Pandoc.def pd
                       Nothing -> Nothing
  where actwriter = writer extp

html2pd :: String -> Maybe Pandoc.Pandoc
html2pd html = eitherToMaybe . Pandoc.runPure $ Pandoc.readHtml Pandoc.def (T.pack html)

-- | Ugly PDF writer from HTML
-- writerHTML2PDF opts pd = fixError . unsafePerformIO . Pandoc.runIO $ PandocPDF.makePDF "wkhtmltopdf" ["--quiet"] PandocWriters.writeHtml5String opts pd
--   where
--     fixError (Left pderr) = Left pderr
--     fixError (Right (Left bserr)) = Left . PandocError.PandocSomeError . T.unpack . E.decodeUtf8 . BL.toStrict $ bserr
--     fixError (Right (Right x)) = Right (BL.toStrict x)

-- | Wrapping HTML to PDF conversion which is unsafe
writerHTML2PDF :: String -> Maybe B.ByteString
writerHTML2PDF = Just . unsafePerformIO . html2pdf

-- | Simple conversion of HTML to PDF using process wkhtmltopdf
html2pdf :: String -> IO B.ByteString
html2pdf html = do
    (Just stdin, Just stdout, _, _) <- createProcess cprocess
    hPutStr stdin html >> hClose stdin
    B.hGetContents stdout
    where
        procWith p = p { std_out = CreatePipe
                       , std_in  = CreatePipe
                       }
        opts = ["--quiet", "--encoding", "utf-8", "-", "-"]
        cprocess = procWith $ proc "wkhtmltopdf" opts

-- | Select Writer based on given ExportType
writer :: ExportType -> Writer
writer = wrapWriter . pandocWriter
  where
    wrapWriter :: Pandoc.Writer Pandoc.PandocPure -> Writer
    wrapWriter (Pandoc.TextWriter        tw) = \opts pd -> E.encodeUtf8 <$> tw opts pd
    wrapWriter (Pandoc.ByteStringWriter bsw) = \opts pd -> BL.toStrict <$> bsw opts pd


-- | Pick Pandoc writer for pure transformation
pandocWriter :: ExportType -> Pandoc.Writer Pandoc.PandocPure
pandocWriter HTML      = Pandoc.TextWriter PandocWriters.writeHtml5String
pandocWriter LaTeX     = Pandoc.TextWriter PandocWriters.writeLaTeX
pandocWriter RTF       = Pandoc.TextWriter PandocWriters.writeRTF
pandocWriter RST       = Pandoc.TextWriter PandocWriters.writeRST
pandocWriter Markdown  = Pandoc.TextWriter PandocWriters.writeMarkdown
pandocWriter AsciiDoc  = Pandoc.TextWriter PandocWriters.writeAsciiDoc
pandocWriter DokuWiki  = Pandoc.TextWriter PandocWriters.writeDokuWiki
pandocWriter MediaWiki = Pandoc.TextWriter PandocWriters.writeMediaWiki
pandocWriter Docx      = Pandoc.ByteStringWriter PandocWriters.writeDocx
pandocWriter ODT       = Pandoc.ByteStringWriter PandocWriters.writeODT
pandocWriter EPUB2     = Pandoc.ByteStringWriter PandocWriters.writeEPUB2
pandocWriter EPUB3     = Pandoc.ByteStringWriter PandocWriters.writeEPUB3
pandocWriter PDF       = pandocWriter HTML -- cannot be done as PandocPure
