{-# LANGUAGE OverloadedStrings #-}
module Text.FromHTML where
--    ( fromHTML
--    , ExportType(..)
--    ) where

-- import Debug.Trace

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Writers as PandocWriters
import qualified Text.Pandoc.Error as PandocError
import qualified Text.Pandoc.PDF as PandocPDF

import           System.IO.Unsafe

-- | Transform given HTML as String to selected format
fromHTML :: ExportType -> String -> Maybe B.ByteString
fromHTML HTML html = Just . E.encodeUtf8 . T.pack $ html  -- HTML is already provided!
fromHTML PDF html = runOnPD writerHTML2PDF (html2pd html)
fromHTML extp html = runOnPD (\d pd -> Pandoc.runPure $ actwriter d pd) (html2pd html)
  where actwriter = writer extp

html2pd :: String -> Maybe Pandoc.Pandoc
html2pd html = eitherToMaybe . Pandoc.runPure $ Pandoc.readHtml Pandoc.def (T.pack html)

-- | Ugly PDF writer from HTML
writerHTML2PDF opts pd = fixError . unsafePerformIO . Pandoc.runIO $ PandocPDF.makePDF "wkhtmltopdf" ["--quiet"] PandocWriters.writeHtml5String opts pd
  where
    fixError (Left pderr) = Left pderr
    fixError (Right (Left bserr)) = Left . PandocError.PandocSomeError . T.unpack . E.decodeUtf8 . BL.toStrict $ bserr
    fixError (Right (Right x)) = Right (BL.toStrict x)

runOnPD f (Just pd) = eitherToMaybe (f Pandoc.def pd)
runOnPD _ Nothing   = Nothing


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

type Writer = (Pandoc.WriterOptions -> Pandoc.Pandoc -> Pandoc.PandocPure B.ByteString)

-- | Helper function to translate Either to Maybe
eitherToMaybe :: Show a => Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _ = Nothing

-- Variant for debugging
-- eitherToMaybe :: Show a => Either a b -> Maybe b
-- eitherToMaybe (Right x) = Just x
-- eitherToMaybe (Left x) = traceShow x Nothing

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
