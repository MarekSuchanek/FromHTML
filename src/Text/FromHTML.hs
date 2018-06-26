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


-- | Transform given HTML as String to selected format
fromHTML :: ExportType -> String -> Maybe B.ByteString
fromHTML HTML html = Just . E.encodeUtf8 . T.pack $ html  -- HTML is already provided!
fromHTML extp html = case mpd of
                        Just pd -> eitherToMaybe . Pandoc.runPure $ actwriter Pandoc.def pd
                        _       -> Nothing
  where
    mpd :: Maybe Pandoc.Pandoc
    mpd = eitherToMaybe . Pandoc.runPure $ Pandoc.readHtml Pandoc.def . T.pack $ html
    actwriter = writer extp

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
