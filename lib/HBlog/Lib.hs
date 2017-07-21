{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HBlog.Lib
    ( caselessre
    , hblogPandocReaderOptions
    , hblogPandocWriterOptions
    ) where

import          Text.Regex.PCRE.Heavy as PCRE
import          Text.Regex.PCRE.Light.Base as PCREL
import          Language.Haskell.TH.Quote
import          Text.Pandoc (def, WriterOptions(..), enableExtension, Extension(..), pandocExtensions, ReaderOptions(..))
import          Text.Pandoc.Highlighting   (pygments)

-- Case-insensitive regular expression quasi-quoter, used in unphone
caselessre :: QuasiQuoter
caselessre = PCRE.mkRegexQQ [utf8, caseless]

-- Stolen from lib/Hakyll/Web/Pandoc.hs
hblogPandocWriterOptions :: WriterOptions
hblogPandocWriterOptions = def
    { -- This option causes literate haskell to be written using '>' marks in
      -- html, which I think is a good default.
      writerExtensions = enableExtension Ext_smart pandocExtensions
    , -- We want to have hightlighting by default, to be compatible with earlier
      -- Hakyll releases
      writerHighlightStyle = Just pygments
    }
hblogPandocReaderOptions :: ReaderOptions
hblogPandocReaderOptions = def
    { -- The following option causes pandoc to read smart typography, a nice
      -- and free bonus.
      readerExtensions = enableExtension Ext_smart pandocExtensions
    }

