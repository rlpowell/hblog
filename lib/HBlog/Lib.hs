{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HBlog.Lib
    ( caselessre
    ) where

import          Text.Regex.PCRE.Heavy as PCRE
import          Text.Regex.PCRE.Light.Base as PCREL
import          Language.Haskell.TH.Quote

caselessre :: QuasiQuoter
caselessre = PCRE.mkRegexQQ [utf8, caseless]
