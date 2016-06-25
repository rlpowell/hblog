--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import           Data.Monoid (mappend)
import           Data.List
import           Text.Regex.PCRE.Heavy as PCRE
import           Data.Maybe (listToMaybe)
import           System.FilePath
import           System.Posix.Files
import           System.Environment

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  let filePath = head args in do
    body <- readFile filePath
    let newBody = unTiki body in do
      _ <- writeFile (filePath ++ ".hblog.tmp") newBody
      -- rename (filePath ++ ".hblog.tmp") filePath
      return ()

unTiki :: String -> String
unTiki oldBody = if newBody == oldBody then
    newBody
  else
    unTiki newBody
  where
    -- FIXME: For bonus points: bail on dead links!
    newBody = 
      -- Bold   __text__
      -- Centered       ::text::
      -- Italic ''text''
      -- Monospaced     -+text+-
      -- Underlined     ===text===
      -- Text box       ^text^
      -- Deleted        --text--
      -- Display syntax ~np~__not bold__~/np~
      -- superscript: {TAG(tag=>sup)}super{TAG} / {SUP()}...{SUP}
      -- subscript: {TAG(tag=>sub)}sub{TAG} / {SUB()}...{SUB}
      -- headings: ! , !! , ...
      -- line break: %%%
      -- square brackets: [[not a link]
      -- non-breaking space: ~hs~
      -- html comments: ~hc~...~/hc~
      -- code: {CODE()}...{CODE}
      -- internal link: ((PluginVersions|The Versions Plugin)) ; ((Using Wiki Pages|#The_Wiki_Menu|The Wiki Menu)) for anchors
      -- Link to a heading on the same page: [#Related_Pages|Related Pages]
      -- Link to a Web page: [http://www.somesite.org] or [http://www.somesite.org|Some Site]
      -- Table of contents: {maketoc}
      -- Bulleted list: *item
      -- Numbered list: #item
      -- Definition list: ;term:definition
      -- Horizontal line: ----

      -- Special characters: ~126~
      -- For this one, maybe just bail and alert the user.

      -- Tables: ||row1-column1|row1-column2||row2-column1|row2-column2||
      -- For this one, maybe just bail and alert the user.

      -- Image display: {img attId="39" imalign="right" link="http://info.tikiwiki.org" alt="Panama Hat"} / {img attId="37", thumb="mouseover", styleimage="border", desc="desc", max="150"} / {img src="img/wiki_up/393px-Pears.jpg" thumb="y" imalign="center" stylebox="border" button="y" desc="Pretty pears" max="200" rel="box"}
      -- For this one, maybe just bail and alert the user.

      -- ~white,white:text~~ ; colored text
      gsub [re|~~([^,~:]*):(.*)~~|] (\(color:text:_) -> "<div style='color: " ++ color ++ "'>" ++ text ++ "</div>" :: String ) $
      gsub [re|~~([^,~:]*),([^~]*):(.*)~~|] (\(color:bgcolor:text:_) -> "<div style='color: " ++ color ++ "; background-color: " ++ bgcolor ++ "'>" ++ text ++ "</div>" :: String ) $
      -- ~tc~...~/tc~ ; tiki comments ; we turn these into YAML metadata
      -- comments per https://groups.google.com/forum/#!topic/pandoc-discuss/xL05J-_4YHs ; they look like:
      -- ---
      -- # foo
      -- ---
      --
      gsub [re|~tc~((\s|\S)*?)~/tc~|] (\(text:_) -> "\n\n---\n" ++ (unlines $ map (\x -> "# " ++ x) $ lines text) ++ "\n---\n\n" :: String) $
      oldBody
