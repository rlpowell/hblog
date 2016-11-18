--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import System.FilePath (makeRelative, (</>), takeDirectory)
import System.Environment (getArgs)
import qualified System.FilePath.Find as F (find, always, extension)
import System.FilePath.Find ((==?))
import System.Directory (createDirectoryIfMissing)
import Text.Pandoc (readerStandalone, def, writeMarkdown, writeHtmlString, writerStandalone, writerTemplate, Pandoc(..))
import Text.Pandoc.Templates (getDefaultTemplate)
import Text.Pandoc.Readers.TikiWiki (readTikiWiki)
import Data.List (intercalate)
import qualified Data.Text as T

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    indir:outdir:[] -> walkTree indir outdir
    _               -> putStrLn "Need exactly two arguments, input directory and output directory."

walkTree :: String -> String -> IO ()
walkTree indir outdir = do
  putStrLn $ "Searching for files ending in .tiki"
  files <- F.find F.always (F.extension ==? ".tiki") indir 
  putStrLn $ "Files found: " ++ (intercalate " " files)
  _ <- mapM (handleFile indir outdir) files
  return ()

handleFile :: FilePath -> FilePath -> FilePath -> IO ()
handleFile indir outdir file = do
  putStrLn $ "Processing file " ++ file
  let shortname = makeRelative indir file
  let shortnameBase = maybe (error $ "Can't strip .tiki from file " ++ file) T.unpack $
        T.stripSuffix ".tiki" $ T.pack $ makeRelative indir file
  body <- readFile $ indir </> shortname
  if T.isInfixOf "~tc~" (T.pack body) then
    error $ "We do not support ~tc~ style comments, in file " ++ file ++ " , for the reasons we described at https://github.com/jgm/pandoc/issues/2552"
  else
    do
      Right mdTemplate <- getDefaultTemplate Nothing "markdown"
      let pandoc = unTiki file mdTemplate body in do
        _ <- createDirectoryIfMissing True (takeDirectory $ (outdir </> shortname))
        _ <- writeFile (outdir </> shortnameBase ++ ".md") $ writeMarkdown def { writerStandalone = True , writerTemplate = mdTemplate } pandoc
        _ <- writeFile (outdir </> shortnameBase ++ ".html") $ writeHtmlString def { writerStandalone = True , writerTemplate = mdTemplate } pandoc
        return ()

unTiki :: FilePath -> String -> String -> Pandoc
unTiki filePath mdTemplate body =
  let pandocEither = readTikiWiki def { readerStandalone = True } body in
    case pandocEither of
      Left e -> error $ "Pandoc error! on file " ++ filePath ++ ": " ++ (show e)
      Right doc -> doc

--     newBody
--   else
--     unTiki newBody
--   where
--     -- FIXME: For bonus points: bail on dead links!
--     newBody = 
--       -- Bold   __text__
--       -- Centered       ::text::
--       -- Italic ''text''
--       -- Monospaced     -+text+-
--       -- Underlined     ===text===
--       -- Text box       ^text^
--       -- Deleted        --text--
--       -- Display syntax ~np~__not bold__~/np~
--       -- superscript: {TAG(tag=>sup)}super{TAG} / {SUP()}...{SUP}
--       -- subscript: {TAG(tag=>sub)}sub{TAG} / {SUB()}...{SUB}
--       -- headings: ! , !! , ...
--       -- line break: %%%
--       -- square brackets: [[not a link]
--       -- non-breaking space: ~hs~
--       -- html comments: ~hc~...~/hc~
--       -- code: {CODE()}...{CODE}
--       -- internal link: ((PluginVersions|The Versions Plugin)) ; ((Using Wiki Pages|#The_Wiki_Menu|The Wiki Menu)) for anchors
--       -- Link to a heading on the same page: [#Related_Pages|Related Pages]
--       -- Link to a Web page: [http://www.somesite.org] or [http://www.somesite.org|Some Site]
--       -- Table of contents: {maketoc}
--       -- Bulleted list: *item
--       -- Numbered list: #item
--       -- Definition list: ;term:definition
--       -- Horizontal line: ----
-- 
--       -- Special characters: ~126~
--       -- For this one, maybe just bail and alert the user.
-- 
--       -- Tables: ||row1-column1|row1-column2||row2-column1|row2-column2||
--       -- For this one, maybe just bail and alert the user.
-- 
--       -- Image display: {img attId="39" imalign="right" link="http://info.tikiwiki.org" alt="Panama Hat"} / {img attId="37", thumb="mouseover", styleimage="border", desc="desc", max="150"} / {img src="img/wiki_up/393px-Pears.jpg" thumb="y" imalign="center" stylebox="border" button="y" desc="Pretty pears" max="200" rel="box"}
--       -- For this one, maybe just bail and alert the user.
-- 
--       -- ~white,white:text~~ ; colored text
--       gsub [re|~~([^,~:]*):(.*)~~|] (\(color:text:_) -> "<div style='color: " ++ color ++ "'>" ++ text ++ "</div>" :: String ) $
--       gsub [re|~~([^,~:]*),([^~]*):(.*)~~|] (\(color:bgcolor:text:_) -> "<div style='color: " ++ color ++ "; background-color: " ++ bgcolor ++ "'>" ++ text ++ "</div>" :: String ) $
--       -- ~tc~...~/tc~ ; tiki comments ; we turn these into YAML metadata
--       -- comments per https://groups.google.com/forum/#!topic/pandoc-discuss/xL05J-_4YHs ; they look like:
--       -- ---
--       -- # foo
--       -- ---
--       --
--       gsub [re|~tc~((\s|\S)*?)~/tc~|] (\(text:_) -> "\n\n---\n" ++ (unlines $ map (\x -> "# " ++ x) $ lines text) ++ "\n---\n\n" :: String) $
--       oldBody
