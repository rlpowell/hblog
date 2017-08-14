{-|
Module      : rectifier
Description : Convert our "wiki-style" markdown links to exact titles/headers/filenames.
Copyright   : (c) Robin Lee Powell, 2017
License     : MIT
Maintainer  : rlpowell@digitalkingdom.org
Stability   : experimental
Portability : POSIX

Check our "wiki-style" markdown links, which match fuzzily in
various ways, to make sure they point at something unique, and
replace them with the exact titles/headers/filenames of the thing
they point at.

See "Wiki Links" in DESIGN-CODE for full details.

-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module HBlog.Rectifier
    ( rectifierMain
    ) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (makeRelative, (</>), takeDirectory, takeBaseName)
import System.Environment (getArgs)
import qualified System.FilePath.Find as F (find, always, extension)
import System.FilePath.Find ((==?))
import Control.Monad (mapM)
import Text.Pandoc (readMarkdown, Inline(..), docTitle, writePlain, writeMarkdown, writerTemplate, runPure, PandocError(..))
import Text.Pandoc.Templates (getDefaultTemplate)
import Text.Pandoc.Walk (query, walk)
import Text.Pandoc.Definition (Pandoc(..), Block(..), nullMeta)
import Network.URI (unEscapeString, isURI)
import Data.List (find, intercalate, sortBy)
import Data.Maybe (isJust)
import Data.Char (toLower)
-- import Debug.Trace
import Text.Regex.PCRE.Heavy ((=~))
import Text.Regex.PCRE.Light (compile, caseless)
import Data.ByteString.Char8 (pack)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)
import Data.Function (on)
import qualified Data.Text as T
import HBlog.Lib

--------------------------------------------------------------------------------

-- Headers and titles from markdown files.
data Target = THeader { tTarget :: String, tFile :: FilePath } | TTitle { tTarget :: String, tFile :: FilePath } deriving (Eq, Ord, Show)

isHeader :: Target -> Bool
isHeader (THeader _ _) = True
isHeader _ = False

isTitle :: Target -> Bool
isTitle (TTitle _ _) = True
isTitle _ = False


--------------------------------------------------------------------------------
rectifierMain :: IO ()
rectifierMain = do
  args <- getArgs
  case args of
    indir:outdir:[] -> walkTree indir outdir
    _               -> putStrLn "Need exactly two arguments, input directory and output directory."

-- Walk the tree of input files, rewriting as necessary the links in
-- each file to point to the full text of link targets that actually
-- exist.
--
-- A mildly unfortunate issue is that we walk the files twice; once
-- to get all the targets and again to munge the links to them.
-- It's not obvious that that's fixable, though, and it's not like
-- we're talking about thousands of files.
walkTree :: String -> String -> IO ()
walkTree indir outdir = do
  -- Grab the possible link targets
  targets <- targetPrep indir
  -- error $ "targets: " ++ show targets
  putStrLn $ "Searching for files ending in .md"
  files <- F.find F.always (F.extension ==? ".md") indir 
  putStrLn $ "Files found: " ++ (intercalate " " files)
  _ <- mapM (handleFile indir outdir targets) files
  return ()

-- Walks the file tree running readTargets
targetPrep :: FilePath -> IO [Target]
targetPrep indir = do
  files <- F.find F.always (F.extension ==? ".md") indir 
  targets <- mapM readTargets files
  return $ filter (\x -> (tTarget x) /= "") $ concat targets

-- Gather all the titles and headers from all the files, along with
-- their file namers, and turn them into Targets
--
-- We don't care about anything else about these files; we're just
-- making sure that each link points to a valid target.
readTargets :: FilePath -> IO [Target]
readTargets file = do
  body <- readFile file
  let pandocEither = runPure $ readMarkdown hblogPandocReaderOptions $ T.pack body
  return $ concat [titleQuery pandocEither file, headersQuery pandocEither file]

-- Turn titles into Targets
titleQuery :: Either PandocError Pandoc -> FilePath -> [Target]
titleQuery (Right (Pandoc meta _)) file =
  [TTitle { tFile = file, tTarget = target }]
  where
    target = case runPure $ writePlain hblogPandocWriterOptions $ Pandoc nullMeta $ [Plain $ docTitle meta] of
                  Left (PandocSomeError err)  -> "rectifier titleQuery: unknown error: " ++ err
                  Left _ -> "rectifier titleQuery: unknown error!"
                  Right item'              -> T.unpack item'
titleQuery (Left e) file = error $ "Pandoc error! on file " ++ file ++ ": " ++ (show e)

-- Turn headers into Targets
headersQuery :: Either PandocError Pandoc -> FilePath -> [Target]
headersQuery (Right x) file = map (\str -> THeader { tFile = file, tTarget = str}) $ query getHeaders x
headersQuery (Left e) file = error $ "Pandoc error! on file " ++ file ++ ": " ++ (show e)

-- Pull headers out as strings
getHeaders :: Block -> [String]
getHeaders (Header _ _ xs) =
  [headers]
  where
    headers = case runPure $ writePlain hblogPandocWriterOptions $ Pandoc nullMeta $ [Plain xs] of
                  Left (PandocSomeError err)  -> "rectifier getHeaders: unknown error: " ++ err
                  Left _ -> "rectifier getHeaders: unknown error!"
                  Right item'              -> T.unpack item'
getHeaders _ = []

-- Basically a wrapper for rectifie that does the IO bits; the code
-- below this is pure
handleFile :: FilePath -> FilePath -> [Target] -> FilePath -> IO ()
handleFile indir outdir targets file = do
  putStrLn $ "Processing file " ++ file
  let shortname = makeRelative indir file
  body <- readFile $ indir </> shortname
  Right mdTemplate <- getDefaultTemplate Nothing "markdown"
  let newBody = rectify body file targets mdTemplate in do
    _ <- createDirectoryIfMissing True (takeDirectory $ (outdir </> shortname))
    _ <- writeFile (outdir </> shortname) newBody
    return ()

-- Read, munge, and rebuild the markdown file we're working on
rectify :: String -> FilePath -> [Target] -> String -> String
rectify body file targets mdTemplate =
  let pandocEither = runPure $ readMarkdown hblogPandocReaderOptions $ T.pack body
      newPandoc = linksWalk pandocEither file targets
      in
        case runPure $ writeMarkdown hblogPandocWriterOptions { writerTemplate = Just mdTemplate } newPandoc of
                  Left (PandocSomeError err)  -> "rectifier rectify: unknown error: " ++ err
                  Left _ -> "rectifier rectify: unknown error!"
                  Right item'              -> T.unpack item'

-- Walk the Pandoc
linksWalk :: Either PandocError Pandoc -> FilePath -> [Target] -> Pandoc
linksWalk (Right x) file targets = walk (replaceLink file targets) x
linksWalk (Left e) file _ = error $ "Pandoc error! on file " ++ file ++ ": " ++ (show e)

-- Return true if the target is a header in the specified file
isFileHeader :: FilePath -> Target -> Bool
isFileHeader file target = isHeader target && tFile target == file

-- Replaces the given link; mostly this is just a wrapper for
-- findTarget, but there's some complexity if it's a fragment style
-- link
replaceLink :: FilePath -> [Target] -> Inline -> Inline
replaceLink file targets linky@(Link x y (ickyLinkStr, z)) =
  let linkStr = unEscapeString ickyLinkStr in
    -- Step 1 from "Rectification" in DESIGN-CODE : check if it
    -- looks like a URL or an absolute path within our blog, and
    -- ignore it in those cases
    if isURI linkStr || head linkStr == '/' then
      linky
    else
      if (length $ filter (== '#') linkStr) == 1 then
        -- If it's a fragment (a # link), first check the left side
        -- of the # and if that matches a title, look in that file
        -- for header matches
        let maybeGoodTarget = findTarget (fst $ span (/= '#') linkStr) isTitle targets in
            case maybeGoodTarget of
              Nothing -> error $ "Failed to find valid main target (the part before #) for link \"" ++ linkStr ++ "\" in file " ++ file
              Just goodTarget ->
                -- Now that we have the file, look for the fragment
                -- in the headers of that file
                let maybeGoodFragTarget = findTarget (tail $ snd $ span (/= '#') linkStr) (isFileHeader (tFile goodTarget)) targets in
                  case maybeGoodFragTarget of
                    Nothing -> error $ "Failed to find valid fragment target (the part after #) for link \"" ++ linkStr ++ "\" in file " ++ file
                    Just goodFragTarget -> Link x y ((tTarget goodTarget) ++ "#" ++ (tTarget goodFragTarget), z)
      else
        -- No # ; try all the internal file headers first, and then
        -- all the titles.
        let twoMaybes = find isJust [
                  findTarget linkStr (isTitle) targets
                , findTarget linkStr (isFileHeader file) targets
              ] in
            -- FIXME: There *must* be a more idiomatic way to do this
            case twoMaybes of
              Nothing -> noTarget linkStr file targets
              Just Nothing -> noTarget linkStr file targets
              Just (Just goodTarget) -> Link x y ((tTarget goodTarget), z)
replaceLink _ _ x = x

-- Return true if the target is visible to us, that is, is a title
-- or is a header in this same file.
isTitleOrFileHeader :: FilePath -> Target -> Bool
isTitleOrFileHeader file target = isFileHeader file target || isTitle target

levenshteinTargetSort :: String -> [Target] -> FilePath -> [Char]
levenshteinTargetSort linkStr targets file =
  intercalate "\n" $
    sortBy (compare `on` (levenshteinDistance defaultEditCosts) linkStr) $
    map tTarget $
    filter (isTitleOrFileHeader file) targets

noTarget :: [Char] -> [Char] -> [Target] -> a
noTarget linkStr file targets = error $ "Failed to find valid target for link \"" ++ linkStr ++ "\" in file " ++ file ++
  "\n\nPossible targets include:\n\n" ++ (levenshteinTargetSort linkStr targets file) ++ "\n\n"

makeFuzzy :: String -> String
makeFuzzy input = ".*" ++ (intercalate ".*" $ words input) ++ ".*"

leftRegex :: String -> String -> Bool
leftRegex left right = right =~ (compile (pack left) [caseless])

-- This is where we actually walk through the steps from
-- "Rectification" in DESIGN-CODE (except step 1); details are there.
findTarget :: String -> (Target -> Bool) -> [Target] -> Maybe Target
findTarget linkStr myFilter targets =
  -- We want to stop immediately once we find something that works.
  let twoMaybes = find isJust [
          -- Step 2
          applyToBothAndFind id                 tTarget                         (==) linkStr (filter myFilter targets)
          -- Step 3
        , applyToBothAndFind (map toLower)      ((map toLower) . tTarget)       (==) linkStr (filter myFilter targets)
          -- Step 4
        , applyToBothAndFind trimMD             (trimMD . tFile)                (==) linkStr (filter myFilter targets)
          -- Step 5
        , applyToBothAndFind trimMDLower        (trimMDLower . tFile)           (==) linkStr (filter myFilter targets)
          -- Step 6
        , applyToBothAndFind makeFuzzy          tTarget                         (leftRegex) linkStr (filter myFilter targets)
        , applyToBothAndFind makeFuzzy          tFile                           (leftRegex) linkStr (filter myFilter targets)
        , Nothing
        ] in
          -- FIXME: There *must* be a more idiomatic way to do this
          case twoMaybes of
            Nothing -> Nothing
            Just Nothing -> Nothing
            Just (Just x) -> Just x

-- Trim ".md" from the file name
trimMD :: FilePath -> String
trimMD fname = let maybeStripped = T.stripSuffix ".md" $ T.pack $ (takeBaseName fname)
  in maybe fname T.unpack maybeStripped

-- Trim ".md" from the file name and force it to lowercase
trimMDLower :: FilePath -> String
trimMDLower fname = map toLower (trimMD (takeBaseName fname))

-- Take left and right string munging functions, a comparison
-- function, the link string we want to rectify, and the targets to
-- use to rectify it. Applies the munging functions to the
-- appropriate sides of the comparison, and runs the comparison
-- function.
applyToBothAndFind :: (String -> String) -> (Target -> String) -> (String -> String -> Bool) -> String -> [Target] -> Maybe Target
applyToBothAndFind _ _ _ _ [] = Nothing
applyToBothAndFind mungeLeft mungeRight comp linkStr xs =
  let findings = applyToBothAndFindInner mungeLeft mungeRight comp linkStr xs in
    if length findings > 1 then
      error $ "Found more than one match for \"" ++ linkStr ++ "\": " ++ (intercalate " " $ map tFile findings)
    else
    if length findings < 1 then
      Nothing
    else
      Just $ head findings

applyToBothAndFindInner :: (String -> String) -> (Target -> String) -> (String -> String -> Bool) -> String -> [Target] -> [Target]
applyToBothAndFindInner _ _ _ _ [] = []
applyToBothAndFindInner mungeLeft mungeRight comp linkStr (target:xs) =
  -- trace ("mlls: " ++ (mungeLeft linkStr)) $
  -- trace ("mrt: " ++ (mungeRight target)) $
  if (mungeLeft linkStr) `comp` (mungeRight target) then
    [target] ++ (applyToBothAndFindInner mungeLeft mungeRight comp linkStr xs)
  else
    applyToBothAndFindInner mungeLeft mungeRight comp linkStr xs
