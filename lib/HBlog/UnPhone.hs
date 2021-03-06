{-|
Module      : unphone
Description : Convert partial-markdown using special phone-friendly markup to our regular "wiki-style" markdown.  See "Phone Shortcuts" in DESIGN-CODE for full details.
Copyright   : (c) Robin Lee Powell, 2016
License     : MIT
Maintainer  : rlpowell@digitalkingdom.org
Stability   : experimental
Portability : POSIX

See "Phone Shortcuts" in DESIGN-CODE for full details.

-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module HBlog.UnPhone where
import          System.Directory (createDirectoryIfMissing, doesFileExist)
import          System.FilePath
-- import          System.Posix.Files
import          System.Environment
import          Text.Regex.PCRE.Heavy as PCRE
import          System.FilePath.Find
import          HBlog.Lib

--------------------------------------------------------------------------------
unPhoneMain :: IO ()
unPhoneMain = do
  args <- getArgs
  case args of
    indir:outdir:[] -> maybeTree indir outdir
    _             -> putStrLn "Need exactly two arguments, input directory and output directory."

maybeTree :: String -> String -> IO ()
maybeTree inthing outthing = do
  isInFile <- doesFileExist inthing
  if isInFile then
    handleFile inthing outthing
  else
    walkTree inthing outthing

walkTree :: String -> String -> IO ()
walkTree indir outdir = do
  files <- find always (extension ==? ".md") indir 
  _ <- mapM (handleFileAndDir indir outdir) files
  return ()

handleFileAndDir :: FilePath -> FilePath -> FilePath -> IO ()
handleFileAndDir indir outdir fname = do
  let shortname = makeRelative indir fname
  _ <- createDirectoryIfMissing True (takeDirectory $ (outdir </> shortname))
  handleFile (indir </> shortname) (outdir </> shortname)

handleFile :: FilePath -> FilePath -> IO ()
handleFile infile outfile = do
  body <- readFile $ infile
  let newBody = unPhone body in do
    _ <- writeFile outfile newBody
    return ()

unPhone :: String -> String
unPhone oldBody =
  if newBody == oldBody then
    newBody
  else
    unPhone newBody
  where
    -- The list of gsubs starts at the bottom and works its way up
    newBody = 
      -- Sub: qwl x qw                [x](x)
      PCRE.gsub [caselessre|(\s+)qwl\s+(.*?)\s+qw(\s+|[.,;]|$)|] sub_link_no_desc $
      -- Sub: qwl x qwu y qw          [x](y)
      PCRE.gsub [caselessre|(\s+)qwl\s+(.*?)\s+qwu\s+(.*?)\s+qw(\s+|[.,;]|$)|] sub_link_with_desc $
      oldBody
    sub_link_no_desc :: [String] -> String
    sub_link_no_desc (ls:url:ts:_) = mconcat [ ls, "[", url, "]", "(", url, ")", ts ] :: String
    sub_link_no_desc _ = error "Called regex function sub_link_no_desc with the wrong stuff in unphone; this is a bug in either Text.Regex.PCRE.Heavy or Text.Regex.PCRE.Light"
    sub_link_with_desc :: [String] -> String
    sub_link_with_desc (ls:text:url:ts:_) = mconcat [ ls, "[", text, "]", "(", url, ")", ts ] :: String
    sub_link_with_desc _ = error "Called regex function sub_link_with_desc with the wrong stuff in unphone; this is a bug in either Text.Regex.PCRE.Heavy or Text.Regex.PCRE.Light"

