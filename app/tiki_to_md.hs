--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import System.FilePath (makeRelative, (</>), takeDirectory)
import System.Environment (getArgs)
import qualified System.FilePath.Find as F (find, always, extension)
import System.FilePath.Find ((==?))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Text.Pandoc (readerStandalone, def, writeMarkdown, writeHtmlString, writerStandalone, writerTemplate, Pandoc(..))
import Text.Pandoc.Templates (getDefaultTemplate)
import Text.Pandoc.Readers.TikiWiki (readTikiWiki)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    indir:outdir:[] -> maybeTree indir outdir
    _               -> putStrLn "Need exactly two arguments, input directory and output directory."

maybeTree :: String -> String -> IO ()
maybeTree inthing outthing = do
  isInFile <- doesFileExist inthing
  if isInFile then
    handleFile inthing outthing Nothing
  else
    walkTree inthing outthing

walkTree :: String -> String -> IO ()
walkTree indir outdir = do
  putStrLn $ "Searching for files ending in .tiki"
  files <- F.find F.always (F.extension ==? ".tiki") indir 
  putStrLn $ "Files found: " ++ (intercalate " " files)
  _ <- mapM (handleFileAndDir indir outdir) files
  return ()

handleFileAndDir :: FilePath -> FilePath -> FilePath -> IO ()
handleFileAndDir indir outdir fpath = do
  let shortname = makeRelative indir fpath
  let shortnameBase = maybe (error $ "Can't strip .tiki from file name " ++ fpath) T.unpack $
        T.stripSuffix ".tiki" $ T.pack $ shortname
  _ <- createDirectoryIfMissing True (takeDirectory $ (outdir </> shortname))
  handleFile (indir </> shortname) (outdir </> shortnameBase ++ ".md") $ Just (outdir </> shortnameBase ++ ".html")

handleFile :: FilePath -> FilePath -> Maybe FilePath -> IO ()
handleFile infile outfileMD outfileHTML = do
  putStrLn $ "Processing file " ++ infile
  body <- readFile $ infile
  if T.isInfixOf "~tc~" (T.pack body) then
    error $ "We do not support ~tc~ style comments, in file " ++ infile ++ " , for the reasons I (rlpowell) described at https://github.com/jgm/pandoc/issues/2552 .  Erroring out to stop you from accidentally revealing something secret."
  else
    do
      Right mdTemplate <- getDefaultTemplate Nothing "markdown"
      let pandoc = unTiki infile body in do
        _ <- writeFile outfileMD $ writeMarkdown def { writerStandalone = True , writerTemplate = mdTemplate } pandoc
        if isJust outfileHTML then do
          _ <- writeFile (fromJust outfileHTML) $ writeHtmlString def { writerStandalone = True , writerTemplate = mdTemplate } pandoc
          return ()
        else
          return ()

unTiki :: FilePath -> String -> Pandoc
unTiki filePath body =
  let pandocEither = readTikiWiki def { readerStandalone = True } body in
    case pandocEither of
      Left e -> error $ "Pandoc error! on file " ++ filePath ++ ": " ++ (show e)
      Right doc -> doc
