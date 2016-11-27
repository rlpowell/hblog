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
    error $ "We do not support ~tc~ style comments, in file " ++ file ++ " , for the reasons I (rlpowell) described at https://github.com/jgm/pandoc/issues/2552 .  Erroring out to stop you from accidentally revealing something secret."
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
