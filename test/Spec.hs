{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

import Test.Hspec
-- import Test.QuickCheck
import System.Process
import System.Exit
import System.Directory
import Control.Monad
import GHC.Generics
import Data.Yaml (decodeFile, FromJSON)
import System.FilePath
import System.Environment

data AutoTest = AutoTest { description :: String } deriving (Ord, Eq, Show, Generic)

instance FromJSON AutoTest

-- Run a command and consider a good exit value a success; the
-- reason this is worth its own function is that we have to make
-- sure we fail in the right way, or we get the output as a
-- "foo\nbar\n" sort of string.
runCmd :: String -> [String] -> String -> IO ()
runCmd cmd args stdin = do
  (exit, stdout, stderr) <- readProcessWithExitCode cmd args stdin
  if exit /= ExitSuccess then
    -- Gives us the properly formatted output
    expectationFailure $ 
      "stdout: \n\n" ++ stdout ++
      "\n\nstderr: \n\n" ++ stderr
  else
    exit `shouldBe` ExitSuccess

pathKids :: Bool -> FilePath -> IO [FilePath]
pathKids full dir = do
  namesAll <- getDirectoryContents dir
  let realNames = filter (\x -> x /= "." && x /= "..") namesAll
  if full then
    let fullNames = map (dir </>) realNames in
      return fullNames
  else
    return realNames

genTest :: String -> String -> FilePath -> FilePath -> SpecWith ()
genTest prog desc dir file = it desc $ do
  let indir = dir </> "in"
  let outdir = dir </> "out"
  let wanteddir = dir </> "wanted"
  _ <- createDirectoryIfMissing True outdir
  runCmd prog [indir, outdir] ""
  runCmd "diff" ["-r", outdir, wanteddir] ""

genDirTests :: String -> FilePath -> Spec
genDirTests prog dir = do
  autoTest <- runIO $ (decodeFile (dir </> "config.yaml") :: IO (Maybe AutoTest))
  fileShortNames <- runIO $ pathKids False (dir </> "in")
  let desc = maybe ("ARGH YAML PARSE FAIL IN " ++ dir) description autoTest
  forM_ fileShortNames $ genTest prog desc dir

genProgDirTests :: FilePath -> Spec
genProgDirTests prog = do
  dirNames <- runIO $ pathKids True ("tests" </> prog)
  forM_ dirNames $ genDirTests prog

genProgTests :: FilePath -> Spec
genProgTests prog = do
  describe ("the " ++ prog ++ " program") $ genProgDirTests prog

main :: IO ()
main = do
  dirNames <- pathKids False "tests"
  hspec $ do
    forM_ dirNames $ genProgTests
