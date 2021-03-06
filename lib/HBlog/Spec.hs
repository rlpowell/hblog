{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HBlog.Spec
    ( specMain
    ) where

import Test.Hspec
-- import Test.QuickCheck
import System.Process
import System.Exit
import System.Directory (getCurrentDirectory, setCurrentDirectory, getDirectoryContents, doesDirectoryExist, removeDirectoryRecursive, createDirectoryIfMissing)
import Control.Monad
import GHC.Generics
import Data.Yaml
import System.FilePath

data AutoTest = AutoTest { description :: String, ttype :: String, tstderr :: String } deriving (Ord, Eq, Show, Generic)

instance FromJSON AutoTest where
  parseJSON (Object v) =
    AutoTest <$>
    v .:   "description"                        <*>
    v .:?  "type"              .!= "success"   <*>
    v .:?  "stderr"             .!= ""
  parseJSON _ = fail "Failed to parse in AutoTest"

-- Run a command and consider a good exit value a success; the
-- reason this is worth its own function is that we have to make
-- sure we fail in the right way, or we get the output as a
-- "foo\nbar\n" sort of string.
runCmd :: AutoTest -> String -> [String] -> String -> Bool -> IO ()
runCmd autoTest cmd args stdin shouldFail = do
  (exit, stdout, stderr) <- readProcessWithExitCode cmd args stdin
  if shouldFail then
    do
      exit `shouldNotBe` ExitSuccess
      (stdout ++ stderr) `shouldContain` (tstderr autoTest)
      -- (stderr =~ (compile (pack (tstderr autoTest)) [])) `shouldBe` True
  else
    if exit /= ExitSuccess then
      -- Gives us the properly formatted output
      expectationFailure $ 
        "stdout: \n\n" ++ stdout ++
        "\n\nstderr: \n\n" ++ stderr
    else
      exit `shouldBe` ExitSuccess

hblogRunCmd :: Bool -> AutoTest -> FilePath -> FilePath -> String -> [String] -> IO ()
hblogRunCmd shouldFail autoTest appDir mainDir cmd args = do
  setCurrentDirectory appDir
  (exit, stdout, stderr) <- readProcessWithExitCode cmd args ""
  setCurrentDirectory mainDir
  if shouldFail then
    do
      exit `shouldNotBe` ExitSuccess
      (stdout ++ stderr) `shouldContain` (tstderr autoTest)
      -- (stderr =~ (compile (pack (tstderr autoTest)) [])) `shouldBe` True
  else
    if exit /= ExitSuccess then
      -- Gives us the properly formatted output
      expectationFailure $ 
        "stdout: \n\n" ++ stdout ++
        "\n\nstderr: \n\n" ++ stderr
    else
      exit `shouldBe` ExitSuccess

rm_rf :: FilePath -> IO ()
rm_rf dir = do
  exists <- doesDirectoryExist dir
  if exists then
    removeDirectoryRecursive dir
  else
    return ()

hblogTest :: Bool -> AutoTest -> FilePath -> SpecWith ()
hblogTest shouldFail autoTest dir = it (description autoTest) $ do
  mainDir <- getCurrentDirectory
  rm_rf $ dir </> "_site"
  rm_rf $ dir </> "_cache"
  rm_rf $ dir </> "templates"
  callProcess "cp" ["-pr", (mainDir </> "templates"), (dir </> "templates")]

  hblogRunCmd shouldFail autoTest dir mainDir "hblog" ["build"]

  rm_rf $ dir </> "_cache"
  rm_rf $ dir </> "templates"

  if (ttype autoTest) == "hblog" then
    do
      hblogRunCmd shouldFail autoTest dir mainDir "diff" ["-u", "-r", (mainDir </> dir </> "wanted"), (mainDir </> dir </> "_site")]
  else
    return ()

pathKids :: Bool -> FilePath -> IO [FilePath]
pathKids full dir = do
  namesAll <- getDirectoryContents dir
  let realNames = filter (\x -> x /= "." && x /= "..") namesAll
  if full then
    let fullNames = map (dir </>) realNames in
      return fullNames
  else
    return realNames

genTest :: String -> AutoTest -> FilePath -> SpecWith ()
genTest prog autoTest dir = it (description autoTest) $ do
  let indir = dir </> "in"
  let outdir = dir </> "out"
  let wanteddir = dir </> "wanted"
  outExists <- doesDirectoryExist outdir
  if outExists then
    do _ <- removeDirectoryRecursive outdir
       return ()
  else
    do return ()
  _ <- createDirectoryIfMissing True outdir
  if (ttype autoTest) == "failure" then
    runCmd autoTest prog [indir, outdir] "" True
  else
    do
      runCmd autoTest prog [indir, outdir] "" False
      runCmd autoTest "diff" ["-r", wanteddir, outdir] "" False

genDirTests :: String -> FilePath -> Spec
genDirTests prog dir = do
  eitherAutoTest <- runIO $ (decodeFileEither (dir </> "config.yaml") :: IO (Either ParseException AutoTest))
  case eitherAutoTest of
    Left parseBad -> error $ "\n\nYAML parse fail on config.yaml in " ++ dir ++ " with error " ++ (show parseBad) ++ "\n\n"
    Right autoTest -> if (ttype autoTest) == "hblog" then
                        hblogTest False autoTest dir
                      else
                        if (ttype autoTest) == "hblog-fail" then 
                          hblogTest True autoTest dir
                        else
                          genTest prog autoTest dir

genProgDirTests :: FilePath -> Spec
genProgDirTests prog = do
  dirNames <- runIO $ pathKids True ("tests" </> prog)
  forM_ dirNames $ genDirTests prog

genProgTests :: FilePath -> Spec
genProgTests prog = do
  describe ("the " ++ prog ++ " program") $ genProgDirTests prog

specMain :: IO ()
specMain = do
  dirNames <- pathKids False "tests"
  hspec $ do
    forM_ dirNames $ genProgTests
