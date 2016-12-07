--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Main where
import           Data.List
import           Hakyll
import           System.FilePath
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Pandoc                     (writePlain, def, nullMeta)
import           Text.Pandoc.Definition          (Pandoc(..), Inline(..), Block(..))
import           Text.Pandoc.Walk                (walk, query)
import           Data.Maybe                      (isJust, fromJust)
import           Debug.Trace
import           Network.URI                     (unEscapeString)
import           Hakyll.Core.Identifier          (toFilePath)
import           Data.Time.Clock                 (UTCTime)
import           System.Process                  (readProcessWithExitCode)
import           System.Exit                     (ExitCode(..))
import qualified Data.Map                        as M
import           Data.Either                     (either)
import           Control.Monad.IO.Class          (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Time.Format (TimeLocale, formatTime, parseTimeM, defaultTimeLocale)
import Control.Monad (liftM, msum)
import Data.Ord (comparing)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import           Data.Time.Locale.Compat       (TimeLocale, defaultTimeLocale) 
import Text.Read (readMaybe)
import Data.Maybe (isJust, isNothing, fromJust)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (Hakyll.fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTagsWith myGetTags "posts/**" (fromCapture "tags/*.html")

    categories <- buildTagsWith myGetCategory "posts/**" (fromCapture "categories/*.html")

    ids <- getMatches "posts/**"

    titles <- fmap mconcat $ mapM getTitlePair ids

    gitTimes <- fmap mconcat $ preprocess $ mapM getGitTimes ids

    match "posts/**" $ do
    -- match ("posts/**" .&&. complement "**/index.html") $ do
        route $ setExtension "html"
        compile $ do
            pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions (titleFixer titles)
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags categories gitTimes)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags categories gitTimes)
            >>= relativizeUrls

--     create ["categories.html"] $ do
--         route idRoute
--         compile $ do
--             posts <- (myRecentFirst gitTimes) =<< loadAll "categories/*"
--             let archiveCtx =
--                     listField "posts" (postCtx tags categories gitTimes) (return posts) `mappend`
--                     constField "title" "Archives"            `mappend`
--                     defaultContext
-- 
--             makeItem ""
--                 >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
--                 >>= loadAndApplyTemplate "templates/default.html" archiveCtx
--                 >>= relativizeUrls
-- 
-- 
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- (myRecentFirst gitTimes) =<< loadAll "posts/**"
            let archiveCtx =
                    listField "posts" (postCtx tags categories gitTimes) (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- (myRecentFirst gitTimes) =<< loadAll "posts/**"
            let indexCtx = mconcat [
                    listField "posts" (postCtx tags categories gitTimes) (return posts)
                    , tagCloudField "tags" 120 200 tags
                    , tagCloudField "categories" 120 200 categories
                    , constField "title" "Home"
                    , defaultContext
                    ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    -- Post categories
    tagsRules categories $ \category pattern -> do
        let title = "Posts in category " ++ category

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- (myRecentFirst gitTimes) =<< loadAll pattern
            let ctx = constField "title" title `mappend`
                        listField "posts" (postCtx tags categories gitTimes) (return posts) `mappend`
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- (myRecentFirst gitTimes) =<< loadAll pattern
            let ctx = constField "title" title `mappend`
                        listField "posts" (postCtx tags categories gitTimes) (return posts) `mappend`
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

        -- -- Create RSS feed as well
        -- version "rss" $ do
        --     route   $ setExtension "xml"
        --     compile $ loadAllSnapshots pattern "content"
        --         >>= fmap (take 10) . (myRecentFirst gitTimes)
        --         >>= renderRss (feedConfiguration title) feedCtx


--------------------------------------------------------------------------------

-- Pulls the title metadata out for an item
getTitlePair :: MonadMetadata m => Identifier -> m [(String, FilePath)]
getTitlePair identifier = do
    metadata <- getMetadataField identifier "title"
    if isJust metadata then
      return $ [(fromJust metadata, toFilePath $ identifier)]
    else
      return []

-- Get the first directory name after posts/ , call that the category
myGetCategory :: MonadMetadata m => Identifier -> m [String]
myGetCategory x = return $ take 1 $ drop 1 $ dropWhile (/= "posts") $ splitDirectories $ toFilePath x

-- | Render the category in a link; mostly copied from https://github.com/jaspervdj/hakyll/blob/ea7d97498275a23fbda06e168904ee261f29594e/src/Hakyll/Web/Tags.hs
myCategoryField :: String -> Tags -> Context a
myCategoryField = tagsFieldWith myGetCategory simpleRenderLink (mconcat)

-- | Render one tag link ; copied from https://github.com/jaspervdj/hakyll/blob/ea7d97498275a23fbda06e168904ee261f29594e/src/Hakyll/Web/Tags.hs
simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

-- | Obtain tags from a page in the default way: parse them from the @tags@
-- metadata field.
--
--does stuff with categories and : and stuff; maybe explain in
--DESIGN-CODE
--
-- FIXME: exmaple
myGetTags :: MonadMetadata m => Identifier -> m [String]
myGetTags identifier = do
    tags <- getTags identifier
    let maintags = map (intercalate "+") $ drop 1 $ subsequences $ sort tags
    cat <- myGetCategory identifier
    return $ maintags ++ (map (\x -> (head cat) ++ ":" ++ x) maintags)

getHeaders :: Block -> [(String, String)]
getHeaders (Header _ (slug, _, _) xs) = [(writePlain def $ Pandoc nullMeta $ [Plain xs], slug)]
getHeaders _ = []

titleFixer :: [(String, FilePath)] -> Pandoc -> Pandoc
titleFixer titles doc = 
  let headers = query getHeaders doc in
    walk (titleFixerInternal titles headers) doc

fixPath :: FilePath -> FilePath
fixPath path =
  if takeExtension path == ".md" then
    "/" </> replaceExtension path ".html"
  else
    "/" </> path

titleFixerInternal :: [(String, FilePath)] -> [(String, String)] -> Inline -> Inline
-- titleFixerInternal titles link@(Link _ _ (url, _)) | trace ("url: " ++ (show url) ++ ", titles: " ++ (show titles)) False = undefined
titleFixerInternal titles headers link@(Link x text (rawURL, z)) =
  let fixedURL = unEscapeString rawURL
      maybePath = lookup fixedURL titles
      maybeHeader = lookup fixedURL headers
      fixedText =
        if length text == 0 then
          [Str fixedURL]
        else
          text
  in
    if isJust maybePath then
      let newURL = "/" </> replaceExtension (fromJust maybePath) ".html" in
        Link x fixedText (newURL, z)
    else
      if isJust maybeHeader then
        Link x fixedText ("#" ++ (fromJust maybeHeader), z)
      else
        link
titleFixerInternal _ _ x = x

--------------------------- Date Handling ------------------------------

data GitTimes = GitTimes { gtid :: Identifier, gtlatest :: UTCTime, gtinitial :: UTCTime } deriving (Ord, Eq, Show)

-- Pull the most and least recent git times for a file out of git
-- using the git command line tool
getGitTimes :: Identifier -> IO [GitTimes]
getGitTimes identifier = do
    let path = toFilePath identifier
    -- Strip off the "posts/" part
    let gitFilePath = joinPath $ drop 1 $ splitPath path
    let gitRepoPath = joinPath $ take 1 $ splitPath path

    (exit1, stdout1, stderr1) <- readProcessWithExitCode "git" ["-C", gitRepoPath, "log", "--diff-filter=A", "--follow", "--format=%ai", "-n", "1", "--", gitFilePath] ""
    if exit1 /= ExitSuccess then
      do fail $ "getGitTimes: Couldn't get the date of the first commit for " ++ path ++ ", error: " ++ stderr1
    else
      return ()
    let origtimeMaybe = readMaybe stdout1 :: Maybe UTCTime
    -- let origtime = read (trace ("std: " ++ stdout1 ++ ", " ++ stderr1) stdout1) :: ZonedTime
    if isNothing origtimeMaybe then
      do fail $ "getGitTimes: couldn't parse " ++ stdout1 ++ " as a date, which was supposed to be the orig date for " ++ path ++ ".  This error message might help: " ++ stderr1
    else
      return ()

    (exit2, stdout2, stderr2) <- readProcessWithExitCode "git" ["-C", gitRepoPath, "log", "--format=%ai", "-n", "1", gitFilePath] ""
    if exit2 /= ExitSuccess then
      do fail $ "getGitTimes: Couldn't get the date of the latest/last commit for " ++ path
    else
      return ()
    let curtimeMaybe = readMaybe stdout2 :: Maybe UTCTime
    -- let curtime = read (trace ("std: " ++ stdout2 ++ ", " ++ stderr2) stdout2) :: ZonedTime
    if isNothing curtimeMaybe then
      do fail $ "getGitTimes: couldn't parse " ++ stdout2 ++ " as a date, which was supposed to be the orig date for " ++ path ++ ".  This error message might help: " ++ stderr2
    else
      return ()

    return [GitTimes identifier (fromJust curtimeMaybe) (fromJust origtimeMaybe)]

-- Returns a function that turns an identifier into a string
-- representing a time pullud out of GitTimes
gitTimeToField :: [GitTimes] -> (GitTimes -> UTCTime) -> (Item String -> Compiler String)
gitTimeToField times typeF = \ident -> return $ formatTime defaultTimeLocale "%B %e, %Y" $ getGitTimeUTC (itemIdentifier ident) times typeF

-- Pull a file's time out of the list
getGitTimeUTC :: Identifier -> [GitTimes] -> (GitTimes -> UTCTime) -> UTCTime
getGitTimeUTC ident times typeF =
  let timeList = filter (\x -> ident == (gtid x)) times in
    if length timeList /= 1 then
      -- It's not obvious to me how this could occur even in theory; I'd expect it to error out during getGitTimes
      error $ "getGitTimeUTC: Couldn't find the time for " ++ (show ident) ++ " in GitTimes list " ++ (show times)
    else
      typeF $ head timeList

-- Returns a function that sorts a list of items using getGitTimeUTC
myRecentFirst gtimes = recentFirstWith $ (\x -> getGitTimeUTC x gtimes gtlatest)

-- Check for header metadata with the given names, and fail if
-- found.  In particular, the only date field we allow is orig_date;
-- the rest come from git
failIfMetadatas :: Show a => [String] -> Context a
failIfMetadatas mds = field "check_bad_fields" $ \ident -> do
  imd <- getMetadata $ itemIdentifier ident 
  return $ mconcat $ map (checkMD ident imd) mds
  where
    checkMD ident imd md = do
      let mmdf = lookupString md imd in
        if isJust mmdf then
          error $ "While generating post context, we found you using the field \"" ++ md ++ "\", which we don't support, in file " ++ (show $ itemIdentifier ident)
        else
          ""

-- This only exists because dateFieldWithFallback needs a monadic
-- UTCTime generator
getGitTimeUTCCompiler :: [GitTimes] -> (GitTimes -> UTCTime) -> Identifier -> Compiler UTCTime
getGitTimeUTCCompiler gtimes typeF ident = return $ getGitTimeUTC ident gtimes typeF

-- Construct our $ replacement tags
postCtx :: Tags -> Tags -> [GitTimes] -> Context String
postCtx tags categories gtimes = mconcat [
    -- We always use the last_mod_date from git; if you want to
    -- override that, make a new git commit and use --date
    field  "last_mod_date" (gitTimeToField gtimes gtlatest)
    -- In fact, we explicitely fail if you try to use it from header
    -- metadata
    , failIfMetadatas ["last_mod_date", "date", "published"]
    -- For orig_date, though, if you specify it in the metadata we
    -- take that, whether it parses or not, otherwise we use git
    , dateFieldWithFallback defaultTimeLocale ((flip getMetadataField) "orig_date") (getGitTimeUTCCompiler gtimes gtinitial) "orig_date" "%B %e, %Y"
    , constField "gitTimes" $ show gtimes
    , myCategoryField "category" categories
    , tagsField "tags" tags
    , defaultContext
    ]

--------------------------- Extra Date Handling ------------------------------
-- "With" versions of stuff from
-- hakyll-4.9.0.0/src/Hakyll/Web/Template/List.hs (chronological /
-- recentFirst) and
-- hakyll-4.9.0.0/src/Hakyll/Web/Template/Context.hs (getItemUTC,
-- dateField)

--------------------------------------------------------------------------------
-- | Defines a date field by parsing the given string, if it exists;
-- if not, uses the UTCTime given instead.
--
-- The use case here was "use the metadata field if it exists,
-- otherwise get UTCTime values from git".
dateFieldWithFallback :: TimeLocale                               --  ^ Output time locale
                      -> (Identifier -> Compiler (Maybe String))  --  ^ Function that returns a string to turn into a date;
                                                                  --    if Just, then we use this value, even if it doesn't parse as a date
                      -> (Identifier -> Compiler UTCTime)         --  ^ Function that returns a date to use if the string is a Nothing
                      -> String                                   --  ^ Destination key
                      -> String                                   --  ^ Format to use on the date
                      -> Context a                                --  ^ Resulting context
dateFieldWithFallback locale getDateString getDateUTC key format = field key $ \i -> do
    let ident = itemIdentifier i
    maybeDateString <- getDateString ident
    dateUTC <- getDateUTC ident
    if isJust maybeDateString then
      do
        time <- stringToUTC (fromJust maybeDateString) defaultTimeLocale ident
        return $ formatTime locale format time
    else
      do return $ formatTime locale format dateUTC


--------------------------------------------------------------------------------
-- | Does the actual parsing for dateFieldWithFallback; in
-- hakyll-4.9.0.0/src/Hakyll/Web/Template/Context.hs this was called
-- getItemUTC, but I moved the actual Item part outwards.
stringToUTC :: MonadMetadata m
                => String                    -- ^ The string to try to parse as a date
                -> TimeLocale                -- ^ Output time locale
                -> Identifier                -- ^ Used to generate an error message
                -> m UTCTime                 -- ^ Parsed UTCTime
stringToUTC dateString locale ident = do
    let tryFmt fmt = parseTime' fmt dateString

    maybe empty' return $ msum [tryFmt fmt | fmt <- formats]
  where
    empty'     = fail $ "Hakyll.Web.Template.Context.getItemUTC: " ++
                        "could not parse time for " ++ show ident
    parseTime' = parseTimeM True locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]

--------------------------------------------------------------------------------
-- | Sort pages chronologically, using whatever function you pass to
-- generate the dates.
chronologicalWith :: MonadMetadata m
                  => (Identifier -> UTCTime)  -- ^ Function that returns the date value for an identifier
                  -> [Item a]                 -- ^ The items to sort
                  -> m [Item a]
chronologicalWith getDate =
    sortByM $ (\x -> return $ getDate $ itemIdentifier x)
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs


--------------------------------------------------------------------------------
-- | The reverse of 'chronological'
recentFirstWith :: MonadMetadata m
                => (Identifier -> UTCTime)  -- ^ Function that returns the date value for an identifier
                -> [Item a]                 -- ^ The items to sort
                -> m [Item a]
recentFirstWith getDate items = fmap reverse $ chronologicalWith getDate items
