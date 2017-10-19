{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module HBlog.HBlog
    ( hblogMain
    ) where

import           HBlog.Lib
import           Data.List
import           Hakyll                          hiding (Redirect(..))
import           System.FilePath
import           Text.Blaze.Html                        (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String        (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Pandoc                            (writePlain, def, nullMeta, PandocError(..), runPure)
import           Text.Pandoc.Definition                 (Pandoc(..), Inline(..), Block(..))
import           Text.Pandoc.Walk                       (walk, query)
import           Network.URI                            (unEscapeString)
import           Hakyll.Core.Identifier                 (toFilePath)
import           Data.Time.Clock                        (UTCTime)
import           System.Process                         (readProcessWithExitCode)
import           System.Exit                            (ExitCode(..))
import           Data.Time.Format                       (TimeLocale, formatTime, parseTimeM, defaultTimeLocale)
import           Control.Monad                          (liftM, msum, forM_)
import           Data.Ord                               (comparing)
import           Text.Read                              (readMaybe)
import           Data.Maybe                             (isJust, isNothing, fromJust)
import qualified Control.Applicative             as CA  (Alternative (..))
import qualified Data.Text                       as T
import qualified Data.Char                       as Char


hblogMain :: IO ()
hblogMain = hakyll $ do
    -- ************
    -- Build up various chunks of data we'll need later
    -- ************
    allTags <- buildTagsWith myGetTags "posts/**" (fromCapture "tags/*.html")

    allCategories <- buildTagsWith myGetCategory "posts/**" (fromCapture "categories/*.html")

    ids <- getMatches "posts/**"

    titles <- fmap mconcat $ mapM getTitlePair ids

    gitTimes <- fmap mconcat $ preprocess $ mapM getGitTimes ids

    -- ************
    -- Build the actual destination files
    -- ************
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/**" $ do
        route $ (gsubRoute "posts/" (const "")) `composeRoutes` setExtension "html"
        compile $ do
            pandocCompilerWithTransform hblogPandocReaderOptions hblogPandocWriterOptions (titleFixer titles)
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx allTags allCategories gitTimes)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx allTags allCategories gitTimes)
            >>= relativizeUrls

-- FIXME: Do we want a list of categories anywhere?  Probably; we'd
-- talked about having a "default" site that takes extra work to get
-- to, that would use something like this
--
--     create ["categories.html"] $ do
--         route idRoute
--         compile $ do
--             posts <- (myRecentFirst gitTimes) =<< loadAll "categories/*"
--             let archiveCtx =
--                     listField "posts" (postCtx allTags allCategories gitTimes) (return posts) `mappend`
--                     constField "title" "Archives"            `mappend`
--                     (postCtx allTags allCategories gitTimes)
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
                    listField "posts" (postCtx allTags allCategories gitTimes) (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    (postCtx allTags allCategories gitTimes)

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


--    match "index.html" $ do
--        route idRoute
--        compile $ do
--            posts <- (myRecentFirst gitTimes) =<< loadAll "posts/**"
--            let indexCtx = mconcat [
--                    listField "posts" (postCtx allTags allCategories gitTimes) (return posts)
--                    , constField "title" "Home"
--                    , (postCtx allTags allCategories gitTimes)
--                    ]
--
--            getResourceBody
--                >>= applyAsTemplate indexCtx
--                >>= loadAndApplyTemplate "templates/default.html" indexCtx
--                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    redirects <- fmap mconcat $ mapM getRedirects ids

    -- Build redirect rule files; partially copied from tagsRules in
    -- Hakyll/Web/Tags.hs
    forM_ (tagsMap allCategories) $ \(categ, _) ->
      create [fromFilePath $ "redirects/" ++ categ] $ do
        route idRoute
        compile $ do
            -- The second argument of listField is the thing we can
            -- render inside the for loop; i.e. if we define a field
            -- named "redirect" as the second argument, then
            -- $redirect$ will work in the for loop in the template.
            let redirectCtx = listField "redirects" insideRedirectCtx $
                                        sequence $ map makeItem $ filter (\redirect -> (redirCategory redirect) == categ) redirects

            makeItem ""
                >>= applyAsTemplate redirectCtx
                >>= loadAndApplyTemplate "templates/redirects" redirectCtx
                >>= relativizeUrls

    -- Post tags; generates pages like _site/tags/futurism+psychology.html listing all the relevant articles.
    tagsRules allTags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- FIXME: Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- (myRecentFirst gitTimes) =<< loadAll pattern
            let ctx = constField "title" title `mappend`
                        listField "posts" (postCtx allTags allCategories gitTimes) (return posts) `mappend`
                        (postCtx allTags allCategories gitTimes)
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

-- Types used for our redirect generation
type RedirectCategory = String
type RedirectPattern = String
data Redirect = Redirect
    { redirIdent     :: Identifier
    , redirCategory  :: RedirectCategory
    , redirPath      :: FilePath
    , redirPattern   :: RedirectPattern
    }

baseURL :: String
baseURL = "http://rlpowell.digitalkingdom.org/"

-- Pulls the title metadata out for an item
getTitlePair :: MonadMetadata m => Identifier -> m [(String, FilePath)]
getTitlePair identifier = do
    metadata <- getMetadataField identifier "title"
    if isJust metadata then
      return $ [(fromJust metadata, toFilePath $ identifier)]
    else
      return []

dropPosts :: FilePath -> [FilePath]
dropPosts fp = drop 1 $ dropWhile (/= "posts") $ splitDirectories fp

-- Pulls the redirect metadata out for an item; pair is
-- (Category, Redirect)
getRedirects :: MonadMetadata m => Identifier -> m [Redirect]
getRedirects identifier = do
    categ <- myGetCategory identifier
    let path = joinPath $ dropPosts $ toFilePath identifier
    redirect <- getMetadataField identifier "redirect"
    if isJust redirect then
      return $ [Redirect identifier (head categ) path (fromJust redirect)]
    else
      return []

-- | Causes the field matching the category name (or "meta") to be
-- set to "true"; this means you can do things like:
--
-- $if(meta)$
-- <p>META!</p>
-- $endif$
--
-- $if(computing)$
-- <p>COMPUTING!</p>
-- $endif$
--
-- in a template.
myCategoryCheckFields :: Context a
myCategoryCheckFields = Context $ \k _ i -> do
  -- k is the key that user is checking (i.e. "computing")
  categBits <- myGetCategory $ itemIdentifier i
  let categ = mconcat categBits
  if categ == k then
    return $ StringField "true"
  else
    if categ == "" && k == "meta" then
      return $ StringField "true"
    else
      -- Compiler is an instance of Alternative from
      -- Control.Applicative ; see Hakyll/Core/Compiler/Internal.hs
      CA.empty

-- | Find the category of the current item.
--
-- What we actually do is get the first directory name after posts/
-- , and call that the category; if there's no posts/ we return
-- nothing.
--
-- Returns an array of strings because that is what buildTagsWith
-- expects, not because this actually makes sense here.
--
-- Examples of file paths that this function has to handle:
--
-- posts/hobbies/contact.md
-- posts/index.md
-- posts/meta/index.md
-- posts/personal/misc/comparables.md
-- tags/philosophy.html
-- categories/computing.html
-- archive.html
myGetCategory :: MonadMetadata m => Identifier -> m [String]
myGetCategory x =
    -- let filePath = trace ("fp1: " ++ (head $ splitDirectories $ toFilePath x) ++ ", " ++ (show $ (head $ splitDirectories $ toFilePath x) == "posts")) $ toFilePath x
    let filePath = toFilePath x
        filePathParts = dropWhile (\foo -> foo == "/") $ splitDirectories filePath
    in
        if length filePathParts > 2 &&
           (head filePathParts) == "posts" &&
           (take 1 $ dropPosts $ filePath) /= ["meta"] then
             return $ take 1 $ dropPosts $ filePath
        else
             return []

titleCase :: String -> String
titleCase (hed:tale) = Char.toUpper hed : map Char.toLower tale
titleCase [] = []

-- | Return a field containing the category of the current item, as
-- simple text, or "meta" if the current item doesn't have one.
--
-- Arguments:
--
-- 1.  The name of the field to generate.
-- 2.  Whether or not to TitleCase the result.
myCategoryField :: String -> Bool -> Context a
myCategoryField key toTitleCase = field key $ \item -> do
    tagBits <- myGetCategory $ itemIdentifier item
    let tag = mconcat tagBits
    if tag == "" then
      if toTitleCase then
        return $ "Meta"
      else
        return $ "meta"
    else
      if toTitleCase then
        return $ titleCase tag
      else
        return $ tag

-- Helper for myCategoriesField
myCategoriesFieldLink :: String -> H.Html
myCategoriesFieldLink category =
    H.li $ H.a ! A.href (toValue $ toUrl $ "/" ++ category) $ toHtml category

-- | Generate a list of links to the pages for each given category;
-- just simple links, nothing fancy.
myCategoriesField :: String -> Tags -> Context a
myCategoriesField key categories = field key $ \item -> do
    let categLinks = fmap myCategoriesFieldLink (sort $ filter (\x -> x /= "meta") $ map fst $ tagsMap categories) in
        return $ renderHtml $ H.ul $ toHtml categLinks

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
    return $ maintags ++ cat ++ (map (\x -> (head cat) ++ ":" ++ x) maintags)

getHeaders :: Block -> [(String, String)]
getHeaders (Header _ (slug, _, _) xs) =
    [(str, slug)]
    where
      str = case runPure $ writePlain def $ Pandoc nullMeta $ [Plain xs] of
                   Left (PandocSomeError err)  -> "hblog getHeaders: unknown error: " ++ err
                   Left _ -> "hblog getHeaders: unknown error!"
                   Right item'              -> T.unpack item'
getHeaders _ = []

titleFixer :: [(String, FilePath)] -> Pandoc -> Pandoc
titleFixer titles doc = 
  let headers = query getHeaders doc in
    walk (titleFixerInternal titles headers) doc

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
    let gitRepoPath = "posts/"
    -- Strip off the "posts/" part
    let gitFilePath = joinPath $ dropPosts path

    (exit1, stdout1, stderr1) <- readProcessWithExitCode "git" ["-C", gitRepoPath, "log", "--diff-filter=A", "--follow", "--format=%ai", "-n", "1", "--", gitFilePath] ""
    if exit1 /= ExitSuccess then
      do fail $ "getGitTimes: Couldn't get the date of the first commit for " ++ path ++ ", error: " ++ stderr1
    else
      return ()

    if stdout1 == "" then
      do fail $ "getGitTimes: The last commit date for " ++ path ++ " was blank, which means it needs to be checked in.\n\n\nA run of munge_files.sh should clear this right up.\n\n"
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
myRecentFirst :: MonadMetadata m =>[GitTimes] -> [Item a] -> m [Item a]
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

-- Most of this is from makeLink in renderTagCloud in hakyll-4.9.0.0/src/Hakyll/Web/Tags.hs
makeNumberedTagLink :: Double -> Double -> String -> String -> Int -> Int -> Int -> String
makeNumberedTagLink minSize maxSize tag url count min' max' =
        -- Show the relative size of one 'count' in percent
        let diff     = 1 + fromIntegral max' - fromIntegral min'
            relative = (fromIntegral count - fromIntegral min') / diff
            size     = floor $ minSize + relative * (maxSize - minSize) :: Int
        in renderHtml $
            H.a ! A.style (toValue $ "font-size: " ++ show size ++ "%")
                ! A.href (toValue url)
                $ toHtml $ tag ++ " (" ++ (show count) ++ ") "

-- What we're trying to do is produce a field that *doesn't* match
-- key in the case where the metadata "header" is not set to "no" or
-- "false"; matching it and returning false or whatever
-- (makeHeaderField above) isn't working, so any call to "field" is
-- guaranteed to not work
makeHeaderField :: String -> Context a
makeHeaderField key = Context $ \k _ i -> do
    if key == k then do
      value <- getMetadataField (itemIdentifier i) "header"
      if isJust value then
        if elem (fromJust value) [ "no", "No", "false", "False" ] then
          -- Compiler is an instance of Alternative from
          -- Control.Applicative ; see Hakyll/Core/Compiler/Internal.hs
          CA.empty
        else
          return $ StringField $ fromJust value
      else
        return $ StringField "yes makeheader"
    else
      CA.empty

--------------------------------------------------------------------------------
-- Context Configuration
--------------------------------------------------------------------------------
-- A "Context" in Hakyll is basically a list of fields that can be
-- replaced in a template with the $stuff$ syntax.  We have many
-- custom fields, and at least one (myCategoryCheckFields)
-- meta-field (that is, it might match any field name, depending on
-- the details).
--
-- Basically, this list is walked with the name of the field being
-- asked for, and the first item that answers to the name gets it.

-- Construct our $ replacement stuff
postCtx :: Tags -> Tags -> [GitTimes] -> Context String
postCtx allTags allCategories gtimes = mconcat
    [ tagCloudFieldWith "tagCloud" makeNumberedTagLink (intercalate " ") 80 200 allTags

    -- Give a way to link absolutely back to the main site
    , constField "homeURL" baseURL

    -- We always use the last_mod_date from git; if you want to
    -- override that, make a new git commit and use --date
    , field  "last_mod_date" (gitTimeToField gtimes gtlatest)
    -- In fact, we explicitely fail if you try to use it from header
    -- metadata
    , failIfMetadatas ["last_mod_date", "date", "published"]
    -- For orig_date, though, if you specify it in the metadata we
    -- take that, whether it parses or not, otherwise we use git
    , dateFieldWithFallback defaultTimeLocale ((flip getMetadataField) "orig_date") (getGitTimeUTCCompiler gtimes gtinitial) "orig_date" "%B %e, %Y"
    , constField "gitTimes" $ show gtimes
    -- This one is weird: the allTags we pass are *not* the tags it
    -- renders; it gets the tags from the file with getTags, the
    -- tags we pass are how it knows what the (textual) tags on the
    -- item point at.
    , tagsField "tags" allTags
    -- Anwers the field named for the current item's category (i.e.
    -- if the current item's category is "computing", $computing$
    -- will work in the template).
    , myCategoryCheckFields
    -- All categories, as links; used very rarely as we don't want
    -- people bouncing around categories most of the time.
    , myCategoriesField "categories" allCategories
    -- The name of the current category.
    , myCategoryField "categoryText" False
    , myCategoryField "categoryTextCap" True
    -- Below is the contents of defaultContext, except for
    -- titleField; titleField is a default in case metadata has no
    -- title, and we want to error out in that case.
    , bodyField     "body"
    , metadataField
    , urlField      "url"
    , pathField     "path"

    -- An artificial field that only exists if "header: False" is not
    -- present in the file header; see posts/index.md for an example.
    , makeHeaderField "makeheader"

    , missingField
    ]

-- Construct our $ replacement stuff for the special case of the
-- redirects file
insideRedirectCtx :: Context Redirect
insideRedirectCtx = mconcat
    [ field "path" $ return . redirPath . itemBody
    , field "category" $ return . redirCategory . itemBody
    , field "redirect" $ return . redirPattern . itemBody
    , constField "homeURL" baseURL
    , missingField
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
