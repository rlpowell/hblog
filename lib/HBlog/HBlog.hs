{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module HBlog.HBlog
    ( hblogMain
    ) where

import           HBlog.Lib
import           Debug.Trace                            (trace, traceM, traceId)
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
import           Data.List.Utils                        (replace, split)
import qualified Control.Applicative             as CA  (Alternative (..))
import qualified Data.Text                       as T
import qualified Data.Char                       as Char


hblogMain :: IO ()
hblogMain = hakyll $ do
    -- ************
    -- Build up various chunks of data we'll need later
    -- ************
    allTags <- buildTagsWith myGetTags "posts/**" myCategBasedTagsToIdentifier

    allCategories <- buildTagsWith myGetCategory "posts/**" (fromCapture "categories/*.html")

    ids <- getMatches "posts/**"

    titles <- fmap mconcat $ mapM getTitlePair ids

    -- Before you make changes to any of the data handling here, read
    -- and update the "Dates" section in DESIGN-CODE.  This stuff is
    -- complicated.
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

    match "posts/**.md" $ do
        route $ (gsubRoute "posts/" (const "")) `composeRoutes` setExtension "html"
        compile $ do
            myId <- getUnderlying
            categs <- myGetCategory myId

            -- Example trace:
            -- loads1 <- loadAll ("posts/computing/**" .&&. hasVersion "recents")
            -- traceM ("loads1: " ++ show (loads1 :: [Item String]))
            -- loads2 <- loadAll ("posts/**" .&&. hasVersion "recents")
            -- traceM ("loads2: " ++ show (loads2 :: [Item String]))

            -- Normally we find recents from the current category,
            -- but for the meta pages we use computing *and* career
            let pattern = if categs == [] then
                    (fromGlob "posts/computing/**.md") .||. (fromGlob "posts/career/**.md")
                else
                    (fromGlob $ "posts/" ++ (head categs) ++ "/**.md")

            recents <- (selectNotSelf myId) =<< (myRecentFirst gitTimes) =<< myGetIdentifiers pattern

            let postsContext = postCtx allTags allCategories gitTimes `mappend`
                               -- Distinguish things like archive.html from regular posts
                               constField "article" "yes"            `mappend`
                               -- Show recent posts
                               listField "recents" (postCtx allTags allCategories gitTimes) (return $ take 3 recents)

            pandocCompilerWithTransform hblogPandocReaderOptions hblogPandocFinalWriterOptions (titleFixer titles)
                >>= loadAndApplyTemplate "templates/post.html"    postsContext
                >>= loadAndApplyTemplate "templates/default.html" postsContext
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


    forM_ (tagsMap allCategories) $ \(categ, _) ->
        create [fromFilePath $ categ ++ "/archive.html" ] $ do
            route idRoute
            compile $ do
                -- Get only the posts in this category
                posts <- (myRecentFirst gitTimes) =<< loadAll (fromGlob $ "posts/" ++ categ ++ "/**")
                recents <- (myRecentFirst gitTimes) =<< myGetIdentifiers (fromGlob $ "posts/" ++ categ ++ "/**")
                let archiveCtx =
                        listField "recents" (postCtx allTags allCategories gitTimes) (return $ take 3 recents) `mappend`
                        listField "posts" (postCtx allTags allCategories gitTimes) (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
                        (postCtx allTags allCategories gitTimes)

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls

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

    -- Post tags; generates pages like
    --          _site/computing/tags/psychology_plus_sysadminning.html
    -- listing all the relevant articles.
    tagsRules allTags $ \tag pattern -> do
        let title = "Posts Tagged " ++ (head $ drop 1 $ split ":" tag)
        let categ = head $ split ":" tag

        -- FIXME: Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- (myRecentFirst gitTimes) =<< loadAll pattern
            recents <- (myRecentFirst gitTimes) =<< myGetIdentifiers (fromGlob $ "posts/" ++ categ ++ "/**")
            let ctx = constField "title" title `mappend`
                        listField "recents" (postCtx allTags allCategories gitTimes) (return $ take 3 recents) `mappend`
                        listField "posts" (postCtx allTags allCategories gitTimes) (return posts) `mappend`
                        (postCtx allTags allCategories gitTimes)
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

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

-- Used for the Recent Changes sidebar; makes sure none of them is
-- the items listed is current article itself, cuz that looks kinda
-- dumb.
selectNotSelf :: MonadMetadata m => Identifier -> [Item a] -> m [Item a]
selectNotSelf myId recents = return $ filter (\x -> ((itemIdentifier x) { identifierVersion = Nothing } ) /= myId) recents

-- | Find the category of the current item.
--
-- What we actually do is get the first directory name after posts/
-- , and call that the category, which we return as a single array
-- entry because this has to work with the rest of hakyll's tag
-- system.
--
-- For all files where the above doesn't make sense (not under
-- posts/, in the meta category, etc), we return an empty array.
--
-- No category (i.e. returning the empty array) is treated as
-- equivalent to being in the "meta" category by various other bits
-- of code.  We could maybe actually return such things as being in
-- the "meta" category by returning that as such from this function,
-- but that makes other things harder (I'm afraid I no longer
-- remember which other things, but feel free to try and see what
-- happens; searching for "meta" in this code should easily find you
-- all the relevant bits, change it and run the tests).
--
-- Returns an array of strings because that is what buildTagsWith
-- expects, not because this actually makes sense here.
--
-- Examples of file paths that this function has to handle:
--
-- posts/computing/general/ched.md
-- posts/computing/index.md
-- posts/index.md
-- posts/meta/index.md
-- posts/personal/contact.md
-- archive.html
--
-- Tags files are a special case; because we create them directly
-- the source file name is not in posts/, but we want it to be in
-- the correct category.  Examples:
--
-- computing/tags/linux.html
-- computing/tags/linux_plus_psychology.html
myGetCategory :: MonadMetadata m => Identifier -> m [String]
myGetCategory x =
    -- let filePath = trace ("fp1: " ++ (head $ splitDirectories $ toFilePath x) ++ ", " ++ (show $ (head $ splitDirectories $ toFilePath x) == "posts")) $ toFilePath x
    -- let filePath = trace ("myGetCategory: " ++ (toFilePath x) ++ ", " ++ (show $ splitDirectories $ toFilePath x)) $ toFilePath x
    let filePath = toFilePath x
        filePathParts = dropWhile (\foo -> foo == "/") $ splitDirectories filePath
    in
        if length filePathParts > 2 then
            if (head filePathParts) == "posts" then
                if (take 1 $ dropPosts filePath) /= ["meta"] then
                    -- This is the normal posts/computing/general/ched.md case
                    return $ take 1 $ dropPosts filePath
                else
                    -- This is posts/meta/* ; no category == meta
                    return []
            else
                -- This is the computing/tags/psychology_plus_sysadminning.html case
                if (filePathParts !! 1) == "tags" then
                    return $ take 1 $ splitDirectories filePath
                else
                    -- This shouldn't ever happen
                    return $ [fail "In myGetCategory, something with more than 2 path elements is neither a post nor a tags file; " ++ (show filePath)]
        else
            -- posts/index.md is the *only* file in posts/, so very
            -- much a special case, and in the blank/meta category.
            if filePathParts == ["posts","index.md"] then
                return []
            else
                -- This is the computing/archive.html case
                if (filePathParts !! 1) == "archive.html" then
                    return $ take 1 $ splitDirectories filePath
                else
                    -- This shouldn't ever happen
                    return $ [fail "In myGetCategory, something with 2 or fewer path elements is not an archive file; " ++ (show filePath)]

-- Given a pattern, searches the already-loaded metadata with
-- getMatches for items matching that pattern and returns them (with
-- no body)
--
-- Mostly stolen from https://github.com/dannysu/hakyll-blog/blob/321532e82d6e847f45c93f58f83b6b354be6da1a/src/HakyllHelper.hs
--
-- See http://rlpowell.name/computing/general/hakyll_recent_posts.html
-- for more details on how this works
myGetIdentifiers :: Pattern -> Compiler [Item String]
myGetIdentifiers pattern = do
    identifiers <- getMatches pattern
    return [Item identifier "" | identifier <- identifiers]

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
renderShortTagLink :: String -> (Maybe FilePath) -> Maybe H.Html
renderShortTagLink _   Nothing         = Nothing
renderShortTagLink tag (Just filePath) =
  if elem '+' tag then
    Nothing
  else
    Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml (head $ drop 1 $ split ":" tag)

-- | Take a tag like foo:bar+baz and return a file identifier like
-- foo/tags/bar_plus_baz.hml
--
-- This used to be:
--
--      (fromCaptures "tags/*.html")
--
-- in case you want to use that to help figure out how it works.
myCategBasedTagsToIdentifier :: String -> Identifier
-- myCategBasedTagsToIdentifier string | trace ("myCategBasedTagsToIdentifier: " ++ (show string) ++ "\n") False = undefined
myCategBasedTagsToIdentifier string = fromFilePath $ (replace "+" "_plus_" (replace ":" "/tags/" string)) ++ ".html"

-- | See the Tags & Categories section of DESIGN-CODE.
myGetTags :: MonadMetadata m => Identifier -> m [String]
myGetTags identifier = do
    tags <- getTags identifier
    let maintags = map (intercalate "+") $ drop 1 $ subsequences $ sort tags
    cat <- myGetCategory identifier
    return $ map (\x -> (head cat) ++ ":" ++ x) maintags

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

-- | Works on Pandoc Link elements to fix minor problems with the
-- Pandoc generated from our markdown.
--
-- In particular, makes URLs for posts absolute, and makes URLs for
-- anchors have the # they need.
--
titleFixerInternal :: [(String, FilePath)] -> [(String, String)] -> Inline -> Inline
-- titleFixerInternal titles headers link@(Link _ _ (url, _)) | trace ("url: " ++ (show url) ++ ", titles: " ++ (show titles)) False = undefined
-- titleFixerInternal titles headers link@(Link _ _ (url, _)) | trace ("url: " ++ (show url)) False = undefined
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
      -- Turn posts/computing/general/ched.md into /computing/general/ched.html
      let newURL = "/" </> (joinPath $ dropPosts $ replaceExtension (fromJust maybePath) ".html") in
        Link x fixedText (newURL, z)
        -- trace ("added slash: " ++ fixedURL ++ ", " ++ (fromJust maybePath) ++ ", " ++ newURL) $ Link x fixedText (newURL, z)
    else
      if isJust maybeHeader then
        -- Turns "about" (as a URL, which is not gonna work) into "#about" (an anchor URL)
        let newURL = "#" ++ (fromJust maybeHeader) in
          -- trace ("added hash: " ++ fixedURL ++ ", " ++ (fromJust maybeHeader) ++ ", " ++ newURL) $ Link x fixedText (newURL, z)
          Link x fixedText (newURL, z)
      else
        link
titleFixerInternal _ _ x = x

--------------------------- Date Handling ------------------------------
-- Before you make changes to any of the data handling here, read
-- and update the "Dates" section in DESIGN-CODE.  This stuff is
-- complicated.

data GitTimes = GitTimes { gtid :: Identifier, gtlatest :: UTCTime, gtinitial :: UTCTime } deriving (Ord, Eq, Show)

-- Pull the most and least recent git times for a file out of git
-- using the git command line tool
getGitTimes :: Identifier -> IO [GitTimes]
getGitTimes identifier = do
    let path = toFilePath identifier
    let gitRepoPath = "posts/"
    -- Strip off the "posts/" part
    let gitFilePath = joinPath $ dropPosts path

    -- Before you make changes to any of the data handling here, read
    -- and update the "Dates" section in DESIGN-CODE.  This stuff is
    -- complicated.
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

    -- Before you make changes to any of the data handling here, read
    -- and update the "Dates" section in DESIGN-CODE.  This stuff is
    -- complicated.
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
  -- The identifier for the things compiled with the "recents"
  -- version has the identifierVersion "recents", but we don't care
  -- about that since the only reason that exists is to avoid loops,
  -- so we strip it here for our lookup.
  let fixedIdent = ident { identifierVersion = Nothing }
      timeList = filter (\x -> fixedIdent == (gtid x)) times in
    if length timeList /= 1 then
      -- It's not obvious to me how this could occur even in theory; I'd expect it to error out during getGitTimes
      error $ "getGitTimeUTC: Couldn't find the time for " ++ (show fixedIdent) ++ " in GitTimes list " ++ (show times)
    else
      typeF $ head timeList

-- Returns a function that sorts a list of items using getGitTimeUTC
myRecentFirst :: MonadMetadata m => [GitTimes] -> [Item a] -> m [Item a]
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

-- This is a wrapper for makeNumberedTagLink; it takes an extra
-- argument, which is the current category.  Based on that, it
-- either outputs everything (if we're in the meta category), or it
-- only outputs tags that match current category, and it strips the
-- category from the front.
categBasedTagLink :: String -> Double -> Double -> String -> String -> Int -> Int -> Int -> String
categBasedTagLink categ minSize maxSize tag url count min' max' =
    if categ == "" || categ == "meta" then
        makeNumberedTagLink minSize maxSize tag url count min' max'
    else
        let maybeTag = stripPrefix (categ ++ ":") tag in
          if isNothing maybeTag then
              ""
          else
              makeNumberedTagLink minSize maxSize (fromJust maybeTag) url count min' max' 

-- Most of this is from makeLink in renderTagCloud in hakyll-4.9.0.0/src/Hakyll/Web/Tags.hs
makeNumberedTagLink :: Double -> Double -> String -> String -> Int -> Int -> Int -> String
makeNumberedTagLink minSize maxSize tag url count min' max' =
        -- Show the relative size of one 'count' in percent
        let diff     = 1 + fromIntegral max' - fromIntegral min'
            relative = (fromIntegral count - fromIntegral min') / diff
            size     = floor $ minSize + relative * (maxSize - minSize) :: Int
            html     = renderHtml $
                        H.a ! A.style (toValue $ "font-size: " ++ show size ++ "%")
                            ! A.href (toValue url)
                            $ toHtml $ tag ++ " (" ++ (show count) ++ ") "
        in html ++ "\n"

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

-- Generates a tag cloud using only the things in the category of
-- the current item (or everything if we're in meta).
--
-- For argument info, see tagCloudFieldWith in hakyll/lib/Hakyll/Web/Tags.hs
--
-- The "makeLink" argumet has an extra string argument; that's the
-- category, which ends up being the first argument of
-- categBasedTagLink.
myTagCloudField :: String
                  -> (String -> Double -> Double -> String -> String -> Int -> Int -> Int -> String)
                  -> ([String] -> String)
                  -> Double
                  -> Double
                  -> Tags
                  -> Context a
myTagCloudField key makeLink cat minSize maxSize tags = Context $ \k _ i ->
  if k == key then do
    categBits <- myGetCategory $ itemIdentifier i
    let categ = mconcat categBits
    str <- renderTagCloudWith (makeLink categ) cat minSize maxSize tags
    return $ StringField str
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
--
-- I'm not clear how it works internally, but conceptually, each
-- entry in this list is given the current Item (this is the "i"
-- argument that various field functions uses internally) to extract
-- information from.

-- Construct our $ replacement stuff
postCtx :: Tags -> Tags -> [GitTimes] -> Context String
postCtx allTags allCategories gtimes = mconcat
    [ myTagCloudField "tagCloud" categBasedTagLink (intercalate " ") 80 200 allTags

    -- Give a way to link absolutely back to the main site
    , constField "homeURL" baseURL

    -- We always use the last_mod_date from git; if you want to
    -- override that, make a new git commit and use --date
    --
    -- Before you make changes to any of the data handling here, read
    -- and update the "Dates" section in DESIGN-CODE.  This stuff is
    -- complicated.
    , field  "last_mod_date" (gitTimeToField gtimes gtlatest)
    -- In fact, we explicitely fail if you try to use it from header
    -- metadata
    , failIfMetadatas ["last_mod_date", "date", "published"]
    -- For orig_date, though, if you specify it in the metadata we
    -- take that, whether it parses or not, otherwise we use git
    --
    -- Before you make changes to any of the data handling here, read
    -- and update the "Dates" section in DESIGN-CODE.  This stuff is
    -- complicated.
    , dateFieldWithFallback defaultTimeLocale ((flip getMetadataField) "orig_date") (getGitTimeUTCCompiler gtimes gtinitial) "orig_date" "%B %e, %Y"
    , constField "gitTimes" $ show gtimes
    -- This one is weird: the allTags we pass are *not* the tags it
    -- renders; it gets the tags from the file with getTags, the
    -- tags we pass are how it knows what the (textual) tags on the
    -- item point at.
    , tagsFieldWith myGetTags renderShortTagLink (mconcat . intersperse ",\n ") "tags" allTags
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
