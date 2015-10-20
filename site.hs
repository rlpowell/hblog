--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.List
import qualified Text.Regex.PCRE as PCRE
import           Data.Maybe (listToMaybe)
import           Control.Monad
import           Hakyll
import           Hakyll.Web.Tags
import           Hakyll.Core.Util.String
import           System.FilePath
import           System.Posix.Files
import           Hakyll.Web.Template.Context
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import qualified Data.Map                        as M

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTagsWith myGetTags "posts/**" (fromCapture "tags/*.html")

    categories <- buildTagsWith myGetCategory "posts/**" (fromCapture "categories/*.html")

    ids <- getMatches "posts/**"

    titles <- mapM getTitle ids

    match "posts/**" $ do
        route $ setExtension "html"
        compile $ do
            wikiCompiler titles
            -- pandocCompiler
            >>= renderPandoc
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags categories)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags categories)
            >>= relativizeUrls

--     create ["categories.html"] $ do
--         route idRoute
--         compile $ do
--             posts <- recentFirst =<< loadAll "categories/*"
--             let archiveCtx =
--                     listField "posts" (postCtx tags categories) (return posts) `mappend`
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
            posts <- recentFirst =<< loadAll "posts/**"
            let archiveCtx =
                    listField "posts" (postCtx tags categories) (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let indexCtx = mconcat [
                    listField "posts" (postCtx tags categories) (return posts)
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
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title `mappend`
                        listField "posts" (postCtx tags categories) (return posts) `mappend`
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
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title `mappend`
                        listField "posts" (postCtx tags categories) (return posts) `mappend`
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

        -- -- Create RSS feed as well
        -- version "rss" $ do
        --     route   $ setExtension "xml"
        --     compile $ loadAllSnapshots pattern "content"
        --         >>= fmap (take 10) . recentFirst
        --         >>= renderRss (feedConfiguration title) feedCtx


--------------------------------------------------------------------------------
postCtx :: Tags -> Tags -> Context String
postCtx tags categories = mconcat [
    dateField "date" "%B %e, %Y"
    , myCategoryField "category" categories
    , tagsField "tags" tags
    , defaultContext
    ]        

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
-- FIXME: exmaple
myGetTags :: MonadMetadata m => Identifier -> m [String]
myGetTags identifier = do
    metadata <- getMetadata identifier
    let maintags = map (intercalate "+") $ drop 1 $ subsequences $ sort $ maybe [] (map trim . splitAll ",") $ M.lookup "tags" metadata
    cat <- myGetCategory identifier
    return $ maintags ++ (map (\x -> (head cat) ++ ":" ++ x) maintags)

-- wikiCompiler :: [String] -> Item String -> Compiler (Item String)
-- wikiCompiler titles (Item i x) = do
--     return (Item i ((toFilePath i) ++ (intercalate " -- " titles)))

wikiCompiler :: [String] -> Compiler (Item String)
wikiCompiler titles = do
    -- FIXME: describe
    filePath <- toFilePath <$> getUnderlying
    -- FIXME: describe
    makeItem =<< unsafeCompiler (unWiki titles filePath)

unWiki :: [String] -> String -> IO String
unWiki titles filePath = do
    body <- readFile filePath
    case filePath of
      "posts/personal/my_views_on_the_future.md" ->
          let newBody = replaceAllPCRE "\\s(qq[a-z]+)\\s(.*?)\\sqq\\s" (unWikiReplacer titles) body in do
            _ <- writeFile (filePath ++ ".hblog.tmp") newBody
            rename (filePath ++ ".hblog.tmp") filePath
            return newBody
          -- return $ (intercalate " -- " titles) ++ body
      _ -> return body

-- Pulls the title metadata out for an item
getTitle :: MonadMetadata m => Identifier -> m String
getTitle identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] id $ M.lookup "title" metadata

unWikiReplacer :: [String] -> (PCRE.MatchResult String) -> String
unWikiReplacer titles mr = case length subList > 0 of
        True -> " --" ++ subList!!1 ++ "-- "
        False -> PCRE.mrMatch mr
    where
        subList = PCRE.mrSubList mr

-- A very simple,
--
-- https://github.com/erantapaa/haskell-regexp-examples/blob/master/RegexExamples.hs
-- was very helpful to me in constructing this
--
-- I also used
-- https://github.com/jaspervdj/hakyll/blob/ea7d97498275a23fbda06e168904ee261f29594e/src/Hakyll/Core/Util/String.hs
replaceAllPCRE :: String              -- ^ Pattern
           -> ((PCRE.MatchResult String) -> String)  -- ^ Replacement (called on capture)
           -> String              -- ^ Source string
           -> String              -- ^ Result
replaceAllPCRE pattern f source =
    if (source PCRE.=~ pattern) == True then
      replaceAllPCRE pattern f newStr
    else
      source
    where
        mr = (source PCRE.=~ pattern)
        newStr = (PCRE.mrBefore mr) ++ (f mr) ++ (PCRE.mrAfter mr)

-- todo: post ^^
