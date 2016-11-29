--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
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

    titles <- trace "wibble" $ fmap mconcat $ mapM getTitlePair ids

    match "posts/**" $ do
    -- match ("posts/**" .&&. complement "**/index.html") $ do
        route $ setExtension "html"
        compile $ do
            pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions (titleFixer titles)
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
titleFixerInternal titles headers link@(Link x y (rawURL, z)) =
  let fixedURL = unEscapeString rawURL
      maybePath = lookup fixedURL titles
      maybeHeader = lookup fixedURL headers
  in
    if isJust maybePath then
      let newURL = "/" </> replaceExtension (fromJust maybePath) ".html" in
        Link x y (newURL, z)
    else
      if isJust maybeHeader then
        Link x y ("#" ++ (fromJust maybeHeader), z)
      else
        link
titleFixerInternal _ _ x = x
