{-# LANGUAGE RelaxedPolyRec, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Much of this was stolen wholesale from Text.Pandoc.Readers.TWiki

module Text.Pandoc.Readers.TikiWiki ( readTikiWiki
                                    , readTikiWikiWithWarnings
                                    ) where

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (enclosed, macro, nested)
import Text.Pandoc.Readers.HTML (htmlTag, isCommentTag)
import Control.Monad
import Text.Printf (printf)
import Debug.Trace (trace)
import Text.Pandoc.XML (fromEntities)
import Data.Maybe (fromMaybe)
import Text.HTML.TagSoup
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import qualified Data.Foldable as F
import Text.Pandoc.Error

-- Much of this was stolen wholesale from Text.Pandoc.Readers.TWiki

-- | Read TikiWiki from an input string and return a Pandoc document.
readTikiWiki :: ReaderOptions -- ^ Reader options
          -> String        -- ^ String to parse (assuming @'\n'@ line endings)
          -> Either PandocError Pandoc
readTikiWiki opts s =
  (readWith parseTikiWiki) def{ stateOptions = opts } (s ++ "\n\n")

readTikiWikiWithWarnings :: ReaderOptions -- ^ Reader options
                      -> String        -- ^ String to parse (assuming @'\n'@ line endings)
                      -> Either PandocError (Pandoc, [String])
readTikiWikiWithWarnings opts s =
  (readWith parseTikiWikiWithWarnings) def{ stateOptions = opts } (s ++ "\n\n")
 where parseTikiWikiWithWarnings = do
         doc <- parseTikiWiki
         warnings <- stateWarnings <$> getState
         return (doc, warnings)

type TikiWikiParser = Parser [Char] ParserState

--
-- utility functions
--

tryMsg :: String -> TikiWikiParser a -> TikiWikiParser a
tryMsg msg p = try p <?> msg

skip :: TikiWikiParser a -> TikiWikiParser ()
skip parser = parser >> return ()

nested :: TikiWikiParser a -> TikiWikiParser a
nested p = do
  nestlevel <- stateMaxNestingLevel <$>  getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

--
-- main parser
--

parseTikiWiki :: TikiWikiParser Pandoc
parseTikiWiki = do
  bs <- mconcat <$> many block
  spaces
  eof
  return $ B.doc bs

block :: TikiWikiParser B.Blocks
block = do
  tr <- getOption readerTrace
  pos <- getPosition
  res <- mempty <$ skipMany1 blankline
         <|> blockElements
         <|> para
  skipMany blankline
  when tr $
    trace (printf "line %d: %s" (sourceLine pos)
           (take 60 $ show $ B.toList res)) (return ())
  return res

blockElements :: TikiWikiParser B.Blocks
blockElements = choice [ -- separator
                       table
                       , hr
                       , header
                       -- , verbatim
                       -- , literal
                       , mixedList
                       , definitionList
                       , codeMacro
                       -- , blockQuote
                       -- , noautolink
                       ]

hr :: TikiWikiParser B.Blocks
hr = try $ do
  string "----"
  many (char '-')
  newline
  return $ B.horizontalRule

header :: TikiWikiParser B.Blocks
header = tryMsg "header" $ do
  level <- many1 (char '!') >>= return . length
  guard $ level <= 6
  skipSpaces
  content <- B.trimInlines . mconcat <$> manyTill inline newline
  attr <- registerHeader nullAttr content
  return $ B.headerWith attr level $ content

tableRow :: TikiWikiParser [B.Blocks]
tableRow = try $ do
  row <- sepBy1 (many1 $ noneOf "\n|") (try $ string "|" <* notFollowedBy (oneOf "|\n")) 
  return $ map (B.plain . B.text) row

table :: TikiWikiParser B.Blocks
table = try $ do
  string "||"
  rows <- sepBy1 tableRow (try $ string "\n" <|> (string "||" <* notFollowedBy (string "\n")))
  string "||"
  newline
  return $ B.simpleTable (headers rows) $ trace ("rows: " ++ (show rows)) rows
  where
    -- The headers are as many empty srings as the number of columns
    -- in the first row
    headers rows = map (B.plain . B.str) $ take (length $ rows !! 0) $ repeat ""

para :: TikiWikiParser B.Blocks
para = many1Till inline endOfParaElement >>= return . result . mconcat
 where
   endOfParaElement = lookAhead $ endOfInput <|> endOfPara <|> newBlockElement
   endOfInput       = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara        = try $ blankline >> skipMany1 blankline
   newBlockElement  = try $ blankline >> skip blockElements
   result content   = if F.all (==Space) content
                      then mempty
                      else B.para $ B.trimInlines content

definitionList :: TikiWikiParser B.Blocks
definitionList = tryMsg "definitionList" $ do
  elements <- many1 $ parseDefinitionListItem
  return $ B.definitionList elements
  where
    parseDefinitionListItem :: TikiWikiParser (B.Inlines, [B.Blocks])
    parseDefinitionListItem = do
      skipSpaces >> char ';' <* skipSpaces
      term <- many1Till inline $ char ':' <* skipSpaces
      line <- listItemLine 1
      return $ (mconcat term, [B.plain line])

data ListType = None | Numbered | Bullet deriving (Ord, Eq, Show)

data ListNesting = LN { lntype :: ListType, lnnest :: Int } deriving (Ord, Eq, Show)

tripleFst :: (a, b, c) -> a
tripleFst (x, _, _) = x

-- The first argument is a stack (most recent == head) of our list
-- nesting status; the list type and the nesting level; if we're in
-- a number list in a bullet list it'd be
-- [LN Numbered 2, LN Bullet 1]
mixedList :: TikiWikiParser B.Blocks
mixedList = try $ do
  items <- try $ many1 listItem
  return $ mconcat $ fixListNesting $ spanFoldUpList (LN None 0) items

-- See the "Handling Lists" section of DESIGN-CODE for why this
-- function exists.  It's to post-process the lists and do some
-- mappends.
--
-- We need to walk the tree two items at a time, so we can see what
-- we're going to join *to* before we get there.
--
-- Because of that, it seemed easier to do it by hand than to try to
-- figre out a fold or something.
fixListNesting :: [B.Blocks] -> [B.Blocks]
fixListNesting [] = []
fixListNesting (first:[]) = [recurseOnList first]
-- fixListNesting all | trace ("\n\nfixListNesting: " ++ (show all)) False = undefined
fixListNesting all@(first:second:rest) = 
  let secondBlock = head $ B.toList second in
    case secondBlock of
      BulletList _ -> fixListNesting $ [(mappend (recurseOnList first) (recurseOnList second))] ++ rest
      OrderedList _ _ -> fixListNesting $ [(mappend (recurseOnList first) (recurseOnList second))] ++ rest
      _ -> [recurseOnList first] ++ fixListNesting (second:rest)

-- This function walks the Block structure for fixListNesting,
-- because it's a bit complicated, what with converting to and from
-- lists and so on.
recurseOnList :: B.Blocks -> B.Blocks
-- recurseOnList item | trace ("rOL: " ++ (show $ length $ B.toList item) ++ ", " ++ (show $ B.toList item)) False = undefined
recurseOnList items
  | (length $ B.toList items) == 1 =
    let itemBlock = head $ B.toList items in
      case itemBlock of
        BulletList listItems -> B.bulletList $ fixListNesting $ map B.fromList listItems
        OrderedList _ listItems -> B.orderedList $ fixListNesting $ map B.fromList listItems
        _ -> items

  -- The otherwise works because we constructed the blocks, and we
  -- know for a fact that no mappends have been run on them; each
  -- Blocks consists of exactly one Block.
  --
  -- Anything that's not like that has already been processed by
  -- fixListNesting; don't bother to process it again.
  | otherwise = items


-- Turn the list if list items into a tree by breaking off the first
-- item, splitting the remainder of the list into items that are in
-- the tree of the first item and those that aren't, wrapping the
-- tree of the first item in its list time, and recursing on both
-- sections.
spanFoldUpList :: ListNesting -> [(ListNesting, B.Blocks)] -> [B.Blocks]
spanFoldUpList _ [] = []
spanFoldUpList ln (first:[]) =
  listWrap ln (fst first) [snd first]
spanFoldUpList ln (first:rest) =
  let (span1, span2) = span (splitListNesting (fst first)) rest
      newTree1 = listWrap ln (fst first) $ [snd first] ++ spanFoldUpList (fst first) span1
      newTree2 = spanFoldUpList ln span2
  in
    newTree1 ++ newTree2

-- Decide if the second item should be in the tree of the first
-- item, which is true if the second item is at a deeper nesting
-- level and of the same type.
splitListNesting :: ListNesting -> (ListNesting, B.Blocks) -> Bool
splitListNesting ln1 (ln2, _) =
  if (lnnest ln1) < (lnnest ln2) then
    True
  else
    if ln1 == ln2 then
      True
    else
      False

-- If we've moved to a deeper nesting level, wrap the new level in
-- the appropriate type of list.
listWrap :: ListNesting -> ListNesting -> [B.Blocks] -> [B.Blocks]
listWrap upperLN curLN retTree =
  if upperLN == curLN then
    retTree
  else
    case lntype curLN of
      Bullet -> [B.bulletList retTree]
      Numbered -> [B.orderedList retTree]

listItem :: TikiWikiParser (ListNesting, B.Blocks)
listItem = choice [
    bulletItem
  , numberedItem
  ]

bulletItem :: TikiWikiParser (ListNesting, B.Blocks)
bulletItem = try $ do
  prefix <- many1 $ char '*'
  many1 $ char ' '
  content <- listItemLine (length prefix)
  return $ (LN Bullet (length prefix), B.plain content)

numberedItem :: TikiWikiParser (ListNesting, B.Blocks)
numberedItem = try $ do
  prefix <- many1 $ char '#'
  many1 $ char ' '
  content <- listItemLine (length prefix)
  return $ (LN Numbered (length prefix), B.plain content)

listItemLine :: Int -> TikiWikiParser B.Inlines
listItemLine nest = lineContent >>= parseContent >>= return
  where
    lineContent = do
      content <- anyLine
      continuation <- optionMaybe listContinuation
      return $ filterSpaces content ++ "\n" ++ (maybe "" id continuation)
    filterSpaces = reverse . dropWhile (== ' ') . reverse
    listContinuation = string (take nest (repeat '+')) >> lineContent
    parseContent x = do
      parsed <- parseFromString (many1 inline) x
      return $ mconcat parsed

-- Turn the CODE macro attributes into Pandoc code block attributes.
mungeAttrs :: [(String, String)] -> (String, [String], [(String, String)])
mungeAttrs rawAttrs = ("", classes, rawAttrs)
  where
    -- "colors" is TikiWiki CODE macro for "name of language to do
    -- highlighting for"; turn the value into a class
    color = fromMaybe "" $ lookup "colors" rawAttrs
    -- ln = 1 means line numbering.  It's also the default.  So we
    -- emit numberLines as a class unless ln = 0
    lnRaw = fromMaybe "1" $ lookup "ln" rawAttrs
    ln = if lnRaw == "0" then
            ""
         else
            "numberLines"
    classes = filter (/= "") [color, ln]

codeMacro :: TikiWikiParser B.Blocks
codeMacro = try $ do
  string "{CODE("
  rawAttrs <- macroAttrs
  string ")}"
  body <- manyTill anyChar (try (string "{CODE}"))
  newline
  if length rawAttrs > 0 then
    return $ B.codeBlockWith (mungeAttrs rawAttrs) body
  else
    return $ B.codeBlock body


--
-- inline parsers
--

inline :: TikiWikiParser B.Inlines
inline = choice [ whitespace
                , noparse
                , strong
                , emph
                , nbsp
                , image
                , htmlComment
                , strikeout
                , code
                , wikiLink
                , notExternalLink
                , externalLink
                , superTag
                , superMacro
                , subTag
                , subMacro
                , escapedChar
                , colored
                , centered
                , underlined
                , boxed
                , breakChars
                , str
                , symbol
                ] <?> "inline"

whitespace :: TikiWikiParser B.Inlines
whitespace = (lb <|> regsp) >>= return
  where lb = try $ skipMany spaceChar >> linebreak >> return B.space
        regsp = try $ skipMany1 spaceChar >> return B.space

nbsp :: TikiWikiParser B.Inlines
nbsp = try $ do
  string "~hs~" 
  return $ B.str " NOT SUPPORTED BEGIN: ~hs~ (non-breaking space) :END "

htmlComment :: TikiWikiParser B.Inlines
htmlComment = try $ do
  string "~hc~" 
  inner <- many1 $ noneOf "~"
  string "~/hc~"
  return $ B.str $ " NOT SUPPORTED: ~hc~ (html comment opener) BEGIN: " ++ inner ++ " ~/hc~ :END "

linebreak :: TikiWikiParser B.Inlines
linebreak = newline >> notFollowedBy newline >> (lastNewline <|> innerNewline)
  where lastNewline  = eof >> return mempty
        innerNewline = return B.space

between :: (Monoid c) => TikiWikiParser a -> TikiWikiParser b -> (TikiWikiParser b -> TikiWikiParser c) -> TikiWikiParser c
between start end p =
  mconcat <$> try (start >> notFollowedBy whitespace >> many1Till (p end) end)

enclosed :: (Monoid b) => TikiWikiParser a -> (TikiWikiParser a -> TikiWikiParser b) -> TikiWikiParser b
enclosed sep p = between sep (try $ sep <* endMarker) p
  where
    endMarker   = lookAhead $ skip endSpace <|> skip (oneOf ".,!?:)|") <|> eof
    endSpace    = (spaceChar <|> newline) >> return B.space


nestedInlines :: Show a => TikiWikiParser a -> TikiWikiParser B.Inlines
nestedInlines end = innerSpace <|> nestedInline
  where
    innerSpace   = try $ whitespace <* (notFollowedBy end)
    nestedInline = notFollowedBy whitespace >> nested inline

image :: TikiWikiParser B.Inlines
image = try $ do
  string "{img "
  rawAttrs <- sepEndBy1 imageAttr spaces
  string "}"
  let src = fromMaybe "" $ lookup "src" rawAttrs
  let title = fromMaybe src $ lookup "desc" rawAttrs
  let alt = fromMaybe title $ lookup "alt" rawAttrs
  let classes = map fst $ filter (\(a,b) -> b == "" || b == "y") rawAttrs
  if length src > 0 then
    return $ B.imageWith ("", classes, rawAttrs) src title (B.str alt)
  else
    return $ B.str $ " NOT SUPPORTED: image without src attribute BEGIN: {img " ++ (printAttrs rawAttrs) ++ "} :END "
  where
    printAttrs attrs = intercalate " " $ map (\(a, b) -> a ++ "=\"" ++ b ++ "\"") attrs

imageAttr :: TikiWikiParser (String, String)
imageAttr = try $ do
  key <- many1 (noneOf "=} \t\n")
  char '='
  optional $ char '"'
  value <- many1 (noneOf "}\"\n")
  optional $ char '"'
  optional $ char ','
  return (key, value)


strong :: TikiWikiParser B.Inlines
strong = try $ enclosed (string "__") nestedInlines >>= return . B.strong

escapedChar :: TikiWikiParser B.Inlines
escapedChar = try $ do
  string "~"
  inner <- many1 $ oneOf "0123456789"
  string "~"
  return $ B.str $ [(toEnum ((read inner) :: Int)) :: Char]

centered :: TikiWikiParser B.Inlines
centered = try $ do
  string "::"
  inner <- many1 $ noneOf ":\n"
  string "::"
  return $ B.str $ " NOT SUPPORTED: :: (centered) BEGIN: ::" ++ inner ++ ":: :END "

colored :: TikiWikiParser B.Inlines
colored = try $ do
  string "~~"
  inner <- many1 $ noneOf "~\n"
  string "~~"
  return $ B.str $ " NOT SUPPORTED: ~~ (colored) BEGIN: ~~" ++ inner ++ "~~ :END "

underlined :: TikiWikiParser B.Inlines
underlined = try $ do
  string "==="
  inner <- many1 $ noneOf "=\n"
  string "==="
  return $ B.str $ " NOT SUPPORTED: ==== (underlined) BEGIN: ===" ++ inner ++ "=== :END "

boxed :: TikiWikiParser B.Inlines
boxed = try $ do
  string "^"
  inner <- many1 $ noneOf "^\n"
  string "^"
  return $ B.str $ " NOT SUPPORTED: ^ (boxed) BEGIN: ^" ++ inner ++ "^ :END "

emph :: TikiWikiParser B.Inlines
emph = try $ enclosed (string "''") nestedInlines >>= return . B.emph

strikeout :: TikiWikiParser B.Inlines
strikeout = try $ enclosed (string "--") nestedInlines >>= return . B.strikeout

nestedString :: Show a => TikiWikiParser a -> TikiWikiParser String
nestedString end = innerSpace <|> (count 1 nonspaceChar)
  where
    innerSpace = try $ many1 spaceChar <* notFollowedBy end

breakChars :: TikiWikiParser B.Inlines
breakChars = try $ string "%%%" >> return B.linebreak

superTag :: TikiWikiParser B.Inlines
superTag = try $ between (string "{TAG(tag=>sup)}") (string "{TAG}") nestedString >>= return . B.superscript . B.text . fromEntities

superMacro :: TikiWikiParser B.Inlines
superMacro = try $ do
  string "{SUP("
  manyTill anyChar (string ")}")
  body <- manyTill anyChar (string "{SUP}")
  return $ B.superscript $ B.text body

subTag :: TikiWikiParser B.Inlines
subTag = try $ between (string "{TAG(tag=>sub)}") (string "{TAG}") nestedString >>= return . B.subscript . B.text . fromEntities

subMacro :: TikiWikiParser B.Inlines
subMacro = try $ do
  string "{SUB("
  manyTill anyChar (string ")}")
  body <- manyTill anyChar (string "{SUB}")
  return $ B.subscript $ B.text body

code :: TikiWikiParser B.Inlines
code = try $ between (string "-+") (string "+-") nestedString >>= return . B.code . fromEntities

macroAttr :: TikiWikiParser (String, String)
macroAttr = try $ do
  key <- many1 (noneOf "=)")
  char '='
  optional $ char '"'
  value <- many1 (noneOf " )\"")
  optional $ char '"'
  return (key, value)

macroAttrs :: TikiWikiParser [(String, String)]
macroAttrs = try $ do
  attrs <- sepEndBy macroAttr spaces
  return attrs

noparse :: TikiWikiParser B.Inlines
noparse = try $ do
  string "~np~"
  body <- manyTill anyChar (string "~/np~")
  return $ B.str body

str :: TikiWikiParser B.Inlines
str = (many1 alphaNum <|> count 1 characterReference) >>= return . B.str

symbol :: TikiWikiParser B.Inlines
symbol = count 1 nonspaceChar >>= return . B.str

notExternalLink :: TikiWikiParser B.Inlines
notExternalLink = try $ do
  start <- string "[["
  body <- many (noneOf "\n[]")
  end <- string "]"
  return $ B.text (start ++ body ++ end)

-- The ((...)) wiki links and [...] external links are handled
-- exactly the same; this abstracts that out
makeLink :: String -> String -> String -> TikiWikiParser B.Inlines
makeLink start middle end = try $ do
  st <- getState
  guard $ stateAllowLinks st
  setState $ st{ stateAllowLinks = False }
  (url, title, anchor) <- wikiLinkText start middle end
  setState $ st{ stateAllowLinks = True }
  return $ B.link (url++anchor) "" $ B.text title

wikiLinkText :: String -> String -> String -> TikiWikiParser (String, String, String)
wikiLinkText start middle end = do
  string start
  url <- many1 (noneOf $ middle ++ "\n")
  seg1 <- option url linkContent
  seg2 <- option "" linkContent
  string end
  if seg2 /= "" then
    return (url, seg2, seg1)
  else
    return (url, seg1, "")
  where
    linkContent      = do
      (char '|')
      str <- many (noneOf middle)
      return $ str

externalLink :: TikiWikiParser B.Inlines
externalLink = makeLink "[" "]|" "]"

-- NB: this wiki linking is unlikely to work for anyone besides me
-- (rlpowell); it happens to work for me because my Hakyll code has
-- post-processing that treats pandoc .md titles as valid link
-- targets, so something like
-- [see also this other post](My Other Page) is perfectly valid.
wikiLink :: TikiWikiParser B.Inlines
wikiLink = makeLink "((" ")|" "))"
