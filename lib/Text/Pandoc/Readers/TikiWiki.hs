{-# LANGUAGE RelaxedPolyRec, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
                       header
                       -- , verbatim
                       -- , literal
                       , mixedList
                       -- , table
                       -- , blockQuote
                       -- , noautolink
                       ]

header :: TikiWikiParser B.Blocks
header = tryMsg "header" $ do
  level <- many1 (char '!') >>= return . length
  guard $ level <= 6
  skipSpaces
  content <- B.trimInlines . mconcat <$> manyTill inline newline
  attr <- registerHeader nullAttr content
  return $ B.headerWith attr level $ content

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

-- list :: String -> TikiWikiParser B.Blocks
-- list prefix = choice [ bulletList prefix
--                      , orderedList prefix
--                      , definitionList prefix]
-- 
-- definitionList :: String -> TikiWikiParser B.Blocks
-- definitionList prefix = tryMsg "definitionList" $ do
--   indent <- lookAhead $ string prefix *> (many1 $ string "   ") <* string "$ "
--   elements <- many $ parseDefinitionListItem (prefix ++ concat indent)
--   return $ B.definitionList elements
--   where
--     parseDefinitionListItem :: String -> TikiWikiParser (B.Inlines, [B.Blocks])
--     parseDefinitionListItem indent = do
--       string (indent ++ "$ ") >> skipSpaces
--       term <- many1Till inline $ string ": "
--       line <- listItemLine indent $ string "$ "
--       return $ (mconcat term, [line])
-- 
-- bulletList :: String -> TikiWikiParser B.Blocks
-- bulletList prefix = tryMsg "bulletList" $
--                     parseList prefix (char '*') (char ' ')
-- 
-- orderedList :: String -> TikiWikiParser B.Blocks
-- orderedList prefix = tryMsg "orderedList" $
--                      parseList prefix (char '#') (char ' ')

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
  -- FIXME: ?? check the list length; error out if more than one
  let stuff = mconcat $ fixListNesting $ spanFoldUpList (LN None 0) items
  return $ trace (show stuff) stuff

recurseOnList :: B.Blocks -> B.Blocks
recurseOnList item | trace ("rOL: " ++ (show $ length $ B.toList item) ++ ", " ++ (show $ B.toList item)) False = undefined
recurseOnList items
  | (length $ B.toList items) == 1 =
    let itemBlock = head $ B.toList items in
      case itemBlock of
        BulletList listItems -> B.bulletList $ fixListNesting $ map B.fromList listItems
        OrderedList _ listItems -> B.orderedList $ fixListNesting $ map B.fromList listItems
        _ -> items
  -- This works because we constructed the blocks, and we know for a
  -- fact that no mappends have been run on them; each Blocks
  -- consists of exactly one Block.
  --
  -- Anything that's not like that has already been processed by
  -- fixListNesting; don't bother to process it again.
  | otherwise = items

fixListNesting :: [B.Blocks] -> [B.Blocks]
fixListNesting [] = []
fixListNesting (first:[]) = [recurseOnList first]
fixListNesting all | trace ("\n\nfixListNesting: " ++ (show all)) False = undefined
fixListNesting all@(first:second:rest) = 
  let secondBlock = head $ B.toList second in
    case secondBlock of
      BulletList _ -> fixListNesting $ [(mappend (recurseOnList first) (recurseOnList second))] ++ rest
      OrderedList _ _ -> fixListNesting $ [(mappend (recurseOnList first) (recurseOnList second))] ++ rest
      _ -> [recurseOnList first] ++ fixListNesting (second:rest)

-- Many {unMany = fromList [OrderedList (1,DefaultStyle,DefaultDelim) [
--   [
--     Plain [Str "nl1.1",Space],BulletList [[Plain [Str "bl2.1",Space]],[Plain [Str "bl2.2",Space]]]
-- ],[Plain [Str "nl1.2",Space]]]]} 

-- Give an LN and an (LN, Blocks) pair, return true IFF the LN in
-- the pair has a higher nesting value.
splitListNesting :: ListNesting -> (ListNesting, B.Blocks) -> Bool
splitListNesting ln1 (ln2, _) =
  if (lnnest ln1) < (lnnest ln2) then
    True
  else
    if ln1 == ln2 then
      True
    else
      False

lnPlus1 :: ListNesting -> ListNesting
lnPlus1 ln = LN (lntype ln) ((lnnest ln) + 1)

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

listWrap upperLN curLN retTree =
  if upperLN == curLN then
    retTree
  else
    case lntype curLN of
      Bullet -> [B.bulletList retTree]
      Numbered -> [B.orderedList retTree]

-- listWrap checkLN checkTree retTree =
--   if length retTree < 1 then
--     []
--   else
--     let firstLN = fst $ head checkTree in
--       if lntype firstLN == lntype firstLN && lnnest firstLN == lnnest firstLN then
--         retTree
--       else
--         case lntype firstLN of
--           Bullet -> [B.bulletList retTree]
--           Numbered -> [B.orderedList retTree]
-- 
-- foldUpList :: ListNesting -> [(ListNesting, B.Blocks)] -> [B.Blocks]
-- foldUpList nest [] = []
-- foldUpList nest all | trace ("\n\nfoldUpList: " ++ (show nest) ++ "\n\nall: " ++ (show all)) False = undefined
-- foldUpList nest all@((ni1, item1):rest) =
--   case compare (lnnest ni1) (lnnest nest) of
--     GT -> let (newTree1, newTree2) = spanFoldUpList nest { lntype = (lntype ni1) } (LN (lntype ni1) ((lnnest nest) + 1)) nest { lntype = (lntype ni1) } all in
--           newTree1 ++ newTree2
--     EQ -> let (newTree1, newTree2) = spanFoldUpList nest nest { lntype = (lntype ni1) } nest { lntype = (lntype ni1) } rest
--               retTree1 = [item1] ++ newTree1
--               retTree2 = newTree2
--           in
--             if length rest < 2 then
--               retTree1 ++ retTree2
--             else
--               let ((ni2, item2):rest2) = rest in
--                 case compare (lnnest ni1) (lnnest ni2) of
--                   EQ -> retTree1 ++ retTree2
--                   GT -> retTree1 ++ retTree2
--                   LT -> [mconcat retTree1] ++ retTree2
-- 
--     LT -> []

-- foldUpList :: ListNesting -> [(ListNesting, B.Blocks)] -> ([B.Blocks], [ListNesting], [(ListNesting, B.Blocks)])
-- -- foldUpList current ((nextLN, nextBlocks):rest) | trace ("foldUpList: " ++ (show current) ++ " -1- " ++ (show nextLN) ++ " -2- " ++ (show nextBlocks) ++ " -3- " ++ (show rest)) False = undefined
-- foldUpList current all@((nextLN, nextBlocks):rest) =
--   case ((lntype current) == (lntype nextLN), compare (lnnest current) (lnnest nextLN)) of
--     -- Level increased, means a new sub-list.  Or new list type,
--     -- means a new sub-list.
--     --
--     -- Collect all the things in that sublist and get the remainder,
--     -- then also recurse on that remainder.  Concat the new sub-list
--     -- with the results of running on the remainder.
--     --
--     -- FIXME: Can we combine two case guards?
--     (_, LT) -> newSubList current all
--     (False, _) -> newSubList current all
--     -- Same type, same level; concat this item to the results of
--     -- recursing on the rest.
--     (True, EQ) -> let (newBlocksList, newLNList, newRest) = foldUpList nextLN rest in
--                     -- sub-lists need to be folded into the list
--                     -- item above them
--                     let returning =
--                           case newBlocksList of
--                                [] -> ([nextBlocks], newLNList, newRest)
--                                -- FIXME: explain this
--                                _ -> if not (endsWithList [nextBlocks]) && startsWithList newBlocksList then
--                                          trace ("list joining: ") ([mappend nextBlocks (head newBlocksList)] ++ tail newBlocksList, newLNList, newRest)
--                                     else
--                                          trace ("no join: " ++ show (head $ B.toList $ head newBlocksList)) ([nextBlocks] ++ newBlocksList, newLNList, newRest)
--                     in
--                     trace ("\n\n***** retmain: current: " ++ (show current) ++ "\n\nall: " ++ (show all) ++ "\n\nnextBlocks: " ++ (show nextBlocks) ++ "\n\n  newBlocks " ++ (show newBlocksList) ++ "\n\nnewLNList: " ++ (show newLNList) ++ "\n\nnewRest: " ++ (show newRest) ++ "\n\n returning:  " ++ (show returning)) returning
--     -- Level decreased; close out the current list by ending.
--     (_, GT) -> ([], [], all)
-- foldUpList _ [] = ([], [], [])
-- 
-- startsWithList :: [B.Blocks] -> Bool
-- startsWithList blocks =
--   case head $ B.toList $ head blocks of
--     (BulletList x) -> True
--     (OrderedList q r) -> True
--     _ -> False
-- 
-- endsWithList :: [B.Blocks] -> Bool
-- endsWithList blocks | trace ("lastlast: " ++ show blocks) False = undefined
-- endsWithList blocks =
--   case last $ B.toList $ last blocks of
--     (BulletList x) -> True
--     (OrderedList q r) -> True
--     _ -> False
-- 
-- newSubList current all@((nextLN, nextBlocks):rest) =
--   let (newBlocks, newLNList, newRest) = foldUpList nextLN all
--       (newBlocks2, newLNList2, newRest2) = foldUpList current newRest
--   in
--     case (lntype nextLN) of
--          Numbered -> let returning = ([(B.orderedList $ newBlocks)] ++ newBlocks2, newLNList2, newRest2) in
--                       trace ("retnsl1: " ++ (show returning)) returning
--          Bullet -> let returning = ([(B.bulletList $ newBlocks)] ++ newBlocks2, newLNList2, newRest2) in
--                       trace ("\n\nretnsl2: " ++ (show all) ++ "\n\n newrest2:" ++ (show newRest2) ++ "\n\nnewBlocks: " ++ (show newBlocks) ++ "\n\nnewRest: " ++ (show newRest) ++ "\n\nnewBlocks2: " ++ (show newBlocks2) ++ "\n\n returning:  " ++ (show returning)) returning

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

-- parseList :: String -> TikiWikiParser Char -> TikiWikiParser a -> TikiWikiParser B.Blocks
-- parseList prefix marker delim = do
--   (indent, style) <- lookAhead $ string prefix *> listStyle <* delim
--   blocks <- many $ parseListItem (prefix ++ indent) (char style <* delim)
--   return $ case style of
--     '#' -> B.orderedListWith (1, DefaultStyle, DefaultDelim) blocks
--     _   -> B.bulletList blocks
--   where
--     listStyle = do
--       indent <- many $ string "   "
--       style <- marker
--       return (concat indent, style)
-- 
-- parseListItem :: Show a => String -> TikiWikiParser a -> TikiWikiParser B.Blocks
-- parseListItem prefix marker = string prefix >> marker >> listItemLine prefix marker

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


--
-- inline parsers
--

inline :: TikiWikiParser B.Inlines
inline = choice [ whitespace
                , br
                , noparse
                , strong
                , emph
                , strikeout
                , code
                , codeMacro
                , wikiLink
                , externalLink
                , superTag
                , superMacro
                , subTag
                , subMacro
                , breakChars
                , str
                , symbol
                ] <?> "inline"

whitespace :: TikiWikiParser B.Inlines
whitespace = (lb <|> regsp) >>= return
  where lb = try $ skipMany spaceChar >> linebreak >> return B.space
        regsp = try $ skipMany1 spaceChar >> return B.space

br :: TikiWikiParser B.Inlines
br = try $ string "%BR%" >> return B.linebreak

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

strong :: TikiWikiParser B.Inlines
strong = try $ enclosed (string "__") nestedInlines >>= return . B.strong

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

codeMacro :: TikiWikiParser B.Inlines
codeMacro = try $ do
  string "{CODE("
  manyTill anyChar (string ")}")
  body <- manyTill anyChar (string "{CODE}")
  return $ B.code body

noparse :: TikiWikiParser B.Inlines
noparse = try $ do
  string "~np~"
  body <- manyTill anyChar (string "~/np~")
  return $ B.str body

str :: TikiWikiParser B.Inlines
str = (many1 alphaNum <|> count 1 characterReference) >>= return . B.str

symbol :: TikiWikiParser B.Inlines
symbol = count 1 nonspaceChar >>= return . B.str

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
  url <- many1 (noneOf middle)
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
-- (rlpowell); it happens to work for me because we have
-- post-processing that treats pandoc .md titles as valid link
-- targets, so [see also this other post](My Other Page) is
-- perfectly valid.
wikiLink :: TikiWikiParser B.Inlines
wikiLink = makeLink "((" ")|" "))"
