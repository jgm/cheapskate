{-# LANGUAGE OverloadedStrings #-}

module Cheapskate (parseMarkdown, renderBlocks) where
import Prelude hiding (takeWhile)
import qualified Data.Map as M
import Control.Monad.State
import Data.Char (isAscii, isSpace, isPunctuation, isSymbol,
                    isDigit, isHexDigit, isAlphaNum, isLetter)
import Data.List (intersperse)
import Network.URI (parseURI, isAllowedInURI, escapeURIString)
import Data.Monoid
import Data.Foldable (foldMap, toList)
import Control.Applicative hiding (optional,empty)
import Data.Sequence (Seq, singleton, empty, (<|), ViewR(..), viewr)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import qualified Data.Text as T
import Data.Text ( Text )
import Data.Attoparsec.Text

-- for HTML rendering
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as BT
import Text.Blaze.Html hiding(contents)

-- for debugging
-- import Debug.Trace

-- Structured representation of a document.

-- Block-level elements.
data Block = Para Inlines
           | Header Int Inlines
           | Blockquote Blocks
           | List Bool ListType [Blocks]
           | CodeBlock CodeAttr Text
           | HtmlBlock Text
           | HRule
           deriving Show

-- Attributes for fenced code blocks.  More structure
-- can be added later, or perhaps a catch-all to contain
-- the remainder of the line after the language.
data CodeAttr = CodeAttr { codeLang :: Maybe Text }
              deriving Show

data ListType = Bullet Char | Numbered NumWrapper Int deriving Show
data NumWrapper = PeriodFollowing | ParenFollowing | ParensAround
                deriving (Eq,Show)

-- We operate with sequences instead of lists, because
-- they allow more efficient appending on to the end.
type Blocks = Seq Block

-- Inline elements.
data Inline = Str Text
            | Space
            | SoftBreak
            | LineBreak
            | Emph Inlines
            | Strong Inlines
            | Code Text
            | Link Inlines Text {- URL -} Text {- title -}
            | Image Inlines Text {- URL -} Text {- title -}
            | Entity Text
            | RawHtml Text
            | Markdown Text -- Raw markdown to be parsed later.
            deriving Show

type Inlines = Seq Inline

-- Returns width of a list marker based on the ListType.
-- Width does not include a following space.
listMarkerWidth :: ListType -> Int
listMarkerWidth (Bullet _) = 1
listMarkerWidth (Numbered wrap n) =
  (if wrap == ParensAround then 2 else 1) +
  case n of
       _ | n < 10    -> 1
         | n < 100   -> 2
         | n < 1000  -> 3
         | n < 10000 -> 4
         | otherwise -> 5  -- nobody counts this high, right?

-- Utility functions.

-- Like T.unlines but does not add a final newline.
-- Concatenates lines with newlines between.
joinLines :: [Text] -> Text
joinLines = T.intercalate "\n"

-- Convert tabs to spaces using a 4-space tab stop.
tabFilter :: Text -> Text
tabFilter = T.concat . pad . T.split (== '\t')
  where pad []  = []
        pad [t] = [t]
        pad (t:ts) = let tl = T.length t
                         n  = tl + 4 - (tl `mod` 4)
                         in  T.justifyLeft n ' ' t : pad ts

-- These are the whitespace characters that are significant in
-- parsing markdown. We can treat \160 (nonbreaking space) etc.
-- as regular characters.  This function should be considerably
-- faster than the unicode-aware isSpace from Data.Char.
isWhitespace :: Char -> Bool
isWhitespace ' '  = True
isWhitespace '\t' = True
isWhitespace '\n' = True
isWhitespace '\r' = True
isWhitespace _    = False

-- A line with all space characters is regarded as empty.
-- Note: we strip out tabs.
isEmptyLine :: Text -> Bool
isEmptyLine = T.all (==' ')

-- The original Markdown only allowed certain symbols
-- to be backslash-escaped.  It was hard to remember
-- which ones could be, so we now allow any ascii punctuation mark or
-- symbol to be escaped, whether or not it has a use in Markdown.
isEscapable :: Char -> Bool
isEscapable c = isAscii c && (isSymbol c || isPunctuation c)


-- Link references.

-- A map of link references.
type ReferenceMap = M.Map Text (Text, Text)

-- Link references are case sensitive and ignore line breaks
-- and repeated spaces.
-- So, [APPLES are good] == [Apples are good] ==
-- [Apples
-- are     good].
normalizeReference :: Text -> Text
normalizeReference = T.toUpper . T.concat . T.split isWhitespace

addLinkReference :: Text           -- reference label
                 -> (Text, Text)   -- (url, title)
                 -> BlockParser ()
addLinkReference key (url,tit) = modify $ \st ->
  st{ references = M.insert (normalizeReference key) (url,tit) (references st) }

lookupLinkReference :: ReferenceMap
                    -> Text                -- reference label
                    -> Maybe (Text, Text)  -- (url, title)
lookupLinkReference refmap key = M.lookup (normalizeReference key) refmap

-- Scanners.

-- Scanners are implemented here as attoparsec parsers,
-- which consume input and capture nothing.  They could easily
-- be implemented as regexes in other languages, or hand-coded.
-- With the exception of scanSpnl, they are all intended to
-- operate on a single line of input (so endOfInput = endOfLine).
type Scanner = Parser ()

-- Try a list of scanners, in order from first to last,
-- returning Just the remaining text if they all match,
-- Nothing if any of them fail.  Note that
-- applyScanners [a,b,c] == applyScanners [a >> b >> c].
applyScanners :: [Scanner] -> Text -> Maybe Text
applyScanners scanners t =
  case parseOnly (sequence_ scanners >> takeText) t of
       Right t'   -> Just t'
       Left _err  -> Nothing

-- Scan the beginning of a blockquote:  up to three
-- spaces indent, the `>` character, and an optional space.
scanBlockquoteStart :: Scanner
scanBlockquoteStart =
  scanNonindentSpaces >> scanChar '>' >> opt (scanChar ' ')

-- Scan four spaces.
scanIndentSpace :: Scanner
scanIndentSpace = () <$ count 4 (skip (==' '))

-- Scan 0-3 spaces.
scanNonindentSpaces :: Scanner
scanNonindentSpaces =
  (scanChar ' ' >>
    (scanChar ' ' >>
      (scanChar ' ' <|> return ())
    ) <|> return ()
  ) <|> return ()

-- Scan a specified character.
scanChar :: Char -> Scanner
scanChar c = char c >> return ()

-- Scan a blankline.
scanBlankline :: Scanner
scanBlankline = skipWhile (==' ') *> endOfInput

-- Scan a space.
scanSpace :: Scanner
scanSpace = skip (==' ')

-- Scan 0 or more spaces
scanSpaces :: Scanner
scanSpaces = skipWhile (==' ')

-- Scan 0 or more spaces, and optionally a newline
-- and more spaces.
scanSpnl :: Scanner
scanSpnl = scanSpaces *> opt (endOfLine *> scanSpaces)

-- Try a scanner; return success even if it doesn't match.
opt :: Scanner -> Scanner
opt s = option () (s >> return ())

-- Not followed by: Succeed without consuming input if the specified
-- scanner would not succeed.
nfb :: Parser a -> Scanner
nfb s = do
  succeeded <- option False (True <$ s)
  if succeeded
     then mzero
     else return ()

-- Succeed if not followed by a character. Consumes no input.
nfbChar :: Char -> Scanner
nfbChar c = nfb (skip (==c))

-- Parse the sequence of `#` characters that begins an ATX
-- header, and return the number of characters.  We require
-- a space after the initial string of `#`s, as not all markdown
-- implementations do. This is because (a) the ATX reference
-- implementation requires a space, and (b) since we're allowing
-- headers without preceding blank lines, requiring the space
-- avoids accidentally capturing a line like `#8 toggle bolt` as
-- a header.
parseAtxHeaderStart :: Parser Int
parseAtxHeaderStart = do
  hashes <- takeWhile1 (=='#')
  scanSpace <|> scanBlankline
  return $ T.length hashes

-- Scan an ATX header start, including the space.
scanAtxHeaderStart :: Scanner
scanAtxHeaderStart = () <$ parseAtxHeaderStart

-- Scan a horizontal rule line: "...three or more hyphens, asterisks,
-- or underscores on a line by themselves. If you wish, you may use
-- spaces between the hyphens or asterisks."
scanHRuleLine :: Scanner
scanHRuleLine = do
  scanNonindentSpaces
  c <- satisfy $ inClass "*_-"
  count 2 $ scanSpaces >> char c
  skipWhile (\x -> x == ' ' || x == c)
  endOfInput

-- Scan a code fence line.
scanCodeFenceLine :: Scanner
scanCodeFenceLine = () <$ codeFenceParserLine

-- Parse an initial code fence line, returning
-- the fence part and the rest (after any spaces).
codeFenceParserLine :: Parser (Text, Text)
codeFenceParserLine = do
  c <- satisfy $ inClass "`~"
  count 2 (char c)
  extra <- takeWhile (== c)
  scanSpaces
  rawattr <- takeWhile (/='`')
  endOfInput
  return (T.pack [c,c,c] <> extra, rawattr)

-- Scan the start of an HTML block:  either an HTML tag or an
-- HTML comment, with no indentation.
scanHtmlBlockStart :: Scanner
scanHtmlBlockStart = (   (pHtmlTag >>= guard . f . fst)
                     <|> (() <$ string "<!--")
                     <|> (() <$ string "-->" ))
  where f (Opening name) = name `Set.member` blockHtmlTags
        f (SelfClosing name) = name `Set.member` blockHtmlTags
        f (Closing name) = name `Set.member` blockHtmlTags

-- Scan the start of a list. If the parameter is Nothing, allow
-- any bullet or list number marker indented no more than 3 spaces.
-- If it is Just listType, then only succeed if the marker is
-- of the appropriate type.  (It must match bullet and number
-- wrapping style, but not the number itself.)
scanListStart :: Maybe ListType -> Parser ()
scanListStart Nothing = () <$ parseListMarker
scanListStart (Just (Bullet   c)) = do
  marker <- parseBullet
  case marker of
        Bullet c' | c == c' -> return ()
        _                   -> fail "Change in list style"
scanListStart (Just (Numbered w _)) = do
  marker <- parseListNumber
  case marker of
        Numbered w' _ | w == w' -> return ()
        _                       -> fail "Change in list style"

-- Parse a list marker and return the list type.
parseListMarker :: Parser ListType
parseListMarker = parseBullet <|> parseListNumber

-- Parse a bullet and return list type.
parseBullet :: Parser ListType
parseBullet = do
  c <- satisfy $ inClass "+*-"
  scanSpace <|> scanBlankline -- allow empty list item
  unless (c == '+')
    $ nfb $ (count 2 $ scanSpaces >> skip (== c)) >>
          skipWhile (\x -> x == ' ' || x == c) >> endOfInput -- hrule
  return $ Bullet c

-- Parse a list number marker and return list type.
parseListNumber :: Parser ListType
parseListNumber =
  (parseListNumberDig <|> parseListNumberPar) <*
     (scanSpace <|> scanBlankline)
  where parseListNumberDig = do
           num <- decimal  -- a string of decimal digits
           wrap <-  PeriodFollowing <$ skip (== '.')
                <|> ParenFollowing <$ skip (== ')')
           return $ Numbered wrap num
        parseListNumberPar = do
           skip (== '(')
           num <- decimal
           skip (== ')')
           return $ Numbered ParensAround num

-- Scan the beginning of a reference block: a bracketed label
-- followed by a colon.  We assume that the label is on one line.
scanReference :: Scanner
scanReference = scanNonindentSpaces >> pLinkLabel >> scanChar ':'

-- BlockParser:  parse the input line by line to discern block-level
-- structure.
--
-- The parser maintains a state that includes two stacks of scanners,
-- the "line scanners" and the "block scanners".
--
-- We start by taking the next input line and applying the block scanners.
-- We then scan the remaining text on the line to see what kind of
-- block we're dealing with, and parse it line by line as required.  The
-- line scanners are applied at the beginning of each line within a
-- block.  When the line scanners can no longer be applied, we have
-- finished the block and we try the block scanners again.  When the
-- block scanners can no longer be applied, we are done parsing.
--
-- When we begin a new block, we may push new block or line scanners
-- onto the stack.  For example, when we're parsing a blockquote, we
-- push a line scanner that consumes an optional (nonident space +
-- > character + optional space) at the beginning of each line within
-- a block.  And we push a block scanner that consumes a mandatory
-- (nonident space + > character + optional space) at the beginning
-- of each block (e.g. paragraph or list) within the blockquote.
-- These new scanners are popped off their respective stacks when
-- we've finished parsing the container block.
--
-- This parsing method allows us to proceed line by line without
-- backtracking.  At this level, we don't try to parse inlines within
-- lines; we just return the raw markdown of the lines parsed, and
-- hand this off to the inline parser after we've parsed all the
-- blocks and the link references.


parseBlocks :: Text -> (Blocks, ReferenceMap)
parseBlocks t = case inputLines s of
                     []       -> (bs, references s)
                     (next:_) -> error $ "Parsing stopped at the line: " ++ T.unpack next
  where (bs, s) = runState (parseLines False Nothing)
                    ParserState{ inputLines = map tabFilter $ T.lines t
                               , lastLine = Nothing
                               , references = M.empty
                               , lineScanners = []
                               , blockScanners = []
                               , textLines = []
                               }


data ParserState = ParserState{
         blockScanners  :: [Scanner]
       , lineScanners   :: [Scanner]
       , inputLines     :: [Text]
       , lastLine       :: Maybe Text
       , references     :: ReferenceMap
       , textLines      :: [Text]
       }

type BlockParser = State ParserState

-- Add a scanner to the line scanner stack and run a parser,
-- then pop the scanner.
withLineScanner :: Scanner -> BlockParser a -> BlockParser a
withLineScanner scanner parser = do
  scanners <- gets lineScanners
  modify $ \st -> st{ lineScanners = scanners ++ [scanner] }
  result <- parser
  modify $ \st -> st{ lineScanners = scanners }
  return result

-- Add a scanner to the block scanner stack and run a parser,
-- then pop the scanner.
withBlockScanner :: Scanner -> BlockParser a -> BlockParser a
withBlockScanner scanner parser = do
  scanners <- gets blockScanners
  modify $ \st -> st{ blockScanners = scanners ++ [scanner] }
  result <- parser
  modify $ \st -> st{ blockScanners = scanners }
  return result


peekTwoLines :: BlockParser (Maybe Text, Maybe Text)
peekTwoLines = do
  xs <- gets inputLines
  case xs of
       []      -> return (Nothing, Nothing)
       (x:[])  -> return (Just x, Nothing)
       (x:y:_) -> return (Just x, Just y)

advance :: BlockParser ()
advance = do
  lns <- gets inputLines
  case lns of
       (x:xs) -> modify $ \st -> st{
                     inputLines = xs
                   , lastLine = Just x }
       []     -> error "Cannot advance past end of text"

addTextLine :: Text -> BlockParser ()
addTextLine t = modify $ \st -> st{ textLines = t : textLines st }

popTextLines :: BlockParser Blocks
popTextLines = do
  t <- T.strip . joinLines . reverse <$> gets textLines
  modify $ \st -> st{ textLines = [] }
  if T.null t
     then return empty
     else return $ singleton $ Para $ singleton $ Markdown t

parseLines :: Bool -> Maybe Text -> BlockParser Blocks
parseLines continuation mbFirstLine = do
  (mbThis, mbNext) <- peekTwoLines
  bscanners <- gets blockScanners
  case mbThis of
    Nothing  -> popTextLines   -- no more lines of text!
                               -- return paragraph containing the
                               -- accumulated textLines
    Just thisLine ->
      -- apply the block scanners to the first line (thisLine)
      case mbFirstLine `mplus` applyScanners bscanners thisLine of
           Just thisLine' -> -- they match!  thisLine' is the remainder.
                             -- so this is the beginning of a new block.
                             -- call tryScanners to figure out what kind
                             -- and parse accordingly.
                             tryScanners scannerPairs thisLine'
           Nothing ->  -- they don't match.  either this is the end of
                       -- a block container (e.g. blockquote context)
                       -- or it is a lazy text line.
                       if continuation
                         then -- parse the lazy text line
                              parseTextLine thisLine
                         else -- return paragraph with accumulated text lines
                              popTextLines

      where tryScanners :: [(Scanner, Text -> Text -> BlockParser Blocks)]
                        -> Text -> BlockParser Blocks
            tryScanners _ ln | isEmptyLine ln = handleBlankLine
            tryScanners [] ln  -- fallback if none of the scanners match
               | mbFirstLine == Nothing && fmap isSetextLine
                  (mbNext >>= applyScanners bscanners) == Just True  = do
                     -- we have a setext header
                     tls <- popTextLines
                     let lev = if T.any (=='=') (maybe "" id mbNext) then 1 else 2
                     next <- setextHeaderParser lev ln ln
                     rest <- parseLines False Nothing
                     return $ tls <> next <> rest
               | otherwise = parseTextLine ln  -- regular text line
            tryScanners ((s,p):rest) ln =
               case applyScanners [s] ln of
                    Just ln' -> do
                      tls <- popTextLines  -- paragraph with accumulated text lines
                      next <- p ln ln'     -- this block
                      rest' <- parseLines False Nothing  -- rest of blocks
                      return $ tls <> next <> rest'
                    Nothing  -> tryScanners rest ln
            isSetextLine  x = not (T.null x) &&
                               (T.all (=='=') x || T.all (=='-') x)

scannerPairs :: [(Scanner, Text -> Text -> BlockParser Blocks)]
scannerPairs = [
    (scanBlockquoteStart, blockquoteParser)
  , (scanIndentSpace, indentedCodeBlockParser)
  , (scanAtxHeaderStart, atxHeaderParser)
  , (scanCodeFenceLine, codeFenceParser)
  , (scanReference, referenceParser)
  , (scanHRuleLine, hruleParser)
  , (scanNonindentSpaces >> scanListStart Nothing, listParser)
  , (scanHtmlBlockStart, htmlBlockParser) ]

handleBlankLine :: BlockParser Blocks
handleBlankLine = do
  advance
  (mbFirst,_) <- peekTwoLines
  bscanners <- gets blockScanners
  tls <- popTextLines
  if null bscanners -- parsing at outer level, just skip blanks
     then (tls <>) <$> parseLines False Nothing
     else -- in container, two blanks exits container
          case mbFirst >>= applyScanners bscanners of
             Just l | isEmptyLine l -> return tls
             _                      -> (tls <>) <$> parseLines False Nothing

parseTextLine :: Text -> BlockParser Blocks
parseTextLine thisLine = do
    line_scanners <- gets lineScanners
    case applyScanners line_scanners thisLine of
          Just x  -- the line scanners match, x is the remainder
            | isEmptyLine x -> popTextLines  -- stop and return paragraph
            | otherwise ->  -- check if line could be start of new block
              case parseOnly (msum $ map fst scannerPairs) x of
                   Right _ -> popTextLines -- start of new block - return paragraph
                   Left _  -> do
                     addTextLine x -- add line to textLines buffer
                     advance       -- move forward
                     parseLines True Nothing  -- continue parsing
          Nothing -> popTextLines  -- return paragraph

getLines :: (ParserState -> [Scanner]) -> BlockParser [Text]
getLines scannerType = do
  scanners <- gets scannerType
  (mbFirst, _) <- peekTwoLines
  case mbFirst >>= applyScanners scanners of
       Just l' -> advance >> (l':) <$> getLines scannerType
       Nothing -> return []

-- Specific block parsers.  These take two parameters. The first
-- is the whole line, before applying the scanner for this type
-- of parser; the second is what remains after that scanner is
-- applied.  Some parsers use the first, others the second,
-- by convenience.

-- Parse a blockquote.
blockquoteParser :: Text -> Text -> BlockParser Blocks
blockquoteParser _ _ = singleton . Blockquote <$>
  (withLineScanner (opt scanBlockquoteStart)
    $ withBlockScanner scanBlockquoteStart $ parseLines False Nothing)

-- Parse an indented code block.
indentedCodeBlockParser :: Text -> Text -> BlockParser Blocks
indentedCodeBlockParser _ ln = do
  advance
  lns <- withBlockScanner (scanIndentSpace <|> scanBlankline) $ getLines blockScanners
  return $ singleton . CodeBlock CodeAttr{ codeLang = Nothing } .  T.unlines
     . reverse . dropWhile T.null . reverse $ (ln:lns)

-- Parse a fenced code block.  Note:  if the fence is never terminated,
-- the entire rest of the document will be put into a fenced code block.
codeFenceParser :: Text -> Text -> BlockParser Blocks
codeFenceParser ln _ = do
  case parseOnly codeFenceParserLine ln of
       Left _  -> error "Could not parse codeFenceParserLine" -- should not happen
       Right (fence, rawattr) -> do
         advance -- consume fence
         lns <- withBlockScanner (nfb $ string fence) $ getLines blockScanners
         (mbFirst, _) <- peekTwoLines
         bscanners <- gets blockScanners
         case mbFirst >>= applyScanners bscanners of
              Just _  -> advance -- consume fence at end
              Nothing -> return () -- end of doc or container, treat whole as code block
         return $ singleton $ CodeBlock (parseCodeAttributes rawattr)
                $ T.unlines $ lns

-- Parse whatever remains on a fenced code block line after the fence.
-- The first word is the language, the rest is currently ignored,
-- but could at some point be returned or even parsed into a structure.
parseCodeAttributes :: Text -> CodeAttr
parseCodeAttributes t = CodeAttr { codeLang = lang }
  where lang = case T.words (T.strip t) of
                     []    -> Nothing
                     (l:_) -> Just l

-- Parse an ATX header.
atxHeaderParser :: Text -> Text -> BlockParser Blocks
atxHeaderParser ln _ = do
  advance  -- consume this line
  let ln' = T.strip $ T.dropAround (=='#') $ T.strip ln
  let inside = if "\\" `T.isSuffixOf` ln' && "#" `T.isSuffixOf` ln
                       then ln' <> "#"  -- escaped final #
                       else ln'
  case parseOnly parseAtxHeaderStart ln of
        Right lev
          | lev >= 1 && lev <= 6 -> return
                     $ singleton . Header lev . singleton . Markdown $ inside
        _  -> return $ singleton $ Para $ singleton $ Str ln

setextHeaderParser :: Int -> Text -> Text -> BlockParser Blocks
setextHeaderParser lev _ ln = do
  advance -- consume this line
  advance -- consume underline
  return $ singleton $ Header lev $ singleton $ Markdown ln

hruleParser :: Text -> Text -> BlockParser Blocks
hruleParser _ _ = (singleton HRule) <$ advance

-- Parse a link reference.  If parsing fails (rare), return a plain
-- paragraph with the first line; otherwise, return an empty sequence
-- and update the reference map in state.
referenceParser :: Text -> Text -> BlockParser Blocks
referenceParser first _ = do
  advance -- consume first line
  rest <- withLineScanner (nfb scanBlankline >> nfb scanReference)
          $ getLines lineScanners
  let raw = joinLines (first:rest)
  case parseOnly pReference raw of
       Left  _               -> return $ singleton $ Para
                                       $ singleton $ Markdown raw
       Right (lab, url, tit) -> empty <$ addLinkReference lab (url,tit)

-- A link reference is a square-bracketed link label, a colon,
-- optional space or newline, a URL, optional space or newline,
-- and an optional link title.
pReference :: Parser (Text, Text, Text)
pReference = do
  scanNonindentSpaces
  lab <- pLinkLabel
  char ':'
  scanSpnl
  url <- pLinkUrl
  tit <- option T.empty $ scanSpnl >> pLinkTitle
  scanSpaces
  endOfInput
  return (lab, url, tit)

-- A link label [like this].  Note the precedence:  code backticks have
-- precedence over label bracket markers, which have precedence over
-- *, _, and other inline formatting markers.
-- So, 2 below contains a link while 1 does not:
-- 1. [a link `with a ](/url)` character
-- 2. [a link *with emphasized ](/url) text*
pLinkLabel :: Parser Text
pLinkLabel = char '[' *> (T.concat <$>
  (manyTill (regChunk <|> pEscaped <|> bracketed <|> codeChunk) (char ']')))
  where regChunk = takeWhile1 (\c -> c /='`' && c /='[' && c /=']' && c /='\\')
        codeChunk = snd <$> pCode'
        bracketed = inBrackets <$> pLinkLabel
        inBrackets t = "[" <> t <> "]"

-- A URL in a link or reference.  This may optionally be contained
-- in `<..>`; otherwise whitespace and unbalanced right parentheses
-- aren't allowed.  Newlines aren't allowed in any case.
pLinkUrl :: Parser Text
pLinkUrl = do
  inPointy <- (char '<' >> return True) <|> return False
  if inPointy
     then T.pack <$> manyTill
           (pSatisfy (\c -> c /='\r' && c /='\n')) (char '>')
     else T.concat <$> many (regChunk <|> parenChunk)
    where regChunk = takeWhile1 (notInClass " \t\n\r()\\") <|> pEscaped
          parenChunk = parenthesize . T.concat <$> (char '(' *>
                         manyTill (regChunk <|> parenChunk) (char ')'))
          parenthesize x = "(" <> x <> ")"

-- A link title, single or double quoted or in parentheses.
-- Note that Markdown.pl doesn't allow the parenthesized form in
-- inline links -- only in references -- but this restriction seems
-- arbitrary, so we remove it here.
pLinkTitle :: Parser Text
pLinkTitle = do
  c <- satisfy (\c -> c == '"' || c == '\'' || c == '(')
  next <- peekChar
  case next of
       Nothing                 -> mzero
       Just x
         | isWhitespace x      -> mzero
         | x == ')'            -> mzero
         | otherwise           -> return ()
  let ender = if c == '(' then ')' else c
  let pEnder = char ender <* nfb (skip isAlphaNum)
  let regChunk = takeWhile1 (\x -> x /= ender && x /= '\\') <|> pEscaped
  let nestedChunk = (\x -> T.singleton c <> x <> T.singleton ender)
                      <$> pLinkTitle
  T.concat <$> manyTill (regChunk <|> nestedChunk) pEnder

-- Parse a list.
listParser :: Text -> Text -> BlockParser Blocks
listParser first first' = do
  let listStart = do
        initialSpaces <- takeWhile (==' ')
        listType <- parseListMarker
        return (initialSpaces, listType)
  (initialSpaces, listType) <-
        case parseOnly listStart first of
             Left _   -> fail "Could not parse list marker"
             Right r  -> return r
  lscanners <- gets lineScanners
  let scanMarkerWidth = () <$ count (listMarkerWidth listType) (skip (==' '))
  -- the indent required for blocks to be inside the list item:
  let scanContentsIndent = string initialSpaces >> scanMarkerWidth
  let starter = string initialSpaces *> nfb scanMarkerWidth *>
                    scanSpaces *> scanListStart (Just listType)
  -- beginning of a block in the list item must be indented by
  -- scancontentsindent.
  let blockScanner = scanContentsIndent <|> scanBlankline
  -- but a line within a block can omit that initial indent.
  let lineScanner = opt scanContentsIndent
  firstItem <- withBlockScanner blockScanner
               $ withLineScanner lineScanner
               $ parseLines False $ Just first'
  prev <- gets lastLine
  -- Check to see if there is blank space before the next list item.
  -- If not, we have a tight list.
  let isTight = case prev of
                     Nothing                 -> True  -- shouldn't happen
                     Just l
                       | isEmptyLine l       -> False
                       | otherwise           ->
                           case applyScanners lscanners l of
                                Just x | isEmptyLine x  -> False
                                _                       -> True
  restItems <- listItemsParser isTight starter blockScanner lineScanner
  let isTight' = isTight || null restItems  -- a one-item list is tight.
  return $ singleton $ List isTight' listType (firstItem:restItems)

-- Parse items after first list item.
listItemsParser :: Bool -> Scanner -> Scanner -> Scanner -> BlockParser [Blocks]
listItemsParser isTight starter blockScanner lineScanner = do
  -- Get next line and chomp off list marker.
  (mbFirst,_) <- peekTwoLines
  bscanners <- gets blockScanners
  case mbFirst >>= applyScanners (bscanners ++ [starter]) of
       Nothing     -> return []
       Just first' -> do
         item <- withBlockScanner blockScanner
                 $ withLineScanner lineScanner
                 $ parseLines False $ Just first'
         prev <- gets lastLine
         rest <- case prev of
                      -- We stop list parsing if we go from tight to loose:
                      Just l | isEmptyLine l && isTight -> return []
                      _  ->
                       listItemsParser isTight starter blockScanner lineScanner
         return (item:rest)

-- Parsers that recognize character escapes.

-- Parses an escaped character and returns a Text.
pEscaped :: Parser Text
pEscaped = T.singleton <$> (skip (=='\\') *> satisfy isEscapable)

-- Parses a (possibly escaped) character satisfying the predicate.
pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy p =
  satisfy (\c -> c /= '\\' && p c)
   <|> (char '\\' *> satisfy (\c -> isEscapable c && p c))

-- Simple representation of HTML tag.
data HtmlTagType = Opening Text | Closing Text | SelfClosing Text deriving Show

-- Returns tag type and whole tag.
pHtmlTag :: Parser (HtmlTagType, Text)
pHtmlTag = do
  char '<'
  -- do not end the tag with a > character in a quoted attribute.
  closing <- (char '/' >> return True) <|> return False
  tagname <- takeWhile1 (\c -> isAlphaNum c || c == '?' || c == '!')
  let tagname' = T.toLower tagname
  let attr = do ss <- takeWhile isSpace
                x <- letter
                xs <- takeWhile (\c -> isAlphaNum c || c == ':')
                skip (=='=')
                v <- pQuoted '"' <|> pQuoted '\'' <|> takeWhile1 isAlphaNum
                      <|> return ""
                return $ ss <> T.singleton x <> xs <> "=" <> v
  attrs <- T.concat <$> many attr
  final <- takeWhile (\c -> isSpace c || c == '/')
  char '>'
  let tagtype = if closing
                   then Closing tagname'
                   else case T.stripSuffix "/" final of
                         Just _  -> SelfClosing tagname'
                         Nothing -> Opening tagname'
  return (tagtype,
          T.pack ('<' : ['/' | closing]) <> tagname <> attrs <> final <> ">")

-- Parses a quoted attribute value.
pQuoted :: Char -> Parser Text
pQuoted c = do
  skip (== c)
  contents <- takeTill (== c)
  skip (== c)
  return (T.singleton c <> contents <> T.singleton c)

-- Parses an HTML comment. This isn't really correct to spec, but should
-- do for now.
pHtmlComment :: Parser Text
pHtmlComment = do
  string "<!--"
  rest <- manyTill anyChar (string "-->")
  return $ "<!--" <> T.pack rest <> "-->"

-- List of block level tags for HTML 5.
blockHtmlTags :: Set.Set Text
blockHtmlTags = Set.fromList
 [ "article", "header", "aside", "hgroup", "blockquote", "hr",
   "body", "li", "br", "map", "button", "object", "canvas", "ol",
   "caption", "output", "col", "p", "colgroup", "pre", "dd",
   "progress", "div", "section", "dl", "table", "dt", "tbody",
   "embed", "textarea", "fieldset", "tfoot", "figcaption", "th",
   "figure", "thead", "footer", "footer", "tr", "form", "ul",
   "h1", "h2", "h3", "h4", "h5", "h6", "video"]

-- Parses an HTML block: block-level content in balanced tags
-- (or an unbalanced hr or br), or an HTML comment.  The docs
-- say: "block-level HTML elements — e.g.
-- <div>, <table>, <pre>, <p>, etc. — must be separated from
-- surrounding content by blank lines, and the start and end tags
-- of the block should not be indented with tabs or spaces."
-- We don't enforce the requirement that end tags be unindented,
-- which seems unnecessary and is probably left over from when
-- Markdown.pl didn't have a way of handling balanced tags.
htmlBlockParser :: Text -> Text -> BlockParser Blocks
htmlBlockParser ln _ = do
  advance -- consume first line
  lns <- withLineScanner (nfb scanBlankline) $ getLines lineScanners
  return $ singleton $ HtmlBlock $ joinLines $ {- map T.stripEnd -} (ln:lns)

-- Parse a text into inlines, resolving reference links
-- using the reference map.
parseInlines :: ReferenceMap -> Text -> Inlines
parseInlines refmap t =
  case parseOnly (msum <$> many (pInline refmap) <* endOfInput) t of
       Left e   -> error ("parseInlines: " ++ show e) -- should not happen
       Right r  -> r

pInline :: ReferenceMap -> Parser Inlines
pInline refmap =
           pSpace
       <|> pStr
       <|> pEnclosure '*' refmap  -- strong/emph
       <|> pEnclosure '_' refmap
       <|> pLink refmap
       <|> pImage refmap
       <|> pCode
       <|> pEntity
       <|> pRawHtml
       <|> pAutolink
       <|> pSym

-- Parse spaces or newlines, and determine whether
-- we have a regular space, a line break (two spaces before
-- a newline), or a soft break (newline without two spaces
-- before).
pSpace :: Parser Inlines
pSpace = do
  ss <- takeWhile1 isWhitespace
  return $ singleton
         $ if T.any (=='\n') ss
              then if "  " `T.isPrefixOf` ss
                   then LineBreak
                   else SoftBreak
              else Space

-- Parse a string.  We include internal underscores,
-- so they won't trigger emphasis.
pStr :: Parser Inlines
pStr = do
  x  <- takeWhile1 isWordChar
  xs <- many $ (T.append <$> takeWhile (== '_') <*> takeWhile1 isWordChar)
          <|>  (T.append <$> takeWhile (== ' ') <*> takeWhile1 isWordChar)
  let sq = Seq.fromList . intersperse Space . map Str . T.words . T.concat
           $ (x:xs)
  case viewr sq of
       (rest :> Str ys)
         | T.all (inClass "a-zA-Z0-9-") ys && ys `Set.member` schemeSet ->
             ((rest `mappend`) <$> pUri ys) <|> return sq
       _ -> return sq
 where isWordChar :: Char -> Bool
       -- This is a dispensable optimization over isAlphaNum, covering
       -- common cases first.
       isWordChar c
         | c >= 'a' && c <= 'z' = True
         | c >= 'A' && c <= 'Z' = True
         | c >= '0' && c <= '9' = True
       isWordChar ':' = False -- otherwise URL detection breaks
       isWordChar ',' = True  -- but we allow other punctuation
       isWordChar '.' = True
       isWordChar '-' = True
       isWordChar ';' = True
       isWordChar '(' = True
       isWordChar ')' = True
       isWordChar ' ' = False
       isWordChar '\n' = False
       isWordChar '_' = False
       isWordChar c = isAlphaNum c

-- Catch all -- parse an escaped character, an escaped
-- newline, or any remaining symbol character.
pSym :: Parser Inlines
pSym = do
  c <- anyChar
  let ch = singleton . Str . T.singleton
  if c == '\\'
     then ch <$> satisfy isEscapable
          <|> singleton LineBreak <$ satisfy (=='\n')
          <|> return (ch '\\')
     else return (ch c)

-- http://www.iana.org/assignments/uri-schemes.html plus
-- the unofficial schemes coap, doi, javascript.
schemes :: [Text]
schemes = [ -- unofficial
            "coap","doi","javascript"
           -- official
           ,"aaa","aaas","about","acap"
           ,"cap","cid","crid","data","dav","dict","dns","file","ftp"
           ,"geo","go","gopher","h323","http","https","iax","icap","im"
           ,"imap","info","ipp","iris","iris.beep","iris.xpc","iris.xpcs"
           ,"iris.lwz","ldap","mailto","mid","msrp","msrps","mtqp"
           ,"mupdate","news","nfs","ni","nih","nntp","opaquelocktoken","pop"
           ,"pres","rtsp","service","session","shttp","sieve","sip","sips"
           ,"sms","snmp","soap.beep","soap.beeps","tag","tel","telnet","tftp"
           ,"thismessage","tn3270","tip","tv","urn","vemmi","ws","wss"
           ,"xcon","xcon-userid","xmlrpc.beep","xmlrpc.beeps","xmpp","z39.50r"
           ,"z39.50s"
           -- provisional
           ,"adiumxtra","afp","afs","aim","apt","attachment","aw"
           ,"beshare","bitcoin","bolo","callto","chrome","chrome-extension"
           ,"com-eventbrite-attendee","content","cvs","dlna-playsingle"
           ,"dlna-playcontainer","dtn","dvb","ed2k","facetime","feed"
           ,"finger","fish","gg","git","gizmoproject","gtalk"
           ,"hcp","icon","ipn","irc","irc6","ircs","itms","jar"
           ,"jms","keyparc","lastfm","ldaps","magnet","maps","market"
           ,"message","mms","ms-help","msnim","mumble","mvn","notes"
           ,"oid","palm","paparazzi","platform","proxy","psyc","query"
           ,"res","resource","rmi","rsync","rtmp","secondlife","sftp"
           ,"sgn","skype","smb","soldat","spotify","ssh","steam","svn"
           ,"teamspeak","things","udp","unreal","ut2004","ventrilo"
           ,"view-source","webcal","wtai","wyciwyg","xfire","xri"
           ,"ymsgr" ]

-- Make them a set for more efficient lookup.
schemeSet :: Set.Set Text
schemeSet = Set.fromList $ schemes ++ map T.toUpper schemes

isUriChar :: Char -> Bool
isUriChar c = not (isPunctuation c) && (not (isAscii c) || isAllowedInURI c)

inParens :: Parser Text
inParens = do char '('
              res <- takeWhile isUriChar
              char ')'
              return $ "(" <> res <> ")"

innerPunct :: Parser Text
innerPunct = T.singleton <$> (char '/'
        <|> (pSatisfy isPunctuation <* nfb space <* nfb endOfInput))

-- Parse a URI, using heuristics to avoid capturing final punctuation.
pUri :: Text -> Parser Inlines
pUri scheme = do
  char ':'
  -- Scan non-ascii characters and ascii characters allowed in a URI.
  -- We allow punctuation except when followed by a space, since
  -- we don't want the trailing '.' in 'http://google.com.'
  -- We want to allow
  -- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
  -- as a URL, while NOT picking up the closing paren in
  -- (http://wikipedia.org)
  -- So we include balanced parens in the URL.
  let uriChunk = takeWhile1 isUriChar <|> inParens <|> innerPunct
  rest <- T.concat <$> many1 uriChunk
  -- now see if they amount to an absolute URI
  let rawuri = scheme <> ":" <> rest
  case parseURI (T.unpack $ escapeUri rawuri) of
       Just uri' -> return $ singleton $ Link (singleton $ Str rawuri)
                                  (T.pack $ show uri') (T.empty)
       Nothing   -> fail "not a URI"

-- Escape a URI.
escapeUri :: Text -> Text
escapeUri = T.pack . escapeURIString
               (\c -> isAscii c && not (isSpace c)) . T.unpack

-- Parses material enclosed in *s, **s, _s, or __s.
-- Designed to avoid backtracking.
pEnclosure :: Char -> ReferenceMap -> Parser Inlines
pEnclosure c refmap = do
  cs <- takeWhile1 (== c)
  (Str cs <|) <$> pSpace
   <|> case T.length cs of
            3  -> pThree c refmap
            2  -> pTwo c refmap empty
            1  -> pOne c refmap empty
            _  -> return (singleton $ Str cs)

-- singleton sequence or empty if contents are empty
single :: (Inlines -> Inline) -> Inlines -> Inlines
single constructor ils = if Seq.null ils
                            then empty
                            else singleton (constructor ils)

-- parse inlines til you hit a c, and emit Emph.
-- if you never hit a c, emit '*' + inlines parsed.
pOne :: Char -> ReferenceMap -> Inlines -> Parser Inlines
pOne c refmap prefix = do
  contents <- msum <$> many ( (nfbChar c >> pInline refmap)
                             <|> (string (T.pack [c,c]) >>
                                  nfbChar c >> pTwo c refmap mempty) )
  (char c >> return (single Emph $ prefix <> contents))
    <|> return (singleton (Str (T.singleton c)) <> (prefix <> contents))

-- parse inlines til you hit two c's, and emit Strong.
-- if you never do hit two c's, emit '**' plus + inlines parsed.
pTwo :: Char -> ReferenceMap -> Inlines -> Parser Inlines
pTwo c refmap prefix = do
  let ender = string $ T.pack [c,c]
  contents <- msum <$> many (nfb ender >> pInline refmap)
  (ender >> return (single Strong $ prefix <> contents))
    <|> return (singleton (Str $ T.pack [c,c]) <> (prefix <> contents))

-- parse inlines til you hit one c or a sequence of two c's.
-- If one c, emit Emph and then parse pTwo.
-- if two c's, emit Strong and then parse pOne.
pThree :: Char -> ReferenceMap -> Parser Inlines
pThree c refmap = do
  contents <- msum <$> (many (nfbChar c >> pInline refmap))
  (string (T.pack [c,c]) >> (pOne c refmap (single Strong contents)))
   <|> (char c >> (pTwo c refmap (single Emph contents)))
   <|> return (singleton (Str $ T.pack [c,c,c]) <> contents)

-- Inline code span.
pCode :: Parser Inlines
pCode = fst <$> pCode'

-- this is factored out because it needed in pLinkLabel.
pCode' :: Parser (Inlines, Text)
pCode' = do
  ticks <- takeWhile1 (== '`')
  let end = string ticks >> nfb (char '`')
  let nonBacktickSpan = takeWhile1 (/= '`')
  let backtickSpan = takeWhile1 (== '`')
  contents <- T.concat <$> manyTill (nonBacktickSpan <|> backtickSpan) end
  return (singleton . Code . T.strip $ contents, ticks <> contents <> ticks)

pLink :: ReferenceMap -> Parser Inlines
pLink refmap = do
  lab <- pLinkLabel
  let lab' = parseInlines refmap lab
  pInlineLink lab' <|> pReferenceLink refmap lab lab'
    -- fallback without backtracking if it's not a link:
    <|> return (singleton (Str "[") <> lab' <> singleton (Str "]"))

-- An inline link: [label](/url "optional title")
pInlineLink :: Inlines -> Parser Inlines
pInlineLink lab = do
  char '('
  scanSpaces
  url <- pLinkUrl
  tit <- option "" $ scanSpnl *> pLinkTitle <* scanSpaces
  char ')'
  return $ singleton $ Link lab url tit

-- A reference link: [label], [foo][label], or [label][].
pReferenceLink :: ReferenceMap -> Text -> Inlines -> Parser Inlines
pReferenceLink refmap rawlab lab = do
  ref <- option rawlab $ scanSpnl >> pLinkLabel
  let ref' = if T.null ref then rawlab else ref
  case lookupLinkReference refmap ref' of
       Just (url,tit)  -> return $ singleton $ Link lab url tit
       Nothing         -> fail "Reference not found"

-- An image:  ! followed by a link.
pImage :: ReferenceMap -> Parser Inlines
pImage refmap = do
  char '!'
  let linkToImage (Link lab url tit) = Image lab url tit
      linkToImage x                  = x
  fmap linkToImage <$> pLink refmap

-- An entity.  We store these in a special inline element.
-- This ensures that entities in the input come out as
-- entities in the output. Alternatively we could simply
-- convert them to characters and store them as Str inlines.
pEntity :: Parser Inlines
pEntity = do
  char '&'
  res <- pCharEntity <|> pDecEntity <|> pHexEntity
  char ';'
  return $ singleton $ Entity $ "&" <> res <> ";"

pCharEntity :: Parser Text
pCharEntity = takeWhile1 (\c -> isAscii c && isLetter c)

pDecEntity :: Parser Text
pDecEntity = do
  char '#'
  res <- takeWhile1 isDigit
  return $ "#" <> res

pHexEntity :: Parser Text
pHexEntity = do
  char '#'
  x <- char 'X' <|> char 'x'
  res <- takeWhile1 isHexDigit
  return $ "#" <> T.singleton x <> res

-- Raw HTML tag or comment.
pRawHtml :: Parser Inlines
pRawHtml = singleton . RawHtml <$> (snd <$> pHtmlTag <|> pHtmlComment)

-- A link like this: <http://whatever.com> or <me@mydomain.edu>.
-- Markdown.pl does email obfuscation; we don't bother with that here.
pAutolink :: Parser Inlines
pAutolink = do
  skip (=='<')
  s <- takeWhile1 (\c -> c /= ':' && c /= '@')
  rest <- takeWhile1 (\c -> c /='>' && c /= ' ')
  skip (=='>')
  case True of
       _ | "@" `T.isPrefixOf` rest -> return $ emailLink (s <> rest)
         | s `Set.member` schemeSet -> return $ autoLink (s <> rest)
         | otherwise   -> fail "Unknown contents of <>"

autoLink :: Text -> Inlines
autoLink t = singleton $ Link (singleton $ Str t) (escapeUri t) (T.empty)

emailLink :: Text -> Inlines
emailLink t = singleton $ Link (singleton $ Str t)
                               (escapeUri $ "mailto:" <> t) (T.empty)


-- Parse text into a sequence of blocks.  There is no "failure"
-- return status, because any input text should be considered a
-- valid markdown document, and any error indicates a programming
-- problem.
parseMarkdown :: Text -> Blocks
parseMarkdown t = processBlocks refmap bls
  where (bls, refmap) = parseBlocks (t <> "\n")

-- Process a sequence of blocks, parsing Markdown elements into
-- Inlines and resolving link references.
processBlocks :: ReferenceMap -> Blocks -> Blocks
processBlocks refmap = fmap processBl
  where processInlines = foldMap processInline
        processInline (Markdown t) = parseInlines refmap t
        processInline x = singleton x
        processBl :: Block -> Block
        processBl bl =
          case bl of
            Para ils -> Para $ processInlines ils
            Header n ils -> Header n $ processInlines ils
            Blockquote bls -> Blockquote $ processBlocks refmap bls
            List tight listType items ->
               List tight listType $ map (processBlocks refmap) items
            x -> x  -- other block elements are terminal nodes

-- Render a sequence of blocks as HTML5.  Currently a single
-- newline is used between blocks, an a newline is used as a
-- separator e.g. for list items. These can be changed by adjusting
-- nl and blocksep.  Eventually we probably want these as parameters
-- or options.
renderBlocks :: Blocks -> Html
renderBlocks = mconcat . intersperse blocksep . map renderBlock . toList
  where renderBlock :: Block -> Html
        renderBlock (Header n ils)
          | n >= 1 && n <= 6 = ([H.h1,H.h2,H.h3,H.h4,H.h5,H.h6] !! (n - 1))
                                  $ renderInlines ils
          | otherwise        = H.p (renderInlines ils)
        renderBlock (Para ils) = H.p (renderInlines ils)
        renderBlock (HRule) = H.hr
        renderBlock (Blockquote bs) = H.blockquote $ nl <> renderBlocks bs <> nl
        renderBlock (CodeBlock attr t) =
          case codeLang attr of
                Nothing   -> base
                Just lang -> base ! A.class_ (toValue lang)
          where base = H.pre $ H.code $ toHtml t
        renderBlock (List tight (Bullet _) items) =
          H.ul $ nl <> mapM_ (li tight) items
        renderBlock (List tight (Numbered _ n) items) =
          if n == 1 then base else base ! A.start (toValue n)
          where base = H.ol $ nl <> mapM_ (li tight) items
        renderBlock (HtmlBlock raw) = H.preEscapedToMarkup raw
        li :: Bool -> Blocks -> Html  -- tight list handling
        li True bs = case toList bs of
                          [Para zs]         -> H.li (renderInlines zs) <> nl
                          [Para zs, List{}] -> H.li (renderInlines zs <>
                             nl <> renderBlocks (Seq.drop 1 bs)) <> nl
                          _                 -> toLi bs
                          -- An item in a tight list with multiple paragraphs
                          -- will be rendered as in a loose list.
        li False bs = toLi bs
        toLi x = (H.li $ renderBlocks x) <> nl
        nl = "\n"
        blocksep = "\n"

-- Render a sequence of inlines as HTML5.
renderInlines :: Inlines -> Html
renderInlines = foldMap renderInline
  where renderInline :: Inline -> Html
        renderInline (Str t) = toHtml t
        renderInline Space   = " "
        renderInline SoftBreak = "\n" -- this preserves the line breaks in the
                                      -- markdown document; replace with " " if this
                                      -- isn't wanted.
        renderInline LineBreak = H.br <> "\n"
        renderInline (Emph ils) = H.em $ renderInlines ils
        renderInline (Strong ils) = H.strong $ renderInlines ils
        renderInline (Code t) = H.code $ toHtml t
        renderInline (Link ils url tit) =
          if T.null tit then base else base ! A.title (toValue tit)
          where base = H.a ! A.href (toValue url) $ renderInlines ils
        renderInline (Image ils url tit) =
          if T.null tit then base else base ! A.title (toValue tit)
          where base = H.img ! A.src (toValue url)
                             ! A.alt (toValue $ BT.renderHtml -- TODO strip tags
                                              $ renderInlines ils)
        renderInline (Entity t) = H.preEscapedToMarkup t
        renderInline (RawHtml t) = H.preEscapedToMarkup t
        renderInline (Markdown t) = toHtml t -- shouldn't happen
