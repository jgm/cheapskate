{-# LANGUAGE OverloadedStrings #-}
module Cheapskate.Blocks (Container(..), parseContainers) where
import Data.Char
import qualified Data.Set as Set
import Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.Sequence (Seq, (|>), (><), viewr, ViewR(..))
import qualified Data.Sequence as Seq
import Control.Monad.RWS
import Control.Monad
import qualified Data.Map as M
import Cheapskate.Types
import Control.Applicative

import Debug.Trace
tr' s x = trace (s ++ ": " ++ show x) x

type ContainerStack = [Container]

type ReferenceMap = M.Map Text (Text, Text)

data Container = Container{
                     containerType :: ContainerType
                   , children      :: Seq Container
                   }
                | TextLines (Seq Text)
                | ATXHeader Int Text
                | Rule
                | BlankLine
                deriving (Show)

data ContainerType = Document | BlockQuote
     deriving Show

type ContainerM = RWS () ReferenceMap ContainerStack

parseContainers :: Text -> (Container, ReferenceMap)
parseContainers t = (closeStack stack, refmap)
  where
  (stack, refmap) = execRWS (mapM_ processLine $ toLines t) () startState
  toLines    = map tabFilter . T.lines
  startState = [Container Document mempty]

addChild :: Container -> Container -> Container
addChild child (Container ct cs) = Container ct (cs |> child)
addChild child c             = error "leaf container cannot have children"

closeStack :: ContainerStack -> Container
closeStack []       = error "empty stack"
closeStack [x]      = x
closeStack (x:y:zs) = closeStack (addChild x y : zs)

addToStack :: Container -> ContainerM ()
addToStack cont = do
  (top:rest) <- get
  put $ case (top, cont) of
        -- top should always be Container
        (Container ct cs, Container ct' cs') -> Container ct' cs' : Container ct cs : rest
        (Container ct cs, TextLines t) ->
          case viewr cs of
            (cs' :> TextLines t') -> Container ct (cs' |> TextLines (t' >< t)) : rest
            _ -> Container ct (cs |> TextLines t) : rest
        (Container ct cs, c) -> Container ct (cs |> c) : rest
        _ -> error "top of stack must be a Container"

tryScanners :: [Container] -> Text -> (Text, Int)
tryScanners [] t = (t, 0)
tryScanners (c:cs) t =
  case parseOnly (scanner >> takeText) t of
       Right t'   -> tryScanners cs t'
       Left _err  -> (t, length (c:cs))
  where scanner = case c of
                       (Container BlockQuote _)  -> scanBlockquoteStart
                       _                         -> return ()

containerize :: Text -> [Container]
containerize t =
  case parseOnly ((,) <$> many blockStart <*> takeText) t of
       Right (cs,t') -> cs ++ if isEmptyLine t'
                                 then [BlankLine]
                                 else [TextLines $ Seq.singleton t']
       Left err      -> error err

blockStart :: Parser Container
blockStart =
     (Container BlockQuote mempty <$ scanBlockquoteStart)
 <|> (ATXHeader <$> parseAtxHeaderStart <*> (T.dropWhileEnd (`elem` " #") <$> takeText))

processLine :: Text -> ContainerM ()
processLine t = do
  (top@(Container ct cs) : rest) <- get  -- assumes stack is never empty
  let (t', numUnmatched) = tryScanners (reverse $ top:rest) t
  case containerize t' of
       [TextLines nt] ->
           case viewr cs of
              -- lazy continuation?
             (cs' :> TextLines ot) -> addToStack (TextLines nt)
             _ -> modify (drop numUnmatched) >> addToStack (TextLines nt)
       ns -> do -- close unmatched containers, add new ones
           modify $ drop numUnmatched
           mapM_ addToStack ns

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
scanNonindentSpaces = do
  xs <- takeWhile (==' ')
  if T.length xs > 3 then mzero else return ()

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

-- Parses an escaped character and returns a Text.
pEscaped :: Parser Text
pEscaped = T.singleton <$> (skip (=='\\') *> satisfy isEscapable)

-- Parses a (possibly escaped) character satisfying the predicate.
pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy p =
  satisfy (\c -> c /= '\\' && p c)
   <|> (char '\\' *> satisfy (\c -> isEscapable c && p c))

-- this is factored out because it needed in pLinkLabel.
pCode' :: Parser (Inlines, Text)
pCode' = do
  ticks <- takeWhile1 (== '`')
  let end = string ticks >> nfb (char '`')
  let nonBacktickSpan = takeWhile1 (/= '`')
  let backtickSpan = takeWhile1 (== '`')
  contents <- T.concat <$> manyTill (nonBacktickSpan <|> backtickSpan) end
  return (Seq.singleton . Code . T.strip $ contents, ticks <> contents <> ticks)


