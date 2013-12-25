{-# LANGUAGE OverloadedStrings #-}
module Cheapskate.Parse (parseMarkdown, processLines {- TODO for now -}) where
import SimpleParserCombinators
import Data.Char hiding (Space)
import qualified Data.Set as Set
import Prelude hiding (takeWhile)
import Data.Maybe (mapMaybe)
import Data.List (intercalate, intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.Foldable (toList)
import Data.Sequence (Seq, (<|), (|>), viewr, ViewR(..), singleton)
import qualified Data.Sequence as Seq
import Network.URI (escapeURIString)
import Control.Monad.RWS
import qualified Data.Map as M
import Cheapskate.Types
import Control.Applicative

-- import Debug.Trace
-- tr' s x = trace (s ++ ": " ++ show x) x

parseMarkdown :: Text -> Blocks
parseMarkdown = processDocument . processLines

data ContainerStack =
  ContainerStack Container {- top -} [Container] {- rest -}

type ReferenceMap = M.Map Text (Text, Text)
-- Link references are case sensitive and ignore line breaks
-- and repeated spaces.
-- So, [APPLES are good] == [Apples are good] ==
-- [Apples
-- are     good].
normalizeReference :: Text -> Text
normalizeReference = T.toUpper . T.concat . T.split isWhitespace

lookupLinkReference :: ReferenceMap
                    -> Text                -- reference label
                    -> Maybe (Text, Text)  -- (url, title)
lookupLinkReference refmap key = M.lookup (normalizeReference key) refmap

type ColumnNumber = Int
type LineNumber   = Int

data Elt = C Container
         | L LineNumber Leaf
         deriving Show

data Container = Container{
                     containerType :: ContainerType
                   , children      :: Seq Elt
                   }

data ContainerType = Document
                   | BlockQuote
                   | ListItem { listIndent :: Int, listType :: ListType }
                   | FencedCode { fence :: Text, info :: Text }
                   | IndentedCode
                   | RawHtmlBlock { openingHtml :: Text }
                   deriving (Eq, Show)

instance Show Container where
  show c = show (containerType c) ++ "\n" ++
    nest 2 (intercalate "\n" (map showElt $ toList $ children c))

nest :: Int -> String -> String
nest num = intercalate "\n" . map ((replicate num ' ') ++) . lines

showElt :: Elt -> String
showElt (C c) = show c
showElt (L _ (TextLine s)) = show s
showElt (L _ lf) = show lf

listMarkerWidth :: ListType -> Int
listMarkerWidth (Bullet _) = 1
listMarkerWidth (Numbered _ n) | n < 10    = 1
                               | n < 100   = 2
                               | n < 1000  = 3
                               | otherwise = 4

data Leaf = TextLine Text
          | BlankLine Text
          | ATXHeader Int Text
          | SetextHeader Int Text
          | Rule
          | Reference{ referenceLabel :: Text, referenceURL :: Text, referenceTitle :: Text }
          deriving (Show)

type ContainerM = RWS () ReferenceMap ContainerStack

processDocument :: (Container, ReferenceMap) -> Blocks
processDocument (Container ct cs, refmap) =
  case ct of
    Document -> processElts refmap (toList cs)
    _        -> error "top level container is not Document"

processElts :: ReferenceMap -> [Elt] -> Blocks
processElts _ [] = mempty
processElts refmap (L _lineNumber lf : rest) =
  case lf of
    TextLine t -> singleton (Para $ parseInlines refmap txt) <>
                  processElts refmap rest'
               where txt = T.stripEnd $ joinLines $ map T.stripStart
                           $ t : map extractText textlines
                     (textlines, rest') = span isTextLine rest
                     isTextLine (L _ (TextLine _)) = True
                     isTextLine _ = False
    BlankLine{} -> processElts refmap rest
    ATXHeader lvl t -> singleton (Header lvl $ parseInlines refmap t) <>
                       processElts refmap rest
    SetextHeader lvl t -> singleton (Header lvl $ parseInlines refmap t) <>
                          processElts refmap rest
    Rule -> singleton HRule <> processElts refmap rest
    Reference{} -> processElts refmap rest

processElts refmap (C (Container ct cs) : rest) =
  case ct of
    Document -> error "Document container found inside Document"
    BlockQuote -> singleton (Blockquote $ processElts refmap (toList cs)) <>
                  processElts refmap rest
    ListItem _ listType' ->
        singleton (List isTight listType' items') <> processElts refmap rest'
              where xs = takeListItems rest
                    rest' = drop (length xs) rest
                    takeListItems (C c@(Container (ListItem _ lt') _) : zs)
                      | listTypesMatch lt' listType' = c : takeListItems zs
                      | otherwise = []
                    takeListItems _ = []
                    listTypesMatch (Bullet c1) (Bullet c2) = c1 == c2
                    listTypesMatch (Numbered w1 _) (Numbered w2 _) = w1 == w2
                    listTypesMatch _ _ = False
                    items = mapMaybe getItem (Container ct cs : xs)
                    getItem (Container ListItem{} cs') = Just $ toList cs'
                    getItem _                         = Nothing
                    items' = map (processElts refmap) items
                    itemsMinusTrailingBlanks =
                          reverse $ case reverse items of
                                         (w:ws) -> stripTrailingBlanks w : ws
                                         []     -> []
                    isTight = not $ any itemWithBlankLine
                              itemsMinusTrailingBlanks
                    -- this first case is an empty tight list item:
                    -- - x
                    -- -
                    -- - y
                    itemWithBlankLine [L _ (BlankLine _)] = False
                    itemWithBlankLine its = any isBlankLine its
    FencedCode _ info' -> singleton (CodeBlock attr txt) <>
                               processElts refmap rest
                  where txt = joinLines $ map extractText $ toList cs
                        attr = case T.words info' of
                                  []    -> CodeAttr Nothing
                                  (w:_) -> CodeAttr (Just w)
    IndentedCode -> singleton (CodeBlock (CodeAttr Nothing) txt)
                    <> processElts refmap rest'
                  where txt = joinLines $ stripTrailingEmpties
                              $ concatMap extractCode cbs
                        stripTrailingEmpties = reverse .
                          dropWhile (T.all (==' ')) . reverse
                        extractCode (L _ (BlankLine t)) = [t]
                        extractCode (C (Container IndentedCode cs')) =
                          map extractText $ toList cs'
                        extractCode _ = []
                        (cbs, rest') = span isIndentedCodeOrBlank
                                       (C (Container ct cs) : rest)
                        isIndentedCodeOrBlank (L _ BlankLine{}) = True
                        isIndentedCodeOrBlank (C (Container IndentedCode _))
                                                              = True
                        isIndentedCodeOrBlank _               = False

    RawHtmlBlock{ openingHtml = openingHtml' } ->
                        singleton (HtmlBlock txt) <> processElts refmap rest
                  where txt = openingHtml' <>
                               joinLines (map extractText (toList cs))

   where isBlankLine (L _ BlankLine{}) = True
         isBlankLine _ = False
         stripTrailingBlanks = reverse . dropWhile isBlankLine . reverse

  -- recursively generate blocks
  -- this requrse grouping text lines into paragraphs,
  -- and list items into lists, handling blank lines,
  -- parsing inline contents of texts and resolving refs.

extractText :: Elt -> Text
extractText (L _ (TextLine t)) = t
extractText _ = mempty

processLines :: Text -> (Container, ReferenceMap)
processLines t = (doc, refmap)
  where
  (doc, refmap) = evalRWS (mapM_ processLine lns >> closeStack) () startState
  lns        = zip [1..] (map tabFilter $ T.lines t)
  startState = ContainerStack (Container Document mempty) []

closeStack :: ContainerM Container
closeStack = do
  ContainerStack top rest  <- get
  if null rest
     then return top
     else closeContainer >> closeStack

closeContainer :: ContainerM ()
closeContainer = do
  ContainerStack top rest <- get
  case rest of
       (Container ct' cs' : rs) -> put $ ContainerStack (Container ct' (cs' |> C top)) rs
       [] -> fail "Cannot close last container on stack"

addLeaf :: LineNumber -> Leaf -> ContainerM ()
addLeaf lineNum lf = do
  ContainerStack top rest <- get
  case (top, lf) of
        (Container ct@(ListItem{}) cs, BlankLine{}) ->
          case viewr cs of
            (_ :> L _ BlankLine{}) -> -- two blanks break out of list item:
                 closeContainer >> addLeaf lineNum lf
            _ -> put $ ContainerStack (Container ct (cs |> L lineNum lf)) rest
        (Container ct cs, _) ->
                 put $ ContainerStack (Container ct (cs |> L lineNum lf)) rest

addContainer :: ContainerType -> ContainerM ()
addContainer ct = modify $ \(ContainerStack top rest) ->
  ContainerStack (Container ct mempty) (top:rest)

tryScanners :: [Container] -> ColumnNumber -> Text -> (Text, Int)
tryScanners [] _ t = (t, 0)
tryScanners (c:cs) colnum t =
  case runParser (scanner >> takeText) t of
       Right t'   -> tryScanners cs (colnum + T.length t - T.length t') t'
       Left _err  -> (t, length (c:cs))
  where scanner = case containerType c of
                       BlockQuote     -> scanBlockquoteStart
                       IndentedCode   -> scanIndentSpace
                       RawHtmlBlock{} -> nfb scanBlankline
                       ListItem{ listIndent = n, listType = lt }
                                      -> scanBlankline
                                      -- we require indent past marker,
                                      -- but allow an extra space so indented
                                      -- code begins where it should:
                                      -- 1. foobar
                                      --
                                      --        code
                                      <|> () <$
                                           (string (T.replicate (n + 1) " "))
                       _              -> return ()

containerize :: Bool -> Text -> ([ContainerType], Leaf)
containerize lastLineIsText t =
  case runParser newContainers t of
       Right (cs,t') -> (cs, t')
       Left err      -> error (show err)
  where newContainers = do
          regContainers <- many (containerStart lastLineIsText)
          verbatimContainers <- option []
                            $ count 1 (verbatimContainerStart lastLineIsText)
          if null verbatimContainers
             then (,) <$> pure regContainers <*> leaf lastLineIsText
             else (,) <$> pure (regContainers ++ verbatimContainers) <*>
                            textLineOrBlank
textLineOrBlank :: Parser Leaf
textLineOrBlank = consolidate <$> takeText
  where consolidate ts = if T.all (==' ') ts
                            then BlankLine ts
                            else TextLine ts

containerStart :: Bool -> Parser ContainerType
containerStart _lastLineIsText =
      (BlockQuote <$ scanBlockquoteStart)
  <|> parseListMarker

verbatimContainerStart :: Bool -> Parser ContainerType
verbatimContainerStart lastLineIsText = nfb scanBlankline *>
   (  parseCodeFence
  <|> (guard (not lastLineIsText) *> (IndentedCode <$ scanIndentSpace))
  <|> (guard (not lastLineIsText) *> (RawHtmlBlock <$> parseHtmlBlockStart))
   )

leaf :: Bool -> Parser Leaf
leaf lastLineIsText = scanNonindentSpace *> (
      (ATXHeader <$> parseAtxHeaderStart <*>
         (T.strip . removeATXSuffix <$> takeText))
  <|> (guard lastLineIsText *> (SetextHeader <$> parseSetextHeaderLine <*> pure mempty))
  <|> (Rule <$ scanHRuleLine)
  <|> (guard (not lastLineIsText) *> pReference)
  <|> textLineOrBlank
  )
  where removeATXSuffix t = case T.dropWhileEnd (`elem` " #") t of
                                 t' | T.null t' -> t'
                                      -- an escaped \#
                                    | T.last t' == '\\' -> t' <> "#"
                                    | otherwise -> t'


processLine :: (LineNumber, Text) -> ContainerM ()
processLine (lineNumber, txt) = do
  ContainerStack top@(Container ct cs) rest <- get
  let (t', numUnmatched) = tryScanners (reverse $ top:rest) 0 txt
  let lastLineIsText = numUnmatched == 0 &&
                       case viewr cs of
                            (_ :> L _ (TextLine _)) -> True
                            _                       -> False
  case ct of
    RawHtmlBlock{} | numUnmatched == 0 -> addLeaf lineNumber (TextLine t')
    IndentedCode   | numUnmatched == 0 -> addLeaf lineNumber (TextLine t')
    FencedCode{ fence = fence' } ->  -- here we don't check numUnmatched because we allow laziness
      if fence' `T.isPrefixOf` t'
         -- closing code fence
         then closeContainer
         else addLeaf lineNumber (TextLine t')
    _ -> case containerize lastLineIsText t' of
       ([], TextLine t) ->
         case viewr cs of
            -- lazy continuation?
            (_ :> L _ (TextLine _))
              | ct /= IndentedCode -> addLeaf lineNumber (TextLine t)
            _ -> replicateM numUnmatched closeContainer >> addLeaf lineNumber (TextLine t)
       ([], SetextHeader lev _) | numUnmatched == 0 ->
           case viewr cs of
             (cs' :> L _ (TextLine t)) -> -- replace last text line with setext header
               put $ ContainerStack (Container ct (cs' |> L lineNumber (SetextHeader lev t))) rest
               -- Note: the following case should not occur, since
               -- we don't add a SetextHeader leaf unless lastLineIsText.
             _ -> error "setext header line without preceding text line"
       (ns, lf) -> do -- close unmatched containers, add new ones
           replicateM numUnmatched closeContainer
           mapM_ addContainer ns
           case (reverse ns, lf) of
             -- don't add blank line at beginning of fenced code or html block
             (FencedCode{}:_,  BlankLine{}) -> return ()
             (_, Reference{ referenceLabel = lab,
                            referenceURL = url,
                            referenceTitle = tit }) ->
               tell (M.singleton (normalizeReference lab) (url, tit))
               >> addLeaf lineNumber lf
             _ -> addLeaf lineNumber lf

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

-- Scan the beginning of a blockquote:  up to three
-- spaces indent, the `>` character, and an optional space.
scanBlockquoteStart :: Scanner
scanBlockquoteStart =
  scanNonindentSpace >> scanChar '>' >> opt (scanChar ' ')

-- Scan four spaces.
scanIndentSpace :: Scanner
scanIndentSpace = () <$ count 4 (skip (==' '))

-- Scan 0-3 spaces.
scanNonindentSpace :: Scanner
scanNonindentSpace = () <$ upToCountChars 3 (==' ')

parseNonindentSpaces :: Parser Int
parseNonindentSpaces = T.length <$> upToCountChars 3 (==' ')

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
scanSpnl = scanSpaces *> opt (char '\n' *> scanSpaces)

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

upToCountChars :: Int -> (Char -> Bool) -> Parser Text
upToCountChars cnt f =
  scan 0 (\n c -> if n < cnt && f c then Just (n+1) else Nothing)

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
  char '#'
  hashes <- upToCountChars 5 (== '#')
  scanSpace <|> scanBlankline
  return $ T.length hashes + 1

parseSetextHeaderLine :: Parser Int
parseSetextHeaderLine = do
  d <- char '-' <|> char '='
  let lev = if d == '=' then 1 else 2
  many (char d)
  scanBlankline
  return lev

-- Scan a horizontal rule line: "...three or more hyphens, asterisks,
-- or underscores on a line by themselves. If you wish, you may use
-- spaces between the hyphens or asterisks."
scanHRuleLine :: Scanner
scanHRuleLine = do
  c <- satisfy $ inClass "*_-"
  count 2 $ scanSpaces >> char c
  skipWhile (\x -> x == ' ' || x == c)
  endOfInput

-- Parse an initial code fence line, returning
-- the fence part and the rest (after any spaces).
parseCodeFence :: Parser ContainerType
parseCodeFence = do
  c <- satisfy $ inClass "`~"
  count 2 (char c)
  extra <- takeWhile (== c)
  scanSpaces
  rawattr <- takeWhile (/='`')
  endOfInput
  return $ FencedCode { fence = T.pack [c,c,c] <> extra, info = rawattr }

-- Parse the start of an HTML block:  either an HTML tag or an
-- HTML comment, with no indentation.
parseHtmlBlockStart :: Parser Text
parseHtmlBlockStart = do
  initial <- (   (do t <- pHtmlTag
                     guard $ f $ fst t
                     return $ snd t)
               <|> string "<!--"
               <|> string "-->" )
  rest <- takeText
  return (initial <> rest)
 where f (Opening name) = name `Set.member` blockHtmlTags
       f (SelfClosing name) = name `Set.member` blockHtmlTags
       f (Closing name) = name `Set.member` blockHtmlTags

-- Parse a list marker and return the list type.
parseListMarker :: Parser ContainerType
parseListMarker = parseBullet <|> parseListNumber

-- Parse a bullet and return list type.
parseBullet :: Parser ContainerType
parseBullet = do
  ind <- parseNonindentSpaces
  c <- satisfy $ inClass "+*-"
  scanSpace <|> scanBlankline -- allow empty list item
  unless (c == '+')
    $ nfb $ (count 2 $ scanSpaces >> skip (== c)) >>
          skipWhile (\x -> x == ' ' || x == c) >> endOfInput -- hrule
  return $ ListItem { listType = Bullet c, listIndent = ind }

-- Parse a list number marker and return list type.
parseListNumber :: Parser ContainerType
parseListNumber = do
    ind <- parseNonindentSpaces
    num <- (read . T.unpack) <$> takeWhile1 isDigit
    wrap <-  PeriodFollowing <$ skip (== '.')
         <|> ParenFollowing <$ skip (== ')')
    scanSpace <|> scanBlankline
    return $ ListItem { listType = Numbered wrap num, listIndent = ind }

-- Returns tag type and whole tag.
pHtmlTag :: Parser (HtmlTagType, Text)
pHtmlTag = do
  char '<'
  -- do not end the tag with a > character in a quoted attribute.
  closing <- (char '/' >> return True) <|> return False
  tagname <- takeWhile1 (\c -> isAlphaNum c || c == '?' || c == '!')
  let tagname' = T.toLower tagname
  let attr = do ss <- takeWhile isSpace
                x <- satisfy isLetter
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

-- A link reference is a square-bracketed link label, a colon,
-- optional space or newline, a URL, optional space or newline,
-- and an optional link title.
pReference :: Parser Leaf
pReference = do
  lab <- pLinkLabel
  char ':'
  scanSpnl
  url <- pLinkUrl
  tit <- option T.empty $ scanSpnl >> pLinkTitle
  scanSpaces
  endOfInput
  return $ Reference { referenceLabel = lab, referenceURL = url, referenceTitle = tit }

-- Parses an escaped character and returns a Text.
pEscaped :: Parser Text
pEscaped = T.singleton <$> (skip (=='\\') *> satisfy isEscapable)

-- Parses a (possibly escaped) character satisfying the predicate.
pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy p =
  satisfy (\c -> c /= '\\' && p c)
   <|> (char '\\' *> satisfy (\c -> isEscapable c && p c))

-- Parse a text into inlines, resolving reference links
-- using the reference map.
parseInlines :: ReferenceMap -> Text -> Inlines
parseInlines refmap t =
  case runParser (msum <$> many (pInline refmap) <* endOfInput) t of
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

-- Parse a URI, using heuristics to avoid capturing final punctuation.
pUri :: Text -> Parser Inlines
pUri scheme = do
  char ':'
  x <- scan (OpenParens 0) uriScanner
  guard $ not $ T.null x
  let (rawuri, endingpunct) =
        case T.last x of
             c | c `elem` ".;?!:," ->
               (scheme <> ":" <> T.init x, singleton (Str (T.singleton c)))
             _ -> (scheme <> ":" <> x, mempty)
  return $ singleton (Link (singleton $ Str rawuri) rawuri mempty) <>
           endingpunct

-- Scan non-ascii characters and ascii characters allowed in a URI.
-- We allow punctuation except when followed by a space, since
-- we don't want the trailing '.' in 'http://google.com.'
-- We want to allow
-- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
-- as a URL, while NOT picking up the closing paren in
-- (http://wikipedia.org)
-- So we include balanced parens in the URL.

data OpenParens = OpenParens Int

uriScanner :: OpenParens -> Char -> Maybe OpenParens
uriScanner _ ' ' = Nothing
uriScanner (OpenParens n) '(' = Just (OpenParens (n + 1))
uriScanner (OpenParens n) ')'
  | n > 0 = Just (OpenParens (n - 1))
  | otherwise = Nothing
uriScanner st '+' = Just st
uriScanner st '/' = Just st
uriScanner _ c | isSpace c = Nothing
uriScanner st _ = Just st

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
            2  -> pTwo c refmap mempty
            1  -> pOne c refmap mempty
            _  -> return (singleton $ Str cs)

-- singleton sequence or empty if contents are empty
single :: (Inlines -> Inline) -> Inlines -> Inlines
single constructor ils = if Seq.null ils
                            then mempty
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
