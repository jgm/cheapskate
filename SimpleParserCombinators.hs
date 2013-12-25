module SimpleParserCombinators where
import Prelude hiding (takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Data.Monoid

data ParseError = ParseError Int String deriving Show

data ParserState = ParserState { subject :: Text
                               , position :: Int
                               }

newtype Parser a = Parser {
  evalParser :: ParserState -> Either ParseError (ParserState, a)
  }

instance Functor Parser where
  fmap f (Parser g) = Parser $ \st ->
    case g st of
         Right (st', x) -> Right (st', f x)
         Left e         -> Left e

instance Applicative Parser where
  pure x = Parser $ \st -> Right (st, x)
  (Parser f) <*> (Parser g) = Parser $ \st ->
    case f st of
         Left e         -> Left e
         Right (st', h) -> case g st' of
                                Right (st'', x) -> Right (st'', h x)
                                Left e          -> Left e

instance Alternative Parser where
  empty = Parser $ \st -> Left $ ParseError (position st) "empty"
  (Parser f) <|> (Parser g) = Parser $ \st ->
    case f st of
         Right res  -> Right res
         _          -> g st

instance Monad Parser where
  return x = Parser $ \st -> Right (st, x)
  p >>= g = Parser $ \st ->
    case evalParser p st of
         Left e        -> Left e
         Right (st',x) -> evalParser (g x) st'

instance MonadPlus Parser where
  mzero = Parser $ \st -> Left $ ParseError (position st) "mzero"
  mplus p1 p2 = Parser $ \st ->
    case evalParser p1 st of
         Right res  -> Right res
         Left e     -> evalParser p2 st

runParser :: Parser a -> Text -> Either ParseError a
runParser p t =
  fmap snd $ evalParser p ParserState{ subject = t, position = 0 }

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser g
  where g st = case T.uncons (subject st) of
                    Just (c, t') | f c ->
                         Right (st{ subject = t', position = position st + 1 },
                                c)
                    _ -> Left $ ParseError (position st) "satisfy"

peekChar :: Parser (Maybe Char)
peekChar = Parser $ \st ->
             case T.uncons (subject st) of
                  Just (c, _) -> Right (st, Just c)
                  Nothing     -> Right (st, Nothing)

-- not efficient as in attoparsec
inClass :: String -> Char -> Bool
inClass s c = c `elem` s

notInClass :: String -> Char -> Bool
notInClass s = not . inClass s

endOfInput :: Parser ()
endOfInput = Parser $ \st ->
  if T.null (subject st)
     then Right (st, ())
     else Left $ ParseError (position st) "endOfInput"

char :: Char -> Parser Char
char c = satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy (const True)

getPosition :: Parser Int
getPosition = Parser $ \st -> Right (st, position st)

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile f = Parser $ \st ->
  let (t, rest) = T.span f (subject st) in
  Right (st{ subject = rest, position = position st + T.length t }, t)

takeTill :: (Char -> Bool) -> Parser Text
takeTill f = takeWhile (not . f)

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 f = Parser $ \st ->
  case T.span f (subject st) of
       (t, rest) | T.null t -> Left $ ParseError (position st) "takeWhile1"
                 | otherwise -> Right
                                (st { subject = rest
                                    , position = position st + T.length t }, t)

takeText :: Parser Text
takeText = Parser $ \st ->
  let t = subject st in
  Right (st{ subject = mempty, position = position st + T.length t }, t)

skip :: (Char -> Bool) -> Parser ()
skip f = Parser $ \st ->
  case T.uncons (subject st) of
       Just (c,t') | f c -> Right (st{ subject = t'
                                     , position = position st + 1 }, ())
       _                 -> Left $ ParseError (position st) "skip"

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile f = Parser $ \st ->
  let (t', rest) = T.span f (subject st) in
  Right (st{ subject = rest, position = position st + T.length t' }, ())

string :: Text -> Parser Text
string s = Parser $ \st ->
  case T.stripPrefix s (subject st) of
       Just t' -> Right (st{ subject = t'
                           , position = position st + T.length s }, s)
       Nothing -> Left $ ParseError (position st) "string"

scan :: s -> (s -> Char -> Maybe s) -> Parser Text
scan s0 f = Parser $ go 0 s0 f
  where go charsParsed s f st =
         case T.uncons (subject st) of
               Nothing        -> finish charsParsed st
               Just (c, rest) -> case f s c of
                                  Just s' -> go (charsParsed + 1) s' f st
                                  Nothing -> finish charsParsed st
        finish charsParsed st =
            let (t', rest) = T.splitAt charsParsed (subject st) in
            Right (st{ subject = T.drop charsParsed (subject st)
                     , position = position st + charsParsed },
                   T.take charsParsed (subject st))

-- combinators (definitions borrowed from attoparsec)

option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x

many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p)

manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = scan
    where scan = (end *> pure []) <|> liftA2 (:) p scan

skipMany :: Alternative f => f a -> f ()
skipMany p = scan
    where scan = (p *> scan) <|> pure ()

skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p

count :: Monad m => Int -> m a -> m [a]
count n p = sequence (replicate n p)


