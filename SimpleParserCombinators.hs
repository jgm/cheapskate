module SimpleParserCombinators where
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

char :: Char -> Parser Char
char c = satisfy (== c)

getPosition :: Parser Int
getPosition = Parser $ \st -> Right (st, position st)

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile f = Parser $ \st ->
  let (t, rest) = T.span f (subject st) in
  Right (st{ subject = rest, position = position st + T.length t }, t)

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
