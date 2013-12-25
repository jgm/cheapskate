module ParserCombinators where
import Prelude hiding (takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Control.Applicative

data Position = Position { line :: Int, column :: Int }

instance Show Position where
  show (Position ln cn) = "line " ++ show ln ++ " column " ++ show cn

data ParseError = ParseError Position String deriving Show

data ParserState = ParserState { subject :: Text
                               , position :: Position
                               }

advance :: ParserState -> Text -> ParserState
advance = T.foldl' go
  where go :: ParserState -> Char -> ParserState
        go st c = st{ subject = T.drop 1 (subject st)
                    , position = case c of
                                      '\n' -> Position { line =
                                                  line (position st) + 1
                                                  , column = 1 }
                                      _    -> Position { line =
                                                  line (position st)
                                                  , column =
                                                  column (position st) + 1
                                                  } }

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
  fail e = Parser $ \st -> Left $ ParseError (position st) e
  p >>= g = Parser $ \st ->
    case evalParser p st of
         Left e        -> Left e
         Right (st',x) -> evalParser (g x) st'

instance MonadPlus Parser where
  mzero = Parser $ \st -> Left $ ParseError (position st) "mzero"
  mplus p1 p2 = Parser $ \st ->
    case evalParser p1 st of
         Right res  -> Right res
         Left _     -> evalParser p2 st

parse :: Parser a -> Text -> Either ParseError a
parse p t =
  fmap snd $ evalParser p ParserState{ subject = t, position = Position 1 1 }

failure :: ParserState -> String -> Either ParseError (ParserState, a)
failure st msg = Left $ ParseError (position st) msg

success :: ParserState -> a -> Either ParseError (ParserState, a)
success st x = Right (st, x)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser g
  where g st = case T.uncons (subject st) of
                    Just (c, _) | f c ->
                         success (advance st (T.singleton c)) c
                    _ -> failure st "satisfy"

peekChar :: Parser (Maybe Char)
peekChar = Parser $ \st ->
             case T.uncons (subject st) of
                  Just (c, _) -> success st (Just c)
                  Nothing     -> success st Nothing

-- not efficient as in attoparsec
inClass :: String -> Char -> Bool
inClass s c = c `elem` s

notInClass :: String -> Char -> Bool
notInClass s = not . inClass s

endOfInput :: Parser ()
endOfInput = Parser $ \st ->
  if T.null (subject st)
     then success st ()
     else failure st "endOfInput"

char :: Char -> Parser Char
char c = satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy (const True)

getPosition :: Parser Position
getPosition = Parser $ \st -> success st (position st)

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile f = Parser $ \st ->
  let t = T.takeWhile f (subject st) in
  success (advance st t) t

takeTill :: (Char -> Bool) -> Parser Text
takeTill f = takeWhile (not . f)

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 f = Parser $ \st ->
  case T.takeWhile f (subject st) of
       t | T.null t  -> failure st "takeWhile1"
         | otherwise -> success (advance st t) t

takeText :: Parser Text
takeText = Parser $ \st ->
  let t = subject st in
  success (advance st t) t

skip :: (Char -> Bool) -> Parser ()
skip f = Parser $ \st ->
  case T.uncons (subject st) of
       Just (c,_) | f c -> success (advance st (T.singleton c)) ()
       _                -> failure st "skip"

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile f = Parser $ \st ->
  let t' = T.takeWhile f (subject st) in
  success (advance st t') ()

string :: Text -> Parser Text
string s = Parser $ \st ->
  if s `T.isPrefixOf` (subject st)
     then success (advance st s) s
     else failure st "string"

scan :: s -> (s -> Char -> Maybe s) -> Parser Text
scan s0 f = Parser $ go s0 []
  where go s cs st =
         case T.uncons (subject st) of
               Nothing        -> finish st cs
               Just (c, _)    -> case f s c of
                                  Just s' -> go s' (c:cs)
                                              (advance st (T.singleton c))
                                  Nothing -> finish st cs
        finish st cs =
            success st (T.pack (reverse cs))

lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \st ->
  case evalParser p st of
       Right (_,x) -> success st x
       Left _      -> failure st "lookAhead"

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \st ->
  case evalParser p st of
       Right (_,_) -> failure st "notFollowedBy"
       Left _      -> success st ()

-- combinators (definitions borrowed from attoparsec)

option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x

many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p)

manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = go
  where go = (end *> pure []) <|> liftA2 (:) p go

skipMany :: Alternative f => f a -> f ()
skipMany p = go
  where go = (p *> go) <|> pure ()

skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p

count :: Monad m => Int -> m a -> m [a]
count n p = sequence (replicate n p)


