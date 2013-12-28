{-# LANGUAGE OverloadedStrings #-}
module Cheapskate.Util (
    joinLines
  , tabFilter
  , isWhitespace
  , isEscapable
  )
  where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

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


