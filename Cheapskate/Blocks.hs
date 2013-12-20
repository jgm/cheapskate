{-# LANGUAGE OverloadedStrings #-}
module Cheapskate.Blocks (Container(..), parseContainers) where
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

newtype ContainerStack = ContainerStack [Container]
        deriving (Show)

data Container = Document (Seq Container)
               | Paragraph (Seq Text)
               deriving (Show)

parseContainers :: Text -> ContainerStack
parseContainers =
  foldl' processLine (ContainerStack [Document mempty]) . map tabFilter . T.lines

processLine :: ContainerStack -> Text -> ContainerStack
processLine (ContainerStack (Paragraph ls : cs)) t =
  ContainerStack (Paragraph (ls |> t) : cs)
processLine (ContainerStack cs) t =
  ContainerStack (Paragraph (Seq.singleton t) : cs)

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

