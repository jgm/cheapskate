module Main where

import Text.Cheapskate
import System.Environment
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Text.Cheapskate.Parse (processLines) -- TODO for now

convert :: [String] -> Text -> IO ()
convert _opts = T.putStrLn . render . parseMarkdown

-- main loop

main :: IO ()
main = do
  args <- getArgs
  let isOpt ('-':_) = True
      isOpt _       = False
  let (opts, files) = partition isOpt args
  let handle = if "--debug" `elem` opts
      then print . fst . processLines
      else convert opts
  case files of
       [] -> T.getContents >>= handle
       _  -> mapM T.readFile files >>= handle . T.unlines
