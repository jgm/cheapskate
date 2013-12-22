module Main where

import Cheapskate
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlToByteStringIO)
import Text.Blaze.Html
import Data.Monoid ((<>))
import System.Environment
import Data.List (partition)
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Cheapskate.Parse (processLines) -- TODO for now

convert :: [String] -> Text -> IO ()
convert opts = render . parseMarkdown
    where render x = if "-n" `elem` opts
                        then print x
                        else do
                          renderHtmlToByteStringIO B.putStr
                             $ renderBlocks x <> toHtml "\n"

-- main loop

main :: IO ()
main = do
  args <- getArgs
  let isOpt ('-':_) = True
      isOpt _       = False
  let (opts, files) = partition isOpt args
  case files of
       [] -> T.getContents >>= print . fst . processLines
       _  -> mapM T.readFile files >>= print . fst . processLines . T.unlines
{-
  case files of
       [] -> T.getContents >>= convert opts
       _  -> mapM T.readFile files >>= convert opts . T.unlines
-}
