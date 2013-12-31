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
                             $ renderBlocks def{ sanitize = "--sanitize" `elem` opts
                                               , allowRawHtml = "--escape-raw-html" `notElem` opts
                                               , preserveHardBreaks = "--hard-breaks" `elem` opts
                                               } x <> toHtml "\n"

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
