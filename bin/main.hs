module Main where

import Cheapskate
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlToByteStringIO)
import Text.Blaze.Html
import System.Environment
import Data.List (partition)
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Text.IO as T
import qualified Data.Text as T

convert :: [String] -> Text -> IO ()
convert opts t = renderHtmlToByteStringIO B.putStr $ toHtml $
  markdown def{ sanitize = "--sanitize" `elem` opts
                    , allowRawHtml = "--escape-raw-html" `notElem` opts
                    , preserveHardBreaks = "--hard-breaks" `elem` opts
                    , debug = "--debug" `elem` opts
                    } t

main :: IO ()
main = do
  args <- getArgs
  let isOpt ('-':_) = True
      isOpt _       = False
  let (opts, files) = partition isOpt args
  let handle = convert opts
  case files of
       [] -> T.getContents >>= handle
       _  -> mapM T.readFile files >>= handle . T.unlines
