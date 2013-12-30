{-# LANGUAGE OverloadedStrings #-}
module Text.Cheapskate.Render (render) where
import Text.Cheapskate.Types
import Data.Text (Text)
import Text.HTML.TagSoup
import Data.Monoid
import Data.Foldable (foldMap, toList)
import qualified Data.Text as T
import Data.List (intersperse)
import Data.Sequence (Seq, (|>), (<|), singleton)
import qualified Data.Sequence as Seq

render :: Blocks -> Text
render = renderTagsOptions renderOptions{ optMinimize = minimize } . toList . renderBlocks
  where minimize "br" = True
        minimize "hr" = True
        minimize _    = False

-- Render a sequence of blocks as HTML5.  Currently a single
-- newline is used between blocks, an a newline is used as a
-- separator e.g. for list items. These can be changed by adjusting
-- nl and blocksep.  Eventually we probably want these as parameters
-- or options.
renderBlocks :: Blocks -> Html
renderBlocks = mconcat . intersperse blocksep . map renderBlock . toList
  where renderBlock :: Block -> Html
        renderBlock (Header n ils)
          | n >= 1 && n <= 6 =
            inTag (T.pack $ "h" ++ show n) [] $ renderInlines ils
          | otherwise        =
            inTag "p" [] $ renderInlines ils
        renderBlock (Para ils) = inTag "p" [] $ renderInlines ils
        renderBlock (HRule) = inTag "hr" [] mempty
        renderBlock (Blockquote bs) =
          inTag "blockquote" [] $ nl <> renderBlocks bs <> nl
        renderBlock (CodeBlock attr t) =
          inTag "pre" attr' $ inTag "code" [] $ toHtml (t <> "\n")
          -- add newline because Markdown.pl does
           where attr' = case codeLang attr of
                              Nothing   -> []
                              Just lang -> [("class", lang)]
        renderBlock (List tight (Bullet _) items) =
          inTag "ul" [] $ nl <> mconcat (map (li tight) items)
        renderBlock (List tight (Numbered _ n) items) =
          inTag "ol" attr' $ nl <> mconcat (map (li tight) items)
            where attr' = if n == 1 then [] else [("start", T.pack $ show n)]
        renderBlock (HtmlBlock raw) = rawHtml raw
        li :: Bool -> Blocks -> Html  -- tight list handling
        li True = (<> nl) .
          (inTag "li" [] . mconcat . intersperse blocksep .
           map renderBlockTight . toList)
        li False = toLi
        renderBlockTight (Para zs) = renderInlines zs
        renderBlockTight x         = renderBlock x
        toLi x = inTag "li" [] (renderBlocks x) <> nl
        nl = toHtml "\n"
        blocksep = toHtml "\n"

-- Render a sequence of inlines as HTML5.
renderInlines :: Inlines -> Html
renderInlines = foldMap renderInline
  where renderInline :: Inline -> Html
        renderInline (Str t) = toHtml t
        renderInline Space   = toHtml " "
        renderInline SoftBreak = toHtml "\n"
          -- this preserves the line breaks in the
          -- markdown document; replace with " " if this isn't wanted.
        renderInline LineBreak = inTag "br" [] mempty <> toHtml "\n"
        renderInline (Emph ils) = inTag "em" [] $ renderInlines ils
        renderInline (Strong ils) = inTag "strong" [] $ renderInlines ils
        renderInline (Code t) = inTag "code" [] $ toHtml t
        renderInline (Link ils url tit) =
          inTag "a" attr' $ renderInlines ils
           where attr' = ("href", gentleEscape url) :
                          [("title", gentleEscape tit) | not $ T.null tit]
        renderInline (Image ils url tit) =
          inTag "img" ([("src", gentleEscape url),
                        ("alt", foldMap fromTagText (renderInlines ils))] ++
                        [("title", gentleEscape tit) | not $ T.null tit])
           mempty
        renderInline (Entity t) = rawHtml t
        renderInline (RawHtml t) = rawHtml t
        renderInline (Markdown t) = rawHtml t -- shouldn't happen

{-
toValue' :: Text -> AttributeValue
toValue' = preEscapedToValue . gentleEscape . T.unpack

-- preserve existing entities
gentleEscape :: String -> String
gentleEscape [] = []
gentleEscape ('"':xs) = "&quot;" ++ gentleEscape xs
gentleEscape ('\'':xs) = "&#39;" ++ gentleEscape xs
gentleEscape ('&':'#':x:xs)
  | x == 'x' || x == 'X' =
  case span isHexDigit xs of
       (ys,';':zs) | not (null ys) && length ys < 6 ->
         '&':'#':x:ys ++ ";" ++ gentleEscape zs
       _ -> "&amp;#" ++ (x : gentleEscape xs)
gentleEscape ('&':'#':xs) =
  case span isDigit xs of
       (ys,';':zs) | not (null ys) && length ys < 6 ->
         '&':'#':ys ++ ";" ++ gentleEscape zs
       _ -> "&amp;#" ++ gentleEscape xs
gentleEscape ('&':xs) =
  case span isAlphaNum xs of
       (ys,';':zs) | not (null ys) && length ys < 11 ->
         '&':ys ++ ";" ++ gentleEscape zs
       _ -> "&amp;" ++ gentleEscape xs
gentleEscape (x:xs) = x : gentleEscape xs
-}

gentleEscape :: Text -> Text
gentleEscape = foldMap fromTagText . parseTags

type Html = Seq (Tag Text)

inTag :: Text -> [Attribute Text] -> Html -> Html
inTag tag attr contents =
  (TagOpen tag attr <| contents) |> TagClose tag

rawHtml :: Text -> Html
rawHtml = Seq.fromList . parseTags

toHtml :: Text -> Html
toHtml = singleton . TagText
