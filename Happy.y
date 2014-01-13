{
module Main where
import Cheapskate.Types
import Cheapskate.Lex (alexScanTokens, Token(..))
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import Data.Monoid
}

%name inlines
%tokentype { Token }
%error { parseError }

%token
  sp             { SPACES $$ }
  hardbreak      { HARDBREAK }
  softbreak      { SOFTBREAK }
  code           { CODESPAN $$ }
  '['            { BRACKETL }
  ']'            { BRACKETR }
  '('            { PARENL }
  ')'            { PARENR }
  '!'            { BANG }
  stars          { STARS $$ }
  ulstart        { STARTUNDERSCORES $$ }
  ulend          { ENDUNDERSCORES $$ }
  autolinkurl    { AUTOLINKURL $$ }
  autolinkemail  { AUTOLINKEMAIL $$ }
  chars          { CHARS $$ }
  entity         { ENTITY $$ }
  html           { HTML $$ }

%%
Inls  :                   { Seq.empty }
      | Inls Inl          { $1 Seq.|> $2 }
Inl   : sp                { Space }
      | hardbreak         { LineBreak }
      | softbreak         { SoftBreak }
      | chars             { Str $ T.pack $1 }
      | autolinkurl       { let {t = T.pack $1} in
                            Link (Seq.singleton $ Str t) t mempty }
      | autolinkemail     { let {t = T.pack $1;
                                t' = T.pack "mailto:" <> t} in
                             Link (Seq.singleton $ Str t) t' mempty }
      | code              { Code $ T.pack $1 }
      | entity            { Entity $ T.pack $1 }
      | html              { RawHtml $ T.pack $1 }
      | stars             { Str $ T.pack "STARS" }
      | ulstart           { Str $ T.pack "ULSTART" }
      | ulend             { Str $ T.pack "ULEND" }
      | '['               { Str $ T.pack "[" }
      | ']'               { Str $ T.pack "]" }
      | '('               { Str $ T.pack "(" }
      | ')'               { Str $ T.pack ")" }
      | '!'               { Str $ T.pack "!" }
{

parseError :: [Token] -> a
parseError ts = error $ "Parse error at " ++ show (take 3 ts)

main = getContents >>= print . Seq.length . inlines . alexScanTokens
}
