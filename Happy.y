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
  linebreak      { LINEBREAK }
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
Inls  :                   { mempty }
      | Inls Inl          { $1 Seq.|> $2 }
Inl   : sp                { Space }
      | linebreak         { LineBreak }
      | chars             { Str $ T.pack $1 }
      | autolinkurl       { \s -> let t = T.pack $1 in
                                Link (Seq.singleton $ Str t) t mempty }
      | autolinkemail     { \s -> let {t = T.pack $1;
                                       t' = T.pack "mailto:" <> t} in
                                Link (Seq.singleton $ Str t) t' mempty }
      | code              { Code $ T.pack $1 }
      | entity            { Entity $ T.pack $1 }
      | html              { Html $ T.pack $1 }
{

parseError :: [Token] -> a
parseError ts = error $ "Parse error at " ++ show (take 3 ts)

main = getContents >>= print . inlines . alexScanTokens
}
