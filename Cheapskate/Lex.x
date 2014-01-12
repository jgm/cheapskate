{
module Cheapskate.Lex (alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9                    -- digits
$hexdigit = [0-9A-Fa-f]
$alpha = [a-zA-Z]               -- alphabetic characters
$alphanum = [0-9a-zA-Z]
@backtickspan = ( `{1} [^`] ([^`]+ | `{2,} [^`]+)* `{1} |
                  `{2} [^`] ([^`]+ | `{3,} [^`]+ | `{1}   [^`]+)* `{2} |
                  `{3} [^`] ([^`]+ | `{4,} [^`]+ | `{1,2} [^`]+)* `{3} |
                  `{4} [^`] ([^`]+ | `{5,} [^`]+ | `{1,3} [^`]+)* `{4} |
                  `{5} [^`] ([^`]+ | `{6,} [^`]+ | `{1,4} [^`]+)* `{5} |
                  `{6} [^`] ([^`]+ | `{7,} [^`]+ | `{1,5} [^`]+)* `{6} )
@entity = "&" ("#" $digit+ | "#" [Xx] $hexdigit+ | $alpha+) ";"
@htmlcomment = "<!--" ([^>\-] | "-" [^\->]) ([^] | "-" [^\-])* "-->"
@singlequotedvalue = ['] [^']* [']
@doublequotedvalue = [\"] [^\"]* [\"]
@unquotedvalue = [^ \n>]+
@htmlattribute = $alphanum+ $white* "=" $white*
                (@singlequotedvalue | @doublequotedvalue | @unquotedvalue){0,1}
@htmltag = "<" "/"{0,1} $white* $alphanum+ ($white* @htmlattribute)*
                $white* "/"{0,1} ">"

tokens :-

  $white+                               { \s -> if hasLineBreak s
                                                   then LINEBREAK s
                                                   else SPACES s }
  @backtickspan / { checkBackticks }    { CODESPAN . stripBackticks }
  $alphanum+                            { CHARS }
  "\\" .                                { CHARS . drop 1 }
  "["                                   { const BRACKETL }
  "]"                                   { const BRACKETR }
  "("                                   { const PARENL }
  ")"                                   { const PARENR }
  "!"                                   { const BANG }
  "*"{1,3}                              { STARS . length }
  $white ^ "_"{1,3}                     { STARTUNDERSCORES . length }
  ^ "_"{1,3}                            { STARTUNDERSCORES . length }
  "_"{1,3} / { followedBySpace }        { ENDUNDERSCORES . length }
  @entity                               { ENTITY }
  @htmlcomment                          { HTML }
  @htmltag                              { HTML }
  .                                     { CHARS }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = SPACES String
  | LINEBREAK String
  | CODESPAN String
  | BRACKETL
  | BRACKETR
  | PARENL
  | PARENR
  | BANG
  | STARS Int
  | STARTUNDERSCORES Int
  | ENDUNDERSCORES Int
  | CHARS String
  | ENTITY String
  | HTML String
  deriving (Eq, Show)

-- check that the token is not preceded or followed by backtick character:
checkBackticks :: user -> AlexInput -> Int -> AlexInput -> Bool
checkBackticks _ before len after =
  not (alexInputPrevChar before == '`' ||
       fst `fmap` alexGetByte after == Just 96)

-- check that the token is followed by space or eof.
followedBySpace :: user -> AlexInput -> Int -> AlexInput -> Bool
followedBySpace _ _ len after =
  case alexGetByte after of
       Nothing    -> True
       Just (x,_) -> x == 32 || x == 10

-- strip surrounding backticks and optionally one whitespace
stripBackticks :: String -> String
stripBackticks = reverse . go . reverse . go
  where go = dropOneSpace . dropWhile (=='`')
        dropOneSpace (' ':xs) = xs
        dropOneSpace xs = xs

-- contains two spaces followed by a newline
hasLineBreak :: String -> Bool
hasLineBreak s = case break (=='\n') s of
                      (' ':' ':_, '\n':_) -> True
                      _ -> False
}
