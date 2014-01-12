{
{-# OPTIONS_GHC -w #-}
module Cheapskate.Lex (alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9                    -- digits
$hexdigit = [0-9A-Fa-f]
$alpha = [a-zA-Z]               -- alphabetic characters
$alphanum = [0-9a-zA-Z]
$punct = [\[ \] \( \) \{ \} \_ \# \& \^ \! \* \+ \= \' \` \| \~ \. \; \? \! \: \, \" \/ \\ \-]
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
@scheme = coap|doi|javascript|aaa|aaas|about|acap|cap|cid|crid|data|dav|dict|dns|file|ftp|geo|go|gopher|h323|http|https|iax|icap|im|imap|info|ipp|iris|iris\.beep|iris\.xpc|iris\.xpcs|iris\.lwz|ldap|mailto|mid|msrp|msrps|mtqp|mupdate|news|nfs|ni|nih|nntp|opaquelocktoken|pop|pres|rtsp|service|session|shttp|sieve|sip|sips|sms|snmp|soap\.beep|soap\.beeps|tag|tel|telnet|tftp|thismessage|tn3270|tip|tv|urn|vemmi|ws|wss|xcon|xcon\-userid|xmlrpc\.beep|xmlrpc\.beeps|xmpp|z39\.50r|z39\.50s|adiumxtra|afp|afs|aim|apt|attachment|aw|beshare|bitcoin|bolo|callto|chrome|chrome\-extension|com\-eventbrite\-attendee|content|cvs|dlna\-playsingle|dlna\-playcontainer|dtn|dvb|ed2k|facetime|feed|finger|fish|gg|git|gizmoproject|gtalk|hcp|icon|ipn|irc|irc6|ircs|itms|jar|jms|keyparc|lastfm|ldaps|magnet|maps|market|message|mms|ms\-help|msnim|mumble|mvn|notes|oid|palm|paparazzi|platform|proxy|psyc|query|res|resource|rmi|rsync|rtmp|secondlife|sftp|sgn|skype|smb|soldat|spotify|ssh|steam|svn|teamspeak|things|udp|unreal|ut2004|ventrilo|view\-source|webcal|wtai|wyciwyg|xfire|xri|ymsgr|COAP|DOI|JAVASCRIPT|AAA|AAAS|ABOUT|ACAP|CAP|CID|CRID|DATA|DAV|DICT|DNS|FILE|FTP|GEO|GO|GOPHER|H323|HTTP|HTTPS|IAX|ICAP|IM|IMAP|INFO|IPP|IRIS|IRIS\.BEEP|IRIS\.XPC|IRIS\.XPCS|IRIS\.LWZ|LDAP|MAILTO|MID|MSRP|MSRPS|MTQP|MUPDATE|NEWS|NFS|NI|NIH|NNTP|OPAQUELOCKTOKEN|POP|PRES|RTSP|SERVICE|SESSION|SHTTP|SIEVE|SIP|SIPS|SMS|SNMP|SOAP\.BEEP|SOAP\.BEEPS|TAG|TEL|TELNET|TFTP|THISMESSAGE|TN3270|TIP|TV|URN|VEMMI|WS|WSS|XCON|XCON\-USERID|XMLRPC\.BEEP|XMLRPC\.BEEPS|XMPP|Z39\.50R|Z39\.50S|ADIUMXTRA|AFP|AFS|AIM|APT|ATTACHMENT|AW|BESHARE|BITCOIN|BOLO|CALLTO|CHROME|CHROME\-EXTENSION|COM\-EVENTBRITE\-ATTENDEE|CONTENT|CVS|DLNA\-PLAYSINGLE|DLNA\-PLAYCONTAINER|DTN|DVB|ED2K|FACETIME|FEED|FINGER|FISH|GG|GIT|GIZMOPROJECT|GTALK|HCP|ICON|IPN|IRC|IRC6|IRCS|ITMS|JAR|JMS|KEYPARC|LASTFM|LDAPS|MAGNET|MAPS|MARKET|MESSAGE|MMS|MS\-HELP|MSNIM|MUMBLE|MVN|NOTES|OID|PALM|PAPARAZZI|PLATFORM|PROXY|PSYC|QUERY|RES|RESOURCE|RMI|RSYNC|RTMP|SECONDLIFE|SFTP|SGN|SKYPE|SMB|SOLDAT|SPOTIFY|SSH|STEAM|SVN|TEAMSPEAK|THINGS|UDP|UNREAL|UT2004|VENTRILO|VIEW\-SOURCE|WEBCAL|WTAI|WYCIWYG|XFIRE|XRI|YMSGR

tokens :-

  $white+                               { \s -> if hasLineBreak s
                                                   then LINEBREAK s
                                                   else SPACES s }
  @backtickspan / { checkBackticks }    { CODESPAN . stripBackticks }
  $alphanum+                            { CHARS }
  "\" .                                 { CHARS . drop 1 }
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
  "<" @scheme ":" [^>]+ ">"             { AUTOLINKURL . stripDelims }
  "<" ~$white+ "@" ~$white+ ">"         { AUTOLINKEMAIL . stripDelims }
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
  | AUTOLINKURL String
  | AUTOLINKEMAIL String
  | CHARS String
  | ENTITY String
  | HTML String
  | URI String
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

stripDelims :: String -> String
stripDelims = reverse . drop 1 . reverse . drop 1

-- contains two spaces followed by a newline
hasLineBreak :: String -> Bool
hasLineBreak s = case break (=='\n') s of
                      (' ':' ':_, '\n':_) -> True
                      _ -> False
}
