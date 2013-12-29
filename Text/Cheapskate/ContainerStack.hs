module Text.Cheapskate.ContainerStack (
    ContainerStack(..)
  , LineNumber
  , Elt(..)
  , Container(..)
  , ContainerType(..)
  , Leaf(..)
  , ContainerState(..)
  , ContainerM
  , getStack
  , updateStack
  , getBlanks
  , addBlank
  , clearBlanks
  , addLeaf
  , addContainer
  , closeStack
  , closeContainer
  , extractText
  ) where
import Data.Sequence (Seq, (|>), viewr, ViewR(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.RWS
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Foldable (toList)
import Text.Cheapskate.Types
import Text.Cheapskate.Util
import Text.Cheapskate.Inlines
import Text.ParserCombinators
import Control.Applicative

data ContainerStack =
  ContainerStack Container {- top -} [Container] {- rest -} deriving Show

type LineNumber   = Int

data Elt = C Container
         | L LineNumber Leaf
         deriving Show

data Container = Container{
                     containerType :: ContainerType
                   , children      :: Seq Elt
                   }

data ContainerType = Document
                   | BlockQuote
                   | ListItem { markerColumn :: Int
                              , padding      :: Int
                              , listType     :: ListType }
                   | FencedCode { startColumn :: Int
                                , fence :: Text
                                , info :: Text }
                   | IndentedCode
                   | RawHtmlBlock
                   | Reference
                   deriving (Eq, Show)

instance Show Container where
  show c = show (containerType c) ++ "\n" ++
    nest 2 (intercalate "\n" (map showElt $ toList $ children c))

nest :: Int -> String -> String
nest num = intercalate "\n" . map ((replicate num ' ') ++) . lines

showElt :: Elt -> String
showElt (C c) = show c
showElt (L _ (TextLine s)) = show s
showElt (L _ lf) = show lf

data Leaf = TextLine Text
          | BlankLine Text
          | ATXHeader Int Text
          | SetextHeader Int Text
          | Rule
          deriving (Show)

data ContainerState = ContainerState{
    stack   :: ContainerStack
  , blanks  :: [Text]
  } deriving Show

type ContainerM = RWS () ReferenceMap ContainerState

getStack :: ContainerM ContainerStack
getStack = gets stack

updateStack :: (ContainerStack -> ContainerStack) -> ContainerM ()
updateStack f = modify $ \st -> st{ stack = f (stack st) }

getBlanks :: ContainerM [Text]
getBlanks = reverse <$> gets blanks

addBlank :: Text -> ContainerM ()
addBlank t = modify $ \st -> st { blanks = t : blanks st }

clearBlanks :: ContainerM ()
clearBlanks = modify $ \st -> st { blanks = [] }

closeStack :: ContainerM Container
closeStack = do
  ContainerStack top rest  <- getStack
  if null rest
     then return top
     else closeContainer >> closeStack

closeContainer :: ContainerM ()
closeContainer = do
  ContainerStack top rest <- getStack
  case top of
       (Container Reference{} cs'') ->
         case parse pReference
               (T.strip $ joinLines $ map extractText $ toList cs'') of
              Right (lab, lnk, tit) -> do
                tell (M.singleton (normalizeReference lab) (lnk, tit))
                case rest of
                    (Container ct' cs' : rs) ->
                      updateStack $ \_ -> ContainerStack (Container ct' (cs' |> C top)) rs
                    [] -> return ()
              Left _ -> -- pass over in silence if ref doesn't parse?
                        case rest of
                             (c:cs) -> updateStack $ \_ -> ContainerStack c cs
                             []     -> return ()
       (Container li@ListItem{} cs'') ->
         case rest of
              -- move final BlankLine outside of list item
              (Container ct' cs' : rs) ->
                       case viewr cs'' of
                            (zs :> b@(L _ BlankLine{})) ->
                              updateStack $ \_ -> ContainerStack
                                   (if Seq.null zs
                                       then Container ct' (cs' |> C (Container li zs))
                                       else Container ct' (cs' |>
                                               C (Container li zs) |> b)) rs
                            _ -> updateStack $ \_ -> ContainerStack (Container ct' (cs' |> C top)) rs
              [] -> return ()
       _ -> case rest of
             (Container ct' cs' : rs) ->
                 updateStack $ \_ -> ContainerStack (Container ct' (cs' |> C top)) rs
             [] -> return ()

addLeaf :: LineNumber -> Leaf -> ContainerM ()
addLeaf lineNum lf = do
  ContainerStack top rest <- getStack
  case (top, lf) of
        (Container ct@(ListItem{}) cs, BlankLine{}) ->
          case viewr cs of
            (_ :> L _ BlankLine{}) -> -- two blanks break out of list item:
                 closeContainer >> addLeaf lineNum lf
            _ -> updateStack $ \_ -> ContainerStack (Container ct (cs |> L lineNum lf)) rest
        (Container ct cs, _) ->
                 updateStack $ \_ -> ContainerStack (Container ct (cs |> L lineNum lf)) rest

addContainer :: ContainerType -> ContainerM ()
addContainer ct = updateStack $ \(ContainerStack top rest) ->
  ContainerStack (Container ct mempty) (top:rest)

extractText :: Elt -> Text
extractText (L _ (TextLine t)) = t
extractText _ = mempty
