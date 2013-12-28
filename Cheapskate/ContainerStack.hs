module Cheapskate.ContainerStack (
    ContainerStack(..)
  , LineNumber
  , Elt(..)
  , Container(..)
  , ContainerType(..)
  , Leaf(..)
  , ContainerM
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
import Cheapskate.Types
import Cheapskate.Util
import Cheapskate.Inlines
import ParserCombinators

data ContainerStack =
  ContainerStack Container {- top -} [Container] {- rest -}

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

type ContainerM = RWS () ReferenceMap ContainerStack

closeStack :: ContainerM Container
closeStack = do
  ContainerStack top rest  <- get
  if null rest
     then return top
     else closeContainer >> closeStack

closeContainer :: ContainerM ()
closeContainer = do
  ContainerStack top rest <- get
  case top of
       (Container Reference{} cs'') ->
         case parse pReference
               (T.strip $ joinLines $ map extractText $ toList cs'') of
              Right (lab, lnk, tit) -> do
                tell (M.singleton (normalizeReference lab) (lnk, tit))
                case rest of
                    (Container ct' cs' : rs) ->
                      put $ ContainerStack (Container ct' (cs' |> C top)) rs
                    [] -> return ()
              Left _ -> -- pass over in silence if ref doesn't parse?
                        case rest of
                             (c:cs) -> put $ ContainerStack c cs
                             []     -> return ()
       (Container li@ListItem{} cs'') ->
         case rest of
              -- move final BlankLine outside of list item
              (Container ct' cs' : rs) ->
                       case viewr cs'' of
                            (zs :> b@(L _ BlankLine{})) ->
                              put $ ContainerStack
                                   (if Seq.null zs
                                       then Container ct' (cs' |> C (Container li zs))
                                       else Container ct' (cs' |>
                                               C (Container li zs) |> b)) rs
                            _ -> put $ ContainerStack (Container ct' (cs' |> C top)) rs
              [] -> return ()
       _ -> case rest of
             (Container ct' cs' : rs) ->
                 put $ ContainerStack (Container ct' (cs' |> C top)) rs
             [] -> return ()

addLeaf :: LineNumber -> Leaf -> ContainerM ()
addLeaf lineNum lf = do
  ContainerStack top rest <- get
  case (top, lf) of
        (Container ct@(ListItem{}) cs, BlankLine{}) ->
          case viewr cs of
            (_ :> L _ BlankLine{}) -> -- two blanks break out of list item:
                 closeContainer >> addLeaf lineNum lf
            _ -> put $ ContainerStack (Container ct (cs |> L lineNum lf)) rest
        (Container ct cs, _) ->
                 put $ ContainerStack (Container ct (cs |> L lineNum lf)) rest

addContainer :: ContainerType -> ContainerM ()
addContainer ct = modify $ \(ContainerStack top rest) ->
  ContainerStack (Container ct mempty) (top:rest)

extractText :: Elt -> Text
extractText (L _ (TextLine t)) = t
extractText _ = mempty
