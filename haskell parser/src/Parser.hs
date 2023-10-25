-- | The simple Parser types.
module Parser where

import Control.Applicative hiding (some,many)
import Control.Monad hiding (fail)
import Data.Char
import Prelude hiding (fail)

-- | The input to a Parser.
type Input = String

-- | The result of a parse
data ParseResult a = Result Input a -- | The result of a successful parse.
  | Error ParseError -- | The result of a failed parse.

instance Functor ParseResult where
  fmap f (Result i a) = Result i $ f a
  fmap _ (Error e)    = Error e

instance Show a => Show (ParseResult a) where
  show (Result i a) = "Result >" ++ i ++ "< " ++ show a
  show (Error e)    = show e

instance Eq a => Eq (ParseResult a) where
  (Result il al) == (Result ir ar) = il == ir && al == ar
  (Error el)     == (Error er)     = el == er
  _              == _              = False

data ParseError = UnexpectedChar Char
  | ExpectedEof Input
  | UnexpectedEof
  deriving (Eq,Show)

-- | The Parser type.
newtype Parser a = P {
  parse :: Input -> ParseResult a -- | Runs the Parser on the input.
  }

instance Functor Parser where
  fmap f (P p) = P $ fmap f . p

instance Applicative Parser where
  pure = P . flip Result
  ff <*> fa = ff >>= (<$> fa)

instance Monad Parser where
  fa >>= f = P (\i ->
    case parse fa i of
      Result i' a -> parse (f a) i'
      Error e     -> Error e)

fail :: ParseError -> Parser a
fail = P . const . Error

get :: Parser Char
get = P go
  where
    go (c:i) = Result i c
    go []    = Error UnexpectedEof

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- get
  if f c then pure c else fail (UnexpectedChar c)

(|||) :: Parser a -> Parser a -> Parser a
lfa ||| rfa = P (\i ->
  case parse lfa i of
    Error _ -> parse rfa i
    r       -> r)

list :: Parser a -> Parser [a]
list fa = list1 fa ||| pure []

list1 :: Parser a -> Parser [a]
list1 fa = liftA2 (:) fa (list fa)

munch1 :: (Char -> Bool) -> Parser String
munch1 = list1 . satisfy

is :: Char -> Parser Char
is = satisfy . (==)

string :: String -> Parser String
string = traverse is

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = list space


between :: Parser open -> Parser close -> Parser a -> Parser a
between fo fc fa = fo *> fa <* fc

parserToReadS :: Parser a -> ReadS a
parserToReadS = (go .) . parse
  where
    go (Result i a) = [(a,i)]
    go (Error _)    = []

oneof :: String -> Parser Char
oneof = satisfy . flip elem

noneof :: String -> Parser Char
noneof = satisfy . flip notElem
