module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Part 1
-}

-- | Exercise 1
-- | BNF grammar for lambda calculus expression

-- <lambdaP> ::= <longLambdaP> | <shortLambdaP> 
-- <longLambdaP> ::= <builderLong> 
-- <shortLambdaP> ::= <builderShort>
-- <builderLong> ::= <open> <lambda> <var> <dot> <bodyLong> <close>
-- <bodyLong> ::= <builderLong> | <parentTermMid> | <parentTermFront> | <parentTerm> | <parentTermBack> | <terms> 
-- <builderShort> ::= <parentShortTwo> | <parentShortFront> | <bodyShort>
-- <bodyShort> ::= <lambda> <var>+ <dot> <shortTerms>
-- <shortTerms> ::= <parentTermShortBack> | <parentTermMid> | <parentTermFront> | <parentTerm> | <parentTermBack> | <terms> 
-- <parentShortTwo> ::= <open> <bodyShort> <close> <open> <bodyShort> <close>
-- <parentShortFront> ::= <open> <bodyShort> <close> <bodyShort>
-- <parentTermShortBack> ::= <terms> <open> <bodyShort> <close>
-- <parentTermMid> ::= <terms> <open> <terms> <close> <terms>
-- <parentTermFront> ::= <open> <terms> <close> <terms> 
-- <parentTerm> ::= <open> <terms> <close> 
-- <parentTermBack> ::= <terms> <open> <terms> <close> 
-- <terms> ::= <var>+
-- <var> ::= [a-z]
-- <lambda> ::= "λ"
-- <dot> ::= "."
-- <open> ::= "("
-- <close> ::= ")"


-- | Exercise 2

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('

longLambdaP :: Parser Lambda
longLambdaP = build <$> builderLong

-- | Exercise 3

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

shortLambdaP :: Parser Lambda
shortLambdaP = build <$> builderShort

-- | Parses a string representing a lambda calculus expression in short or long form
--
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'

lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP


-- | Additional functions for Part 1

-- | Parses a lambda character ('λ')
--
-- >>> parse lambda "λ"
-- Result >< '\955'
--
-- >>> parse lambda "λx.xx"
-- Result >x.xx< '\955'
--
-- >>> parse lambda "x.xx"
-- UnexpectedChar 'x'

lambda :: Parser Char
lambda = is 'λ'

-- | Parses a open bracket character ('(')
--
-- >>> parse open "("
-- Result >< '('
--
-- >>> parse open "(abc"
-- Result >abc< '('
--
-- >>> parse open "abc"
-- UnexpectedChar 'a'

open :: Parser Char
open = is '('

-- | Parses a close bracket character (')')
--
-- >>> parse close ")"
-- Result >< ')'
--
-- >>> parse close ")abc"
-- Result >abc< ')'
--
-- >>> parse close "abc"
-- UnexpectedChar 'a'

close :: Parser Char
close = is ')'

-- | Parses a dot character ('.')
--
-- >>> parse dot "."
-- Result >< '.'
--
-- >>> parse dot ".abc"
-- Result >abc< '.'
--
-- >>> parse dot "abc"
-- UnexpectedChar 'a'

dot :: Parser Char
dot = is '.'

-- | Parses an alphabet character ([a-z]) representing a variable
--
-- >>> parse var "a"
-- Result >< 'a'
--
-- >>> parse var "abc"
-- Result >bc< 'a'
--
-- >>> parse (list var) "abc123"
-- Result >123< "abc"
--
-- >>> parse var "123"
-- UnexpectedChar '1'

var :: Parser Char
var = oneof "abcdefghijklmnopqrstuvwxyz"

-- | Parses a string representing terms of a lambda calculus

terms :: Parser Builder
terms = do
    v <- var
    vs <- list var
    pure $ foldl ap (term v) $ term <$> vs

-- | Parses a string representing terms of a lambda calculus within a bracket

parentTerm :: Parser Builder
parentTerm = do
    open
    v <- var
    vs <- list var
    close
    pure $ foldl ap (term v) $ term <$> vs

-- | Parses a string representing terms of a lambda calculus within a bracket followed by regular terms

parentTermFront :: Parser Builder
parentTermFront = do
    inside <- parentTerm
    outside <- list var
    pure $ foldl ap inside $ term <$> outside

-- | Parses a string representing terms of a lambda calculus followed by terms within a bracket

parentTermBack :: Parser Builder
parentTermBack = do
    outside <- terms
    inside <- parentTerm
    pure $ ap outside inside

-- | Parses a string representing terms of a lambda calculus with terms within a bracket in the middle

parentTermMid :: Parser Builder
parentTermMid = do
    inside <- parentTermBack
    outside <- terms
    pure $ ap inside outside

-- | The possible bodies of the lambda expression in long form

bodyLong :: Parser Builder
bodyLong = builderLong ||| parentTermMid ||| parentTermFront ||| parentTerm ||| parentTermBack ||| terms 

-- | Parses a string representing a long lambda expression into a builder
--
-- >>> parse builderLong "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse builderLong "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse builderLong "(λx(λy.x))"
-- UnexpectedChar '('

builderLong :: Parser Builder
builderLong = do
    open
    lambda
    v <- var
    dot
    e <- bodyLong
    close
    pure $ lam v e

-- | Parses a string representing a short lambda expression into a builder
--
-- >>> parse builderShort "λx.xx"
-- Result >< \x.xx
--
-- >>> parse builderShort "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse builderShort "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse builderShort "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse builderShort "λxyz"
-- UnexpectedEof

builderShort :: Parser Builder
builderShort = parentShortTwo ||| parentShortFront ||| bodyShort

-- | Parses a string representing two short lambda expressions first within a bracket into a builder
--
-- >>> parse parentShortFront "(λx.x)λy.yy"
-- Result >< (\x.x)\y.yy

parentShortFront :: Parser Builder
parentShortFront = do
    open
    inside <- bodyShort
    close
    outside <- bodyShort
    pure $ inside `ap` outside

-- | Parses a string representing two short lambda expressions within brackets into a builder
--
-- >>> parse parentShortTwo "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy

parentShortTwo :: Parser Builder
parentShortTwo = do
    open
    one <- bodyShort
    close
    open
    two <- bodyShort
    close
    pure $ one `ap` two

-- | Parses a string representing two short lambda expressions second within a bracket into a builder

parentTermShortBack :: Parser Builder
parentTermShortBack = do
    vs <- terms
    open
    b <- bodyShort
    close
    pure $ ap vs b

-- | Parses a string representing the terms of a short lambda expression

shortTerms :: Parser Builder
shortTerms = parentTermShortBack ||| parentTermMid ||| parentTermFront ||| parentTerm ||| parentTermBack ||| terms 

-- | Parses a string representing a simple short lambda expression into a builder
--
-- >>> parse bodyShort "λx.xx"
-- Result >< \x.xx

bodyShort :: Parser Builder
bodyShort = do
    lambda
    vs <- list var
    dot
    t <- shortTerms
    pure $ foldr ($) t $ lam <$> vs


{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
--
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
--
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

logicP :: Parser Lambda
logicP = build <$> logicExpr


-- | Additional functions for Part 2 Exercise 1

-- | Adapted from Parser Combinators https://tgdwyer.github.io/parsercombinators/
-- Chain function to combine parsers by applying them to each other

chain :: Parser Builder -> Parser Builder -> Parser Builder
chain p op = p >>= rest
   where
   rest a = (do
               f <- op
               b <- p
               rest (f `ap` a `ap` b)
            ) ||| pure a

-- | Parser for logic expressions that can either be a single term or more
--
-- >>> parse logicExpr "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicExpr "True and False or not False and True"
-- Result >< (\xy.(\btf.btf)xy\_f.f)((\xy.(\btf.btf)xy\_f.f)(\t_.t)((\xy.(\btf.btf)x(\t_.t)y)(\_f.f)((\x.(\btf.btf)x(\_f.f)\t_.t)(\_f.f))))\t_.t
--
-- >>> parse logicExpr "not not not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)((\x.(\btf.btf)x(\_f.f)\t_.t)((\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f))
--
-- >>> parse logicExpr "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f

logicExpr :: Parser Builder
logicExpr = boolExpr ||| single ||| ifP

-- | Parser for logic expression combining all or terms with "and"
--
-- >>> parse boolExpr "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse boolExpr "True and False or not False and True"
-- Result >< (\xy.(\btf.btf)xy\_f.f)((\xy.(\btf.btf)xy\_f.f)(\t_.t)((\xy.(\btf.btf)x(\t_.t)y)(\_f.f)((\x.(\btf.btf)x(\_f.f)\t_.t)(\_f.f))))\t_.t

boolExpr :: Parser Builder
boolExpr = chain orTerms andP

-- | Parser for logic expression combining all terms with "or"

orTerms :: Parser Builder
orTerms = chain single orP

-- | Parser for a single logic expression term

single :: Parser Builder
single = preBool ||| boolP ||| complexExpr ||| bracketComplex

-- | Parser for boolean terms with prefix
--
-- >>> parse preBool "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
--
-- >>> parse preBool "not not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)((\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f)

preBool :: Parser Builder
preBool = do
    p <- notP
    b <- single
    pure $ ap p b

-- | Parser for boolean values 
--
-- >>> parse boolP "True"
-- Result >< \t_.t
--
-- >>> parse boolP "False"
-- Result >< \_f.f
--
-- >>> parse boolP "   True"
-- Result >< \t_.t
--
-- >>> parse boolP "  False"
-- Result >< \_f.f

boolP :: Parser Builder
boolP = trueP ||| falseP

-- | Parser for boolean value of "True"
--
-- >>> parse trueP "True"
-- Result >< \t_.t
--
-- >>> parse trueP "   True"
-- Result >< \t_.t

trueP :: Parser Builder
trueP = do
    spaces
    string "True" 
    pure $ boolToLam True

-- | Parser for boolean value of "False"
--
-- >>> parse falseP "False"
-- Result >< \_f.f
--
-- >>> parse falseP "  False"
-- Result >< \_f.f

falseP :: Parser Builder
falseP = do
    spaces
    string "False"
    pure $ boolToLam False

-- | Parser for boolean combinator of "and"
--
-- >>> parse andP "and"
-- Result >< \xy.(\btf.btf)xy\_f.f
--
-- >>> parse andP "   and"
-- Result >< \xy.(\btf.btf)xy\_f.f

andP :: Parser Builder
andP = do
    spaces
    string "and"
    pure $ andBuilder

-- | Builder for boolean combinator of "and"

andBuilder :: Builder
andBuilder = 'x' `lam` 'y' `lam` ifBuilder `ap` term 'x' `ap` term 'y' `ap` (boolToLam False)

-- | Parser for boolean combinator of "or"
--
-- >>> parse orP "or"
-- Result >< \xy.(\btf.btf)x(\t_.t)y
--
-- >>> parse orP "   or"
-- Result >< \xy.(\btf.btf)x(\t_.t)y

orP :: Parser Builder
orP = do
    spaces
    string "or"
    pure $ orBuilder

-- | Builder for boolean combinator of "and"

orBuilder :: Builder
orBuilder = 'x' `lam` 'y' `lam` ifBuilder `ap` term 'x' `ap` (boolToLam True) `ap` term 'y'

-- | Parser for boolean prefix of "not"
--
-- >>> parse notP "not"
-- Result >< \x.(\btf.btf)x(\_f.f)\t_.t
--
-- >>> parse notP "   not"
-- Result >< \x.(\btf.btf)x(\_f.f)\t_.t

notP :: Parser Builder
notP = do
    spaces
    string "not"
    pure $ notBuilder

-- | Builder for boolean prefix of "not"

notBuilder :: Builder
notBuilder = 'x' `lam` ifBuilder `ap` term 'x' `ap` (boolToLam False) `ap` (boolToLam True)

-- | Parser for boolean in if statements
--
-- >>> parse ifP "if True then True else False"
-- Result >< (\btf.btf)(\t_.t)(\t_.t)\_f.f
--
-- >>> parse ifP "if True and not False then True or True else False"
-- Result >< (\btf.btf)((\xy.(\btf.btf)xy\_f.f)(\t_.t)((\x.(\btf.btf)x(\_f.f)\t_.t)(\_f.f)))((\xy.(\btf.btf)x(\t_.t)y)(\t_.t)(\t_.t))\_f.f

ifP :: Parser Builder
ifP = do
    spaces
    string "if"
    spaces
    condition <- logicExpr
    spaces
    string "then"
    spaces
    thenV <- logicExpr
    spaces 
    string "else"
    spaces
    elseV <- logicExpr
    pure $ ifBuilder `ap` condition `ap` thenV `ap` elseV
    
-- | Builder for boolean prefix of "if"

ifBuilder :: Builder
ifBuilder = 'b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f'


-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ x
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13
basicArithmeticP :: Parser Lambda
basicArithmeticP = build <$> basicArithmeticExpr

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68
arithmeticP :: Parser Lambda
arithmeticP = build <$> arithmeticExpr


-- | Additional functions for Part 2 Exercise 2

-- | Builder for successor lambda expression

succBuilder :: Builder
succBuilder = 'n' `lam` 'f' `lam` 'x' `lam` term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x')

-- | Builder for predecessor lambda expression

predBuilder :: Builder
predBuilder = 'n' `lam` 'f' `lam` 'x' `lam` term 'n' `ap` ('g' `lam` 'h' `lam` term 'h' `ap` (term 'g' `ap` term 'f')) `ap` ('u' `lam` term 'x') `ap` ('u' `lam` term 'u')

-- | Builder for addition lambda expression

addBuilder :: Builder
addBuilder = 'x' `lam` 'y' `lam` term 'y' `ap` succBuilder `ap` term 'x'

-- | Builder for subtraction lambda expression

minusBuilder :: Builder
minusBuilder = 'x' `lam` 'y' `lam` term 'y' `ap` predBuilder `ap` term 'x'

-- | Builder for multiplication lambda expression

multiplyBuilder :: Builder
multiplyBuilder = 'x' `lam` 'y' `lam` 'f' `lam` term 'x' `ap` (term 'y' `ap` term 'f')

-- | Builder for exponential lambda expression

expBuilder :: Builder
expBuilder = 'x' `lam` 'y' `lam` term 'y' `ap` term 'x'

-- | Parser for basic arithmetic using the chain function
--
-- >>> lamToInt <$> (build <$> parse basicArithmeticExpr "5")
-- Result >< Just 5
-- >>> lamToInt <$> (build <$> parse basicArithmeticExpr "1+5")
-- Result >< Just 6
-- >>> lamToInt <$> (build <$> parse basicArithmeticExpr "1 + 5 - 3 + 5")
-- Result >< Just 8

basicArithmeticExpr :: Parser Builder
basicArithmeticExpr = chain intP basicOperator

-- | Parser for an arithmetic expression
--
-- >>> lamToInt <$> (build <$> parse arithmeticExpr "5")
-- Result >< Just 5
-- >>> lamToInt <$> (build <$> parse arithmeticExpr "(1+5)")
-- Result >< Just 6
-- >>> lamToInt <$> (build <$> parse arithmeticExpr "(1+5) * 2**3")
-- Result >< Just 48
-- >>> lamToInt <$> (build <$> parse arithmeticExpr "(1+5) + 2 * 3 ** 2")
-- Result >< Just 24

arithmeticExpr :: Parser Builder
arithmeticExpr = chain multiplyTerm basicOperator

-- | Parser for expressions in a bracket
--
-- >>> lamToInt <$> (build <$> parse bracketTerm "(2**3 * 2)")
-- Result >< Just 16
-- >>> lamToInt <$> (build <$> parse bracketTerm "((1+2)*(1+1))")
-- Result >< Just 6

bracketTerm :: Parser Builder
bracketTerm = do
    open
    spaces
    x <- arithmeticExpr
    spaces
    close
    pure $ x

-- | Parser for multiplication terms
--
-- >>> lamToInt <$> (build <$> parse multiplyTerm "2**3 * 2")
-- Result >< Just 16
-- >>> lamToInt <$> (build <$> parse multiplyTerm "(1+2)*(1+1)")
-- Result >< Just 6
-- >>> lamToInt <$> (build <$> parse multiplyTerm "5")
-- Result >< Just 5
-- >>> lamToInt <$> (build <$> parse multiplyTerm "(1+5)")
-- Result >< Just 6

multiplyTerm :: Parser Builder
multiplyTerm = chain expTerm multiplyP

-- | Parser for exponential terms
--
-- >>> lamToInt <$> (build <$> parse expTerm "2**3")
-- Result >< Just 8
-- >>> lamToInt <$> (build <$> parse expTerm "(1+2)**(1+1)")
-- Result >< Just 9
-- >>> lamToInt <$> (build <$> parse expTerm "5")
-- Result >< Just 5
-- >>> lamToInt <$> (build <$> parse expTerm "(1+5)")
-- Result >< Just 6

expTerm :: Parser Builder
expTerm = chain (bracketTerm ||| intP) expP

-- | Parser for either addition or subtraction operations

basicOperator :: Parser Builder
basicOperator =  minusP ||| addP

-- | Parser for addition symbol in string representing an arithmetic expression
--
-- >>> parse addP "+"
-- Result >< \xy.y(\nfa.f(nfa))x
-- >>> parse addP " +"
-- Result >< \xy.y(\nfa.f(nfa))x
-- >>> parse addP "1 +"
-- UnexpectedChar '1'

addP :: Parser Builder
addP = do
    spaces
    is '+'
    spaces
    pure $ addBuilder
    -- pure $ ('x' `lam` 'y' `lam` term 'y' `ap` ('n' `lam` 'f' `lam` 'x' `lam` term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x')) `ap` term 'x')

-- | Parser for subtraction symbol in string representing an arithmetic expression
--
-- >>> parse minusP "-"
-- Result >< \xy.y(\nfa.n(\gh.h(gf))(\u.a)\u.u)x
-- >>> parse minusP " -"
-- Result >< \xy.y(\nfa.n(\gh.h(gf))(\u.a)\u.u)x
-- >>> parse minusP "1 -"
-- UnexpectedChar '1'

minusP :: Parser Builder
minusP = do
    spaces
    is '-'
    spaces
    pure $ minusBuilder
    -- pure $ ('x' `lam` 'y' `lam` term 'y' `ap` ('n' `lam` 'f' `lam` 'x' `lam` term 'n' `ap` ('g' `lam` 'h' `lam` term 'h' `ap` (term 'g' `ap` term 'f')) `ap` ('u' `lam` term 'x') `ap` ('u' `lam` term 'u') ) `ap` term 'x')

-- | Parser for multiplication symbol in string representing an arithmetic expression
--
-- >>> parse multiplyP "*"
-- Result >< \xyf.x(yf)
-- >>> parse multiplyP " *"
-- Result >< \xyf.x(yf)
-- >>> parse multiplyP "1 *"
-- UnexpectedChar '1'

multiplyP :: Parser Builder
multiplyP = do
    spaces
    is '*'
    spaces
    pure $ multiplyBuilder

-- | Parser for exponential symbol in string representing an arithmetic expression
--
-- >>> parse expP "**"
-- Result >< \xy.yx
-- >>> parse expP " **"
-- Result >< \xy.yx
-- >>> parse expP "1 **"
-- UnexpectedChar '1'

expP :: Parser Builder
expP = do
    spaces
    string "**"
    spaces
    pure $ expBuilder

-- | Parser for integers in string representing an arithmetic expression
--
-- >>> parse intP "1"
-- Result >< \f.f
-- >>> parse intP "2"
-- Result >< \fx.f(fx)
-- >>> parse intP "3"
-- Result >< \fx.f(f(fx))
-- >>> parse intP "4"
-- Result >< \fx.f(f(f(fx)))

intP :: Parser Builder
intP = do
    spaces
    d <- list digits
    pure $ intToLam (read d :: Int)

-- | Parser for digits
--
-- >>> parse digits "1"
-- Result >< '1'
-- >>> parse digits "22"
-- Result >2< '2'
-- >>> parse digits " 44"
-- UnexpectedChar ' '

digits :: Parser Char
digits = oneof "1234567890"



-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False

complexCalcP :: Parser Lambda
complexCalcP = build <$> logicExpr

-- | Additional functions for Exercise 3

-- | Builder for isZero lambda expression

isZero :: Builder
isZero = 'n' `lam` term 'n' `ap` ('x' `lam` (boolToLam False)) `ap` (boolToLam True)

-- | Builder for less or equal lambda expression

leqBuilder :: Builder
leqBuilder = 'm' `lam` 'n' `lam` isZero `ap` (minusBuilder `ap` term 'm' `ap` term 'n')

-- | Builder for equal lambda expression

eqBuilder :: Builder
eqBuilder = 'm' `lam` 'n' `lam` andBuilder `ap` (leqBuilder `ap` term 'm' `ap` term 'n') `ap` (leqBuilder `ap` term 'n' `ap` term 'm')

-- | Parser for "<=" 

leqP :: Parser Builder
leqP = do
    left <- arithmeticExpr
    spaces
    string "<=" 
    spaces
    right <- arithmeticExpr
    pure $ leqBuilder `ap` left `ap` right

-- | Parser for "<" 

lP :: Parser Builder
lP = do
    left <- arithmeticExpr
    spaces
    is '<'
    spaces
    right <- arithmeticExpr
    pure $ andBuilder `ap` (leqBuilder `ap` left `ap` right) `ap` (notBuilder `ap` (eqBuilder `ap` left `ap` right))

-- | Parser for "==" 

eqP :: Parser Builder
eqP = do
    left <- arithmeticExpr
    spaces
    string "==" 
    spaces
    right <- arithmeticExpr
    pure $ eqBuilder `ap` left `ap` right

-- | Parser for "!="

noteqP :: Parser Builder
noteqP = do
    left <- arithmeticExpr
    spaces
    string "!=" 
    spaces
    right <- arithmeticExpr
    pure $ notBuilder `ap` (eqBuilder `ap` left `ap` right)

-- | Parser for ">=" 

geqP :: Parser Builder
geqP = do
    left <- arithmeticExpr
    spaces
    string ">="
    spaces
    right <- arithmeticExpr
    pure $ orBuilder `ap` (notBuilder `ap` (leqBuilder `ap` left `ap` right)) `ap` (eqBuilder `ap` left `ap` right)

-- | Parser for "<" 

gP :: Parser Builder
gP = do
    left <- arithmeticExpr
    spaces
    is '>'
    spaces
    right <- arithmeticExpr
    pure $ notBuilder `ap` (leqBuilder `ap` left `ap` right)

-- | Parser for different types of complex expressions

complexExpr :: Parser Builder
complexExpr = leqP ||| lP ||| geqP ||| gP ||| eqP ||| noteqP

bracketComplex :: Parser Builder
bracketComplex = do
    spaces
    open
    spaces
    c <- complexExpr
    spaces
    close
    pure $ c

{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
listP :: Parser Lambda
listP = undefined

-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
listOpP :: Parser Lambda
listOpP = undefined


-- | Exercise 2

-- | Implement your function(s) of choice below!
