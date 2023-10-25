{-# LANGUAGE InstanceSigs,ScopedTypeVariables #-}

-- | Defines the 'Lambda' datatype and operations on it.
module Data.Builder (
  Builder,build,map,lamToBuilder,term,ap,ap',lam,lam',showLam
  ,boolToLam,intToLam
  ) where

import Control.Applicative
import Data.Lambda
import Data.List hiding (map)
import Data.Maybe
import Data.Bool
import Data.Bifunctor
import Prelude hiding (map)
-- import Debug.Trace

-- $setup
-- >>> import Data.Bifunctor
-- >>> import GHC.Float
-- >>> import Test.QuickCheck
-- >>> import GHC.Base (maxInt)
-- >>> :{
--  let
--    buildInt :: Int -> Lambda
--    buildInt = build . intToLam
--    equating :: Eq a => (b -> a) -> b -> b -> Bool
--    equating f a b = f a == f b
-- :}
--
-- >>> :{
--  instance Eq Lambda where
--    (LamVar i)   == (LamVar j)   = i == j
--    (LamAp m n)  == (LamAp x y)  = m == x && n == y
--    (LamAbs _ e) == (LamAbs _ f) = e == f
--    _            == _            = False
-- :}
--
-- >>> :{
--  instance Eq Builder where
--    (==) = equating build
-- :}
--
-- >>> :{
--  instance Arbitrary Builder where
--    arbitrary = intToLam <$> chooseInt (0,10)
-- :}
--
-- >>> :{
--  instance Arbitrary Lambda where
--    arbitrary = build <$> arbitrary
-- :}

-- | The result type of running a 'Builder'.
--
-- Failure contains the malformed 'Lambda' and a function which produced an error message
-- for that 'Lambda'.
type BuildResult = Either (ShowS,Lambda) Lambda

-- | A builder for 'Lambda' values.
--
-- A builder only allows well formed expressions with no free variables.
--
-- >>> build $ 'x' `lam` 'y' `lam` term 'x'
-- \xy.x
newtype Builder = MkBuilder {
  runBuilder :: [Char] -> BuildResult -- Run the builder with the given namespace.
  }

instance Show Builder where
  showsPrec :: Int -> Builder -> ShowS
  showsPrec _ = shows . build

-- | Attempts to extract a 'Lambda' term from the 'Builder'.
--
-- If the inner 'Lambda' expression is malformed, this function crashes with an error
-- message displaying the 'Lambda' and an explanation.
--
-- * Free variables are printed as an underscore followed by the number of variables they
--   had access to.
-- * Negative numbers are printed as an underscore followed by `-1`.
--
-- >>> build $ 'y' `lam` term 'x'
-- *** Exception: The expression `(\y._1)` is malformed:
--   Error: The expression contains a free variable `x`
--
-- >>> build $ intToLam (-2)
-- *** Exception: The expression `_-1` is malformed:
--   Error: The expression contains a negative number `-2`
build :: Builder -> Lambda
build = either showErr id . ($[]) . runBuilder
  where
    showErr :: (ShowS,Lambda) -> Lambda
    showErr (ss,l) = errorWithoutStackTrace (
      showString "The expression `" . showsDebug l
      . showString "` is malformed:\n" $ ss ""
      )

-- | Maps the 'Lambda' built by the 'Builder'.
--
-- >>> let two = intToLam 2
--
-- prop> (normal $ build $ n) == (build $ map normal $ n)
map :: (Lambda -> Lambda) -> Builder -> Builder
map f (MkBuilder g) = MkBuilder $ fmap f <$> g

-- | Wraps the 'Lambda' in a Builder.
--
-- prop> l == build (lamToBuilder l)
lamToBuilder :: Lambda -> Builder
lamToBuilder = MkBuilder . const . Right

-- | Creates a free variable.
--
-- >>> build $ 'x' `lam` term 'x'
-- \x.x
--
-- >>> build $ 'y' `lam` term 'x'
-- *** Exception: The expression `(\y._1)` is malformed:
--   Error: The expression contains a free variable `x`
term :: Char -> Builder
term c = MkBuilder $ (\vars -> let
    -- | The failure result, indicating the number of variables by its index.
    noVar = Left (hasFree,LamVar $ length vars)
  in maybe noVar (Right . LamVar) $ elemIndex c vars)
  where
    -- | Error message for free variables.
    hasFree :: ShowS
    hasFree = (showString "  Error: The expression contains a free variable `"
      . showChar c . showString "`")

-- | Applies the first term to the second.
--
-- >>> build $ 'x' `lam` 'y' `lam` 'z' `lam` term 'x' `ap` term 'z' `ap` (term 'y' `ap` term 'z')
-- \xyz.xz(yz)
--
-- >>> let x = term 'x'
-- >>> let f = term 'f'
-- >>> build $ 'f' `lam` ('x' `lam` f `ap` (x `ap` x)) `ap` ('x' `lam` f `ap` (x `ap` x))
-- \f.(\x.f(xx))\x.f(xx)
--
-- >>> let x = term 'x'
-- >>> let f = term 'f'
-- >>> build ('f' `lam` (('x' `lam` (f `ap` (x `ap` x))) `ap` ('x' `lam` (f `ap` (x `ap` x)))))
-- \f.(\x.f(xx))\x.f(xx)
--
-- >>> let l = 'x' `lam` term 'x'
-- >>> build $ l `ap` l
-- (\x.x)\x.x
ap :: Builder -> Builder -> Builder
ap (MkBuilder m) (MkBuilder n) = MkBuilder $ liftA2 merge m n
  where
    -- | Merge the outputs including errors.
    merge :: BuildResult -> BuildResult -> BuildResult
    merge (Right m')     (Right n')     = Right $ LamAp m' n'
    merge (Right m')     (Left n')      = Left $ LamAp m' <$> n'
    merge (Left m')      (Right n')     = Left $ (`LamAp` n') <$> m'
    merge (Left (em,m')) (Left (en,n')) = Left $ (em . showChar '\n' . en,LamAp m' n')

-- | Applies the first term to the second.
--
-- >>> let l = build $ 'x' `lam` term 'x'
-- >>> build $ l `ap'` l
-- (\x.x)\x.x
ap' :: Lambda -> Lambda -> Builder
ap' = (lamToBuilder .) . LamAp
infixl 6 `ap`,`ap'`

-- | Binds the variable in the body.
--
-- >>> build $ 'x' `lam` term 'x'
-- \x.x
lam :: Char -> Builder -> Builder
lam c (MkBuilder e) = MkBuilder $ bimap (fmap $ LamAbs c) (LamAbs c) . e . (c:)

-- | Binds the variable in the body.
--
-- >>> let l = build $ 'x' `lam` term 'x'
-- >>> build $ 'y' `lam'` l
-- \yx.x
lam' :: Char -> Lambda -> Builder
lam' = (lamToBuilder .) . LamAbs
infixr 5 `lam`,`lam'`

-- | Adds custome 'Show' logic.
--
-- >>> let s = showLam $ const $ Just $ showString "Hello, World!"
-- >>> build $ s `ap` ('x' `lam` term 'x')
-- Hello, World!
--
-- >>> build s
-- *** Exception: Can't show a <showLam> on its own
-- CallStack (from HasCallStack):
--   error, called at src/Data/Lambda.hs:119:36 in main:Data.Lambda
showLam :: (Lambda -> Maybe ShowS) -> Builder
showLam = MkBuilder . const . Right . LamShow

-- | Creates a Church boolean.
--
-- prop> Just b == lamToBool (build $ boolToLam b)
--
-- >>> build $ boolToLam True
-- \t_.t
--
-- >>> build $ boolToLam False
-- \_f.f
boolToLam :: Bool -> Builder
boolToLam = bool ('_' `lam` 'f' `lam` term 'f') ('t' `lam` '_' `lam` term 't')

-- | Creates a Church numeral.
--
-- prop> n < 0 || Just n == lamToInt (build $ intToLam n)
--
-- >>> build $ intToLam 2
-- \fx.f(fx)
--
-- >>> build $ intToLam 1
-- \f.f
--
-- >>> build $ intToLam (-2)
-- *** Exception: The expression `_-1` is malformed:
--   Error: The expression contains a negative number `-2`
intToLam :: Int -> Builder
intToLam n
  | n < 0     = MkBuilder $ const $ Left (isNeg,LamVar (-1))
  | n == 1    = 'f' `lam` term 'f'
  | otherwise = lamToBuilder $ LamAbs 'f' $ LamAbs 'x'
    $ iterate (LamAp (LamVar 1)) (LamVar 0) !! n
  where
    -- | Error message for negative numbers.
    isNeg :: ShowS
    isNeg = (showString "  Error: The expression contains a negative number `"
      . shows n . showString "`")
