{-# LANGUAGE InstanceSigs,ScopedTypeVariables,TupleSections #-}
-- DO NOT DISABLE WARNINGS IN YOUR CODE
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Defines the 'Lambda' datatype and operations on it.
module Data.Lambda (
  Lambda(..),DebugLambda(..),showsDebug,showDebug,recLambda
  ,usesIndex
  ,beta,eta,betaNormalise,etaNormalise,normalise,betaNormal,etaNormal,normal
  ,lamToShowS,lamToString,lamToBool,lamToInt
  ) where

import Control.Applicative
import Data.List hiding (map)
import Data.Maybe
import Data.Bool
import Prelude hiding (map)
import GHC.Stack
-- import Debug.Trace

-- $setup
-- >>> import Data.Lambda.Builder
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

-- | A representation of the untyped lambda calculus.
--
-- Variables are represented with <https://en.wikipedia.org/wiki/De_Bruijn_index "De Brujin"> indices.
--
-- >>> build $ 'x' `lam` 'y' `lam` term 'x'
-- \xy.x
data Lambda = LamVar Int -- A bound variable.
  | LamAp Lambda Lambda -- Application of one term to another.
  | LamAbs Char Lambda -- A lambda abstraction.
  | LamShow (Lambda -> Maybe ShowS) -- Converts a 'Lambda' to a 'String'.

-- | A type used to recursively show a 'Lambda'.
type ShowLam = Bool -- Flag if this term is the head term of a chain of applications.
  -> Bool -- Flag if this term is the tail term of a chain of applications.
  -> Int -- Value indicating the depth of this term in an abstraction.
  -> String -- The context of unique variable names.
  -> (Bool,ShowS) -- A flag if the term is an abstraction and a show function of the term.

instance Show Lambda where
  showsPrec :: Int -> Lambda -> ShowS
  showsPrec _ l = snd (recLambda recVar recAp recAbs recShow l True True 0 [])
    where
      recVar :: Lambda -> Int -> ShowLam
      recVar _ i _ _ _ vars = (False,showChar var)
        where
          var    = fromMaybe errStr $ lookup i $ zip [0..] vars
          errStr = error ("No variable exists with index `" ++ show i
            ++ "` indexes only go up to `" ++ show (length vars - 1) ++ "`")
      recAp :: Lambda -> ShowLam -> ShowLam -> ShowLam
      recAp (LamAp (LamShow f) n) _ n' isHead isTail absDepth vars =
        maybe
        (n' isHead isTail absDepth vars) -- Show the remaining value as normal.
        (False,)
        $ f $ normal n
      recAp _ m n isHead isTail _ vars = let
          (_,m') = m True  False 0 vars
          (_,n') = n False isTail 0 vars
          ss     = m' . n'
        in (False,if isHead then ss else open . ss . close)
      recAbs :: Lambda -> Char -> ShowLam -> ShowLam
      recAbs _ c e _ isTail absDepth vars = let
          -- | Find a free variable name.
          altC       = fromMaybe (error "exhausted all variable names")
            $ find (`notElem`vars) $ ['a'..'z'] ++ ['A'..'Z']
          -- | A unique variable name.
          c'         = if c /= '_' && c `elem` vars then altC else c
          -- | The context for the body.
          vars'      = c':vars
          (isAbs,e') = e True True (absDepth + 1) vars'
          -- | Shows all of the immediate context along with the body.
          altBody    =
            foldl'
            (flip $ (.) . showChar)
            (showChar c')
            (take absDepth vars) . dot . e'
          -- | Shows the body, optionally finalising the chain of abstractions.
          body       = if isAbs then e' else altBody
          -- | Shows the body, optionally beginning the chain of abstractions.
          ss         = if absDepth /= 0 then body else lambda . body
        in (True,if isTail then ss else open . ss . close)
      recShow :: Lambda -> (Lambda -> Maybe ShowS) -> ShowLam
      recShow _ _ _ _ _ _ = (False,error "Can't show a <showLam> on its own")
      [open,close,lambda,dot] = showChar <$> "()\\." :: [ShowS]

-- | A type which provides a verbose Show instance for 'Lambda'.
newtype DebugLambda = MkDebug {
  getDebugLambda :: Lambda -- The wrapped 'Lambda'.
  }

instance Show DebugLambda where
  showsPrec :: Int -> DebugLambda -> ShowS
  showsPrec _ = showsDebug . getDebugLambda

{-
  BEGIN OPERATIONS DEFINITIONS
-}

-- | A type used to recursively show a 'Lambda' with debub syntax.
type ShowLamDebug = String -- The context of unique variable names.
  -> ShowS -- A show function of the term.

-- | A very verbose show function for 'Lambda's which includes all brackets.
--
-- Name clashes are handled by reassigning the second occourance to the first
-- available name.
--
-- Missing names are displayed as an underscore followed by the index.
--
-- >>> let num = build $ intToLam 2
-- >>> putStrLn $ showsDebug num ""
-- (\f.(\x.(f (f x))))
showsDebug :: HasCallStack => Lambda -> ShowS
showsDebug l = recLambda recVar recAp recAbs recShow l []
  where
    -- | Show the free variable and the index.
    recVar :: Lambda -> Int -> ShowLamDebug
    recVar _ i vars = maybe (showChar '_' . shows i) showChar $ lookup i $ zip [0..] vars
    recAp :: Lambda -> ShowLamDebug -> ShowLamDebug -> ShowLamDebug
    recAp (LamAp (LamShow f) n) _ n' vars =
      maybe
      (showString "<showLam> " . n' vars) -- Show the remaining value as normal.
      ((. close) . (open .)) -- Include brackets around the output.
      $ f $ normal n
    recAp _ m n vars = open . m vars . space . n vars . close
    recAbs :: Lambda -> Char -> ShowLamDebug -> ShowLamDebug
    recAbs _ c e vars = open . lambda . showChar c' . dot . e (c':vars) . close
      where
        -- | Find a free variable name.
        altC = fromMaybe (error "exhausted all variable names")
          $ find (`notElem`vars) $ ['a'..'z'] ++ ['A'..'Z']
        -- | A unique variable name.
        c'   = if c /= '_' && c `elem` vars then altC else c
    recShow :: Lambda -> (Lambda -> Maybe ShowS) -> ShowLamDebug
    recShow _ _ _ = showString "<showLam>"
    [open,close,space,lambda,dot] = showChar <$> "() \\."

-- | A very verbose show function for 'Lambda's which includes all brackets.
--
-- Name clashes are handled by reassigning the second occourance to the first
-- available name.
--
-- Missing names are displayed as an underscore followed by the index.
--
-- >>> let num = build $ intToLam 2
-- >>> putStrLn $ showDebug num
-- (\f.(\x.(f (f x))))
showDebug :: Lambda -> String
showDebug = flip showsDebug ""

-- | This is the elimination function for 'Lambda'.
--
-- You can think of this function as <https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:foldr foldr> for 'Lambda'.
-- Each case receives the 'Lambda' the function was called on and the results
-- of the recursive calls.
recLambda :: forall a. (Lambda -> Int -> a) -- The eliminator for lambda variables.
  -> (Lambda -> a -> a -> a) -- The eliminator for lambda application.
  -> (Lambda -> Char -> a -> a) -- The eliminator for lambda abstractions.
  -> (Lambda -> (Lambda -> Maybe ShowS) -> a) -- The eliminator for LamShow.
  -> Lambda -- The lambda to recur over.
  -> a -- The final result.
recLambda recVar recAp recAbs recShow = go
  where
    -- | Case analysis for Lambda elimination.
    go :: Lambda -> a
    go l@(LamVar i)   = recVar l i
    go l@(LamAp m n)  = recAp l (rec m) $ rec n
    go l@(LamAbs c e) = recAbs l c $ rec e
    go l@(LamShow f) = recShow l f
    -- | Recursive elimination of Lambdas.
    rec :: Lambda -> a
    rec = recLambda recVar recAp recAbs recShow

-- | Performs a single beta reduction step.
--
-- >>> beta $ build $ ('x' `lam` term 'x') `ap` intToLam 0
-- Just \fx.x
beta :: Lambda -> Maybe Lambda
beta l = listToMaybe $ betaNormalise l

-- | Performs a single eta reduction step.
--
-- >>> eta $ build $ 'x' `lam` 'y' `lam` term 'x' `ap` term 'y'
-- Just \x.x
eta :: Lambda -> Maybe Lambda
eta l = listToMaybe $ etaNormalise l

-- | A type used to beta reduce a 'Lambda'.
type BetaLam = Int -- The index of the argument.
  -> Lambda -- The updated term.

-- | Produces a stream of beta normalisation steps.
--
-- >>> let true  = boolToLam True
-- >>> let false = boolToLam False
-- >>> let not   = 'b' `lam` term 'b' `ap` false `ap` true
-- >>> betaNormalise $ build $ not `ap` true
-- [(\t_.t)(\_f.f)\t_.t,(\__f.f)\t_.t,\_f.f]
betaNormalise :: Lambda -- The term to normalise.
  -> [Lambda] -- The reduction steps.
betaNormalise (LamAp (LamAbs _ body) expr) =
  liftA2 (:) id betaNormalise
  $ recLambda recVar recAp recAbs recShow body 0 where
    -- | Cache the fixed Lambdas.
    fixed :: [Lambda]
    fixed = fixDepth <$> [0..]
    -- | Fix the variables by offsetting them by the terms new depth.
    fixDepth :: Int -> Lambda
    fixDepth = recLambda recVar' recAp' recAbs' recShow' expr 0
      where
        recVar' l i depth' offset
          | i >= depth' = LamVar $ i + offset
          | otherwise   = l
        recAp' _ = liftA2 (liftA2 LamAp)
        recAbs' _ c e depth' = LamAbs c . e (depth' + 1)
        recShow' l _ _ _ = l
    recVar :: Lambda -> Int -> BetaLam
    recVar l i arg
      | i == arg  = fixed !! arg
      | i > arg   = LamVar $ i - 1
      | otherwise = l
    recAp :: Lambda -> BetaLam -> BetaLam -> BetaLam
    recAp _ = liftA2 LamAp
    recAbs :: Lambda -> Char -> BetaLam -> BetaLam
    recAbs _ c e arg = LamAbs c $ e $ arg + 1
    recShow :: Lambda -> (Lambda -> Maybe ShowS) -> BetaLam
    recShow l _ _ = l
betaNormalise (LamAp (LamAp (LamShow _) m@(LamAbs _ _)) n) = betaNormalise $ LamAp m n
betaNormalise (LamAp m@(LamShow _) n)                      = let
    f n'@(LamAp (LamShow _) _) = n'
    f n'                       = LamAp m n'
  in f <$> betaNormalise n
betaNormalise (LamAp m n) = let
    m' = listToMaybe $ betaNormalise m
    ns = betaNormalise n
  in
    maybe
    (LamAp m <$> ns) -- Reduce the right hand side.
    (liftA2 (:) id betaNormalise . (`LamAp` n)) -- Continue reducing with the updated right.
    m'
betaNormalise (LamAbs c e) = LamAbs c <$> betaNormalise e
betaNormalise _ = []

-- | A type used to eta reduce a 'Lambda'.
type EtaLam = Int -- The index of the argument being eta reduced.
  -> Bool -- A flag if the argument is used in the term.

usesIndex :: Int -> Lambda -> Bool
usesIndex = flip $ recLambda recVar recAp recAbs recShow
  where
    recVar :: Lambda -> Int -> EtaLam
    recVar _ = (==)
    recAp :: Lambda -> EtaLam -> EtaLam -> EtaLam
    recAp _ = liftA2 (||)
    recAbs :: Lambda -> Char -> EtaLam -> EtaLam
    recAbs _ _ = (. (+1))
    recShow :: Lambda -> (Lambda -> Maybe ShowS) -> EtaLam
    recShow _ _ _ = False

-- | Produces a stream of eta normalisation steps.
--
-- >>> let if' = 'b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f'
-- >>> etaNormalise $ build if'
-- [\bt.bt,\b.b]
etaNormalise :: Lambda -> [Lambda]
etaNormalise (LamAbs c (LamAp m (LamVar 0)))
  -- If the param is not used in `m` then we can eta reduce.
  | not $ usesIndex 0 m = liftA2 (:) id etaNormalise $ fixDepth m
  | otherwise                         = LamAbs c <$> etaNormalise m
  where
    -- | Fix indicies which are affected by an eta reduction.
    fixDepth :: Lambda -> Lambda
    fixDepth l = snd $ recLambda recVar' recAp' recAbs' recShow' l 0
      where
        recVar' :: Lambda -> Int -> Int -> (Bool,Lambda)
        recVar' l' i lim
          | i > lim   = (True,LamVar $ i - 1)
          | otherwise = (False,l')
        recAp' :: Lambda -> (Int -> (Bool,Lambda)) -> (Int -> (Bool,Lambda)) -> Int -> (Bool,Lambda)
        recAp' l' m' n' lim = let
            (mb,m'') = m' lim
            (nb,n'') = n' lim
            b        = mb || nb
            l''      = if b then LamAp m'' n'' else l'
          in (b,l'')
        recAbs' :: Lambda -> Char -> (Int -> (Bool,Lambda)) -> Int -> (Bool,Lambda)
        recAbs' l' c' e lim = let
            (b,e') = e $ lim + 1
            l''    = if b then LamAbs c' e' else l'
          in (b,l'')
        recShow' :: Lambda -> (Lambda -> Maybe ShowS) -> Int -> (Bool,Lambda)
        recShow' l' _ _ = (False,l')
etaNormalise (LamAp m n) =
  foldr
  (\m' f _ -> LamAp m' n : f m') -- Pass the reduced expression forwards.
  (\m' -> LamAp m' <$> etaNormalise n) -- Reduce the right half of the expression.
  (etaNormalise m) m
etaNormalise (LamAbs c e) = foldr branch [] $ etaNormalise e
  where
    -- | Attempt to continue eta normalisation from the top each time.
    branch :: Lambda -> [Lambda] -> [Lambda]
    branch e' ls = let
        l   = LamAbs c e'
        ls' = etaNormalise l
      in l:if null ls' then ls else ls'
etaNormalise _ = []

-- | Produces a stream of eta then beta normalisation steps.
--
-- >>> let true  = boolToLam True
-- >>> let false = boolToLam False
-- >>> let not   = 'b' `lam` term 'b' `ap` false `ap` true
-- >>> let if'   = 'b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f'
--
-- >>> normalise $ build $ not `ap` true
-- [(\t_.t)(\_f.f)\t_.t,(\__f.f)\t_.t,\_f.f]
--
-- >>> normalise $ build if'
-- [\bt.bt,\b.b]
normalise :: Lambda -> [Lambda]
normalise l = foldr (\l' f _ -> l':f l') etaNormalise (betaNormalise l) l

-- | Get the beta normal form of a 'Lambda' term.
--
-- This function will cause an infinite loop if the 'Lambda' does not terminate.
--
-- betaNormal = last . betaNormalise
--
-- >>> let true  = boolToLam True
-- >>> let false = boolToLam False
-- >>> let not   = 'b' `lam` term 'b' `ap` false `ap` true
-- >>> betaNormal $ build $ not `ap` true
-- \_f.f
betaNormal :: Lambda -> Lambda
betaNormal (LamAp (LamAbs _ body) expr) = betaNormal
  $ recLambda recVar recAp recAbs recShow body 0
  where
    -- | Cache the fixed Lambdas.
    fixed :: [Lambda]
    fixed = fixDepth <$> [0..]
    -- | Fix the variables by offsetting them by the terms new depth.
    fixDepth :: Int -> Lambda
    fixDepth = recLambda recVar' recAp' recAbs' recShow' (betaNormal expr) 0
      where
        recVar' l i depth' offset
          | i >= depth' = LamVar $ i + offset
          | otherwise   = l
        recAp' _ = liftA2 (liftA2 LamAp)
        recAbs' _ c e depth' = LamAbs c . e (depth' + 1)
        recShow' l _ _ _ = l
    recVar :: Lambda -> Int -> BetaLam
    recVar l i arg
      | i == arg  = fixed !! arg
      | i > arg   = LamVar $ i - 1
      | otherwise = l
    recAp :: Lambda -> BetaLam  -> BetaLam  -> BetaLam
    recAp _ = liftA2 LamAp
    recAbs :: Lambda -> Char -> BetaLam -> BetaLam
    recAbs _ c e arg = LamAbs c $ e $ arg + 1
    recShow :: Lambda -> (Lambda -> Maybe ShowS) -> BetaLam
    recShow l _ _ = l
betaNormal (LamAp (LamAp (LamShow _) m@(LamAbs _ _)) n) = betaNormal $ LamAp m n
betaNormal (LamAp m@(LamShow _) n)                      = case betaNormal n of
  n'@(LamAp (LamShow _) _) -> n'
  n'                       -> LamAp m n'
betaNormal (LamAp m n) = let
    m'   = betaNormal m
    n'   = betaNormal n
    cont = case m' of
      (LamAbs _ _)                     -> betaNormal
      (LamAp (LamShow _) (LamAbs _ _)) -> betaNormal
      (LamShow _)                      -> betaNormal
      _                                -> id
  in cont $ LamAp m' n'
betaNormal (LamAbs c e) = LamAbs c $ betaNormal e
betaNormal l = l

type EtaNormal = Int -> Maybe Lambda

-- | Get the eta normal form of a 'Lambda' term.
--
-- etaNormal = last . etaNormalise
--
-- >>> let if' = 'b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f'
-- >>> normalise $ build if'
-- [\bt.bt,\b.b]
etaNormal :: Lambda -> Lambda
etaNormal l@(LamAbs _ (LamAp (LamShow _) (LamVar 0))) = l
etaNormal l@(LamAbs _ (LamAp expr (LamVar 0))) = maybe l etaNormal
  $ recLambda recVar recAp recAbs recShow expr 0
  where
    recVar :: Lambda -> Int -> EtaNormal
    recVar l' i arg
      | i == arg  = Nothing
      -- Fix the indicies.
      | otherwise = Just $ if i > arg then LamVar $ i - 1 else l'
    recAp :: Lambda -> EtaNormal -> EtaNormal -> EtaNormal
    recAp _ = liftA2 (liftA2 LamAp)
    recAbs :: Lambda -> Char -> EtaNormal -> EtaNormal
    recAbs _ c e arg = fmap (LamAbs c) $ e $ arg + 1
    recShow :: Lambda -> (Lambda -> Maybe ShowS) -> EtaNormal
    recShow l' _ _ = Just l'
etaNormal l = recLambda recVar recAp recAbs recShow l
  where
    recVar l' _ = l'
    recAp _ = LamAp
    recAbs _ c e = LamAbs c $ etaNormal e
    recShow l' _ = l'

-- | Get the normal form of a 'Lambda' term.
--
-- This function will cause an infinite loop if the 'Lambda' does not terminate.
--
-- normal = etaNormal . betaNormal
--
-- >>> let true  = boolToLam True
-- >>> let false = boolToLam False
-- >>> let not   = 'b' `lam` term 'b' `ap` false `ap` true
-- >>> let if'   = 'b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f'
-- >>> normal $ build $ if' `ap` not `ap` true
-- \fa.a
normal :: Lambda -> Lambda
normal = etaNormal . betaNormal

-- | A type used to recursively show a 'Lambda'.
type ShowLamMaybe = Bool -- Flag if this term is the head term of a chain of applications.
  -> Bool -- Flag if this term is the tail term of a chain of applications.
  -> Int -- Value indicating the depth of this term in an abstraction.
  -> String -- The context of unique variable names.
  -> Maybe (Bool,ShowS) -- A flag if the term is an abstraction and a show function of the term.

-- | A show function which cannot error.
lamToShowS :: Lambda -> Maybe ShowS
lamToShowS l = snd <$> recLambda recVar recAp recAbs recShow l True True 0 []
  where
    recVar :: Lambda -> Int -> ShowLamMaybe
    recVar _ i _ _ _ vars = fmap ((False,) . showChar) $ lookup i $ zip [0..] vars
    recAp :: Lambda -> ShowLamMaybe -> ShowLamMaybe -> ShowLamMaybe
    recAp (LamAp (LamShow f) n) _ n' isHead isTail absDepth vars =
      maybe
      (n' isHead isTail absDepth vars) -- Show the remaining value as normal.
      (Just . (False,))
      $ f $ normal n
    recAp _ m n isHead isTail _ vars = do
      (_,m') <- m True  False 0 vars
      (_,n') <- n False isTail 0 vars
      let ss = m' . n'
      pure (False,if isHead then ss else open . ss . close)
    recAbs :: Lambda -> Char -> ShowLamMaybe -> ShowLamMaybe
    recAbs _ c e _ isTail absDepth vars = do
      -- Find a free variable name.
      let altC = find (`notElem`vars) $ ['a'..'z'] ++ ['A'..'Z']
      -- A unique variable name.
      c' <- if c /= '_' && c `elem` vars then altC else Just c
      -- The context for the body.
      let vars' = c':vars
      (isAbs,e') <- e True True (absDepth + 1) vars'
      let
        -- | Shows all of the immediate context along with the body.
        altBody =
          foldl'
          (flip $ (.) . showChar)
          (showChar c')
          (take absDepth vars) . dot . e'
        -- | Shows the body, optionally finalising the chain of abstractions.
        body = if isAbs then e' else altBody
        -- | Shows the body, optionally beginning the chain of abstractions.
        ss = if absDepth /= 0 then body else lambda . body
      pure (True,if isTail then ss else open . ss . close)
    recShow :: Lambda -> (Lambda -> Maybe ShowS) -> ShowLamMaybe
    recShow _ _ _ _ _ _ = Nothing
    [open,close,lambda,dot] = showChar <$> "()\\." :: [ShowS]

-- | A show function which cannot error.
lamToString :: Lambda -> Maybe String
lamToString = fmap ($"") <$> lamToShowS

-- | Recognises a normalised church bool.
--
-- prop> Just b == lamToBool (build $ boolToLam b)
lamToBool :: Lambda -> Maybe Bool
lamToBool = go . normal
  where
    go (LamAp (LamShow _) l)            = go l
    go (LamAbs _ (LamAbs _ (LamVar i)))
      | i <= 1    = Just $ i == 1
      | otherwise = Nothing
    go _                                = Nothing

-- | Recognises a normalised church numeral.
--
-- prop> n < 0 || Just n == lamToInt (build $ intToLam n)
lamToInt :: Lambda -> Maybe Int
lamToInt = go . normal
  where
    go (LamAp (LamShow _) l)      = go l
    go (LamAbs _ (LamVar 0))      = Just 1
    go (LamAbs f (LamAbs x (LamAp (LamShow _) body))) = go $ LamAbs f $ LamAbs x body
    go (LamAbs _ (LamAbs _ body)) = snd <$> recLambda recVar recAp recAbs recShow body
      where
        recVar _ i
          | i <= 1    = Just (i==0,i)
          | otherwise = Nothing
        recAp _ (Just (False,1)) = fmap $ fmap (1+)
        recAp _ _                = const Nothing
        recAbs _ _ _ = Nothing
        recShow _ _ = Nothing
    go _                          = Nothing
