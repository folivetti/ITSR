{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : IT
Description : IT expression data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Definitions of IT data structure and support functions.
-}
module IT where

import Data.Coerce
import Data.Foldable
import Data.Semigroup

import Data.List (intercalate, nub)

-- * IT expression definitions
-- An IT expression  is a set of additive terms (with optional weighting)
-- with each term being the composition of a /transformation/ function ('a -> a')
-- with a /interaction/ function ('[a] -> a').
-- The interaction function is the product of each variable to a given power.

newtype Interaction      = Strength [Int]
data    Transformation a = Transformation String (a -> a)
data    Term a           = Term (Transformation a) Interaction
newtype Expr a           = Expr [Term a]


-- * Class 'IT' of IT-expressions

-- | The 'IT' class defines how we should
-- evaluate an expression given the task 'a'.
--
-- It is defined by the functions
-- `itTimes` : given a vector of `a` and the Interaction strength, how to compute the product
-- `itAdd`   : given a vector of `a`, how to add the terms together
-- `itWeight` : how to apply the weight into the terms
--
-- This default for this instance is to provide a Semiring instance for the product
-- a Semiring instance for the sum, and a way to use the weights (optional).
class IT a where
  itTimes  :: [a] -> Interaction -> a
  itAdd    :: [a] -> a
  itWeight :: Double -> a -> a

-- | The Coercible instance is just to make the unboxing of
-- the type faster. It is a safe operation.
--

-- given a type a2, provide a function to convert t o a Monoid a1 and apply a fold
-- after applying `stimes` of the interaction strengths. This only works for
-- positive interactions
itTimesDefault :: (Monoid a1, Coercible a1 c) => (a2 -> a1) -> [a2] -> Interaction -> c
itTimesDefault p xs (Strength is) = coerce . fold $ zipWith stimesMonoid is (map p xs)

-- given a type a2, provides a function to convert to a Monoid a1 and apply fold
itAddDefault :: (Foldable t, Monoid a1, Coercible a1 b) => (a2 -> a1) -> t a2 -> b
itAddDefault p xs                 = coerce $ foldMap p xs

-- to evaluate a term we apply the transformation function
-- to the product Monoid of interaction
evalTerm :: IT a => Term a -> [a] -> a
evalTerm (Term (Transformation _ f) is) xs = f (itTimes xs is)

-- | evaluates the expression into a list of terms
-- in case the evaluated values are needed
evalExprToList :: (IT a) => Expr a -> [a] -> [a]
evalExprToList (Expr es) xs = map (\t -> evalTerm t xs) es

-- | evaluates an expression by evaluating the terms into a list
-- applying the weight and summing the results.
evalExpr :: (IT a) => Expr a -> [a] -> [Double] -> a
evalExpr es xs ws = itAdd $ zipWith itWeight ws (evalExprToList es xs)
  
-- * 'Show' and 'Eq' instances
-- obs.: maybe this makes more sense specifically for each instance of IT
instance (Show a) => Show (Expr a) where
  show (Expr es) = intercalate " + " $ map show es

instance Show a => Show (Term a) where
   show (Term tr i)   = show tr ++ "(" ++ show i ++ ")" 

instance Show a => Show (Transformation a) where
  show (Transformation s _) = s

-- | TODO: replace variable names with provided labels
instance Show Interaction where
  show (Strength es) = intercalate "*" $ filter (/="") $ zipWith show' [0..] es
    where show' n 0 = ""
          show' n 1 = 'x' : show n
          show' n e = ('x' : show n) ++ "^(" ++  show e ++ ")"
          
-- | Two terms are equal if their interactions are equal
-- this instance is used for the mutation operation to avoid adding 
-- two equal interactions on the same expression. 
instance Eq (Term a) where
  (Term tr1 (Strength i1)) == (Term tr2 (Strength  i2)) = (i1 == i2)

instance Eq (Transformation a) where
  (Transformation s1 _) == (Transformation s2 _) = s1==s2
  
-- * Utility functions

-- | Remove duplicated terms
uniqueTerms :: Expr a -> Expr a
uniqueTerms (Expr ts) = Expr (nub ts)

-- | Check whether and expression has a given term.
hasTerm :: Expr a -> Term a -> Bool
hasTerm (Expr ts) t = t `elem` ts


-- * Internal functions

removeIthTerm :: Int -> Expr a -> Expr a
removeIthTerm i (Expr e) = Expr (take i e ++ drop (i+1) e)

getIthTerm :: Int -> Expr a -> Maybe (Term a)
getIthTerm ix (Expr e) = if   ix >= length e
                         then Nothing
                         else Just (e !! ix)

exprlen (Expr e)          = length e

consTerm t (Expr e) = Expr (t:e)

consInter i (Strength is) = Strength (i:is)


getListOfTerms (Expr ts) = ts
numberOfTerms (Expr ts) = length ts

getDimension (Expr ts) = getDim (head ts)
  where getDim (Term _ (Strength is)) = length is

getInteractions (Term _ (Strength is)) = is
