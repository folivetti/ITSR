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

import System.Random
import Control.Monad.State
import Data.List (intercalate, nub)
import qualified Numeric.LinearAlgebra as V

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
-- evaluate an expression given the task 'a'
--
-- For example, an 'IT Regression' is represented by
-- an 'Expr Double' and should be evaluated with a 
-- vector of input parameters of type 'Vector Double'.
itTimesDefault :: (Monoid a1, Coercible a1 c) => (a2 -> a1) -> [a2] -> Interaction -> c
itTimesDefault p xs (Strength is) = coerce . fold $ zipWith stimesMonoid is (map p xs)

itAddDefault :: (Foldable t, Monoid a1, Coercible a1 b) => (a2 -> a1) -> t a2 -> b
itAddDefault p xs                 = coerce $ foldMap p xs

class IT a where
  itTimes  :: [a] -> Interaction -> a
  itAdd    :: [a] -> a
  itWeight :: Double -> a -> a

evalTerm :: IT a => Term a -> [a] -> a
evalTerm (Term (Transformation _ f) is) xs = f (itTimes xs is)

evalExprToList :: (IT a) => Expr a -> [a] -> [a]
evalExprToList (Expr es) xs = map (\t -> evalTerm t xs) es

evalExpr :: (IT a) => Expr a -> [a] -> [Double] -> a
evalExpr (Expr es) xs ws = itAdd $ zipWith itWeight ws (map (\t -> evalTerm t xs) es)
  
-- * 'Show' and 'Eq' instances

instance (Show a) => Show (Expr a) where
  show (Expr es) = intercalate " + " $ map show es

instance Show a => Show (Term a) where
   show (Term tr i)   = show tr ++ "(" ++ show i ++ ")" 

instance Show a => Show (Transformation a) where
  show (Transformation s _) = s

instance Show Interaction where
  show (Strength es) = intercalate "*" $ filter (/="") $ zipWith show' [0..] es
    where show' n 0 = ""
          show' n 1 = 'x' : show n
          show' n e = ('x' : show n) ++ "^(" ++  show e ++ ")"
          
instance Eq (Term a) where
  (Term tr1 (Strength i1)) == (Term tr2 (Strength  i2)) = i1 == i2

instance Eq (Transformation a) where
  (Transformation s1 _) == (Transformation s2 _) = s1==s2
  
-- * Utility functions

-- | Remove duplicated terms
-- implementation similar to 'nub'
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

exprlen (Expr e) = length e
consTerm t (Expr e) = Expr (t:e)
consInter i (Strength is) = Strength (i:is)
