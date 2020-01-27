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

import System.Random
import Control.Monad.State
import Data.List (intercalate)
import qualified Numeric.LinearAlgebra as V

-- * IT expression definitions
-- An IT expression  is a set of additive terms (with optional weighting)
-- with each term being the composition of a /transformation/ function ('a -> a')
-- with a /interaction/ function ('[a] -> a').
-- The interaction function is the product of each variable to a given power.

data Interaction      = One | Int `Times` Interaction
data Transformation a = T String (a -> a)
data Term           a = (Transformation a) `After` Interaction 
data Expr           a = Zero | Term a `Plus` (Expr a)

-- | infix priority of constructors
infixr 6 `Times`
infixr 6 `Plus`

-- * Class 'IT' of IT-expressions

-- | The 'IT' class defines how we should
-- evaluate an expression given the task 'a'
--
-- For example, an 'IT Regression' is represented by
-- an 'Expr Double' and should be evaluated with a 
-- vector of input parameters of type 'Vector Double'.
class IT a where
  type Rep a :: *
  evalExpr  :: Expr (Rep a)           -> [Rep a] -> [a]
  evalTerm  :: Term (Rep a)           -> [Rep a] -> a
  evalTrans :: Transformation (Rep a) -> a                -> a 
  evalInter :: Interaction            -> [Rep a] -> a
  
-- * 'Show' and 'Eq' instances

instance (Show a) => Show (Expr a) where
  show Zero            = ""
  show (t `Plus` Zero) = show t
  show (t `Plus` e)    = show t ++ " + " ++ show e

instance Show a => Show (Term a) where
   show (t `After` i) = show t ++ "(" ++ show i ++ ")"

instance Show a => Show (Transformation a) where
  show (T s _) = s

instance Show Interaction where
  show es = intercalate "*" $ show' es 0
    where
      show' One            _ = []
      show' (0 `Times` es) n = show' es (n+1)
      show' (1 `Times` es) n = ("x" ++ show n) : show' es (n+1)
      show' (e `Times` es) n = ("x" ++ show n ++ "^" ++ show e) : show' es (n+1)

instance Eq (Term a) where
  (t1 `After` i1) == (t2 `After` i2) = i1 == i2

instance Eq (Transformation a) where
  (T s1 _) == (T s2 _) = s1==s2

instance Eq Interaction where
  One == One = True
  One == _   = False
  _   == One = False
  (i1 `Times` is1) == (i2 `Times` is2) = (i1==i2) && (is1 == is2)

-- * Utility functions

-- | Remove duplicated terms
-- implementation similar to 'nub'
uniqueTerms :: Expr a -> Expr a
uniqueTerms Zero         = Zero
uniqueTerms (t `Plus` e) = t `Plus` uniqueTerms (filterExpr (/=t) e)
  where
    filterExpr p Zero = Zero
    filterExpr p (t `Plus` e) 
      | p t       = t `Plus` filterExpr p e
      | otherwise = filterExpr p e

-- | Check whether and expression has a given term.
hasTerm :: Expr a -> Term a -> Bool
hasTerm Zero _ = False
hasTerm (t `Plus` e) t' = t'==t || hasTerm e t'
