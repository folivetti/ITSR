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
import qualified Data.Vector.Unboxed as V


-- * IT expression definitions
-- An IT expression  is a set of additive terms (with optional weighting)
-- with each term being the composition of a /transformation/ function ('a -> a')
-- with a /interaction/ function ('[a] -> a').
-- The interaction function is the product of each variable to a given power.

data Interaction      = One | Int `Times` Interaction
data Transformation a = T String (a -> a)
data Term           a = (Transformation a) `After` Interaction deriving Eq
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
  evalExpr  :: Expr (Rep a)           -> V.Vector (Rep a) -> [a]
  evalTerm  :: Term (Rep a)           -> V.Vector (Rep a) -> a
  evalTrans :: Transformation (Rep a) -> a                -> a 
  evalInter :: Interaction            -> V.Vector (Rep a) -> a
  
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

-- * Random expressions generation
 
-- | The type used for generating random values of 'a'
type Rnd a = State StdGen a

-- | Creates a random interaction with up to n variables. 
--   Guaranteed to choose at least one variable.
sampleInterMax :: Int               -- ^ problem dimension
               -> Int               -- ^ maximum number of variables in this interaction
               -> Int               -- ^ minimum exponent            
               -> Int               -- ^ maximum exponent
               -> Rnd Interaction   -- ^ Random interaction generator
sampleInterMax 0   _      _      _      = return One
sampleInterMax dim 0      minExp maxExp = do t <- sampleInterMax (dim-1) 0 minExp maxExp
                                             return $ 0 `Times` t
sampleInterMax 1   budget minExp maxExp = do e <- sampleNZRng minExp maxExp
                                             return $ e `Times` One
sampleInterMax dim budget minExp maxExp = do b <- toss
                                             if b
                                             then do e <- sampleNZRng minExp maxExp
                                                     t <- sampleInterMax (dim-1) (budget-1) minExp maxExp
                                                     return $ e `Times` t
                                             else do t <- sampleInterMax (dim-1) budget minExp maxExp
                                                     return $ 0 `Times` t

sampleInter :: Int               -- ^ problem dimension
            -> Int               -- ^ minimum exponent            
            -> Int               -- ^ maximum exponent
            -> Rnd Interaction   -- ^ Random interaction generator
sampleInter 0   _      _      = return One
sampleInter dim minExp maxExp = do e <- sampleRng minExp maxExp
                                   t <- sampleInter (dim-1) minExp maxExp
                                   return $ e `Times` t
                                          
                                          
-- | Samples a random transformation function from a provided list of functions
sampleTrans :: [Transformation a]      -- ^ choices of transformation functions
            -> Rnd (Transformation a)  -- ^ Random generator
sampleTrans ts = do ix <- sampleTo (length ts - 1)
                    return $ ts !! ix

-- | Samples a random term using a random transformation and a random interaction generators.
sampleTerm :: Rnd (Transformation a)  -- ^ random transformation function
           -> Rnd Interaction         -- ^ random interaction function
           -> Rnd (Term a)            -- ^ Random generator
sampleTerm rndTrans rndInter = do t <- rndTrans
                                  i <- rndInter
                                  return $ t `After` i

-- | Create a random expression with exactly n terms
sampleExpr :: Rnd (Term a)         -- ^ random term function
           -> Int                  -- ^ number of terms
           -> Rnd (Expr a)         -- ^ Random generator
sampleExpr _ 0            = return Zero
sampleExpr rndTerm nTerms = do t <- rndTerm 
                               e <- sampleExpr rndTerm (nTerms-1)
                               return $ t `Plus` e

-- | Create a random population of n expressions with varying number of terms
samplePop :: Int                   -- population size
          -> Int                   -- max number of terms
          -> (Int -> Rnd (Expr a)) -- random expression generator
          -> Rnd [Expr a]
samplePop nPop maxNTerms rndExpr = do n  <- sampleRng 1 maxNTerms
                                      e  <- rndExpr n
                                      es <- samplePop (nPop-1) maxNTerms rndExpr
                                      return $ e : es

-- * Utility random functions

sampleTo :: Int -> Rnd Int
sampleTo n = state $ randomR (0,n)

sampleRng :: Int -> Int -> Rnd Int
sampleRng x y = state $ randomR (x,y)

sampleNZRng :: Int -> Int -> Rnd Int
sampleNZRng x y = do z <- sampleRng x y
                     if z == 0 
                     then sampleNZRng x y
                     else return z
toss :: Rnd Bool
toss = state random
