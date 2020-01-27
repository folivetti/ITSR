{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : IT.Random
Description : random generation of IT components
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

-}
module IT.Random where

import IT

import System.Random
import Control.Monad.State
import Data.List (intercalate)


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
sampleTrans ts = sampleFromList ts

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

-- | sample from [0,n]
sampleTo :: Int -> Rnd Int
sampleTo n = sampleRng 0 n

-- | Sample from a range of integers
sampleRng :: Int -> Int -> Rnd Int
sampleRng x y = state $ randomR (x,y)

-- | Sample from a range of integers excluding 0
sampleNZRng :: Int -> Int -> Rnd Int
sampleNZRng x y = do z <- sampleRng x y
                     if z == 0 
                     then sampleNZRng x y
                     else return z

-- | Sample a random element from a list
sampleFromList :: [a] -> Rnd a
sampleFromList xs = do let n = length xs
                       i <- sampleTo (n-1)
                       return (xs !! i)

-- | Toss a coin to your Witcher
toss :: Rnd Bool
toss = state random
