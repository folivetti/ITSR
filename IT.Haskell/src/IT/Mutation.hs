{-|
Module      : IT.Mutation
Description : Mutation operators for ITEA
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

-}
module IT.Mutation where

import IT -- (itea, addTerm, dropTerm)
import IT.Algorithms
import IT.Random

import Control.Monad.Extra (iterateM)

import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as S

-- ---------------------------------------------------------------------------


-- * Mutation builder functions


-- | Create an Add new term mutation.
--
--      Adds a random term into the expression.
--      If this term already exists in the expression,
--      it returns the original expression without modification.
--
--      You need to provide a Fitness function and a function that
--      samples a random term.
--
addTerm :: Rnd (Term a) -> Mutation a
addTerm rndTerm e = do t <- rndTerm
                       if e `hasTerm` t
                       then return e
                       else return (t `Plus` e)

-- | Create a Drop term mutation.
--
--      Drops a random term of the expression.
--
--      You need to provide a Fitness function.
--
dropTerm :: Mutation a
dropTerm e = do let n = exprlen e
                i <- sampleTo (n-1)
                return (removeIthTerm i e)

replaceTerm :: Rnd (Term a) -> Mutation a
replaceTerm rndTerm e = do let n = exprlen e
                           i <- sampleTo (n-1)
                           t <- rndTerm
                           if   e `hasTerm` t
                           then return e
                           else return $ t `Plus` (removeIthTerm i e)  
                                            
replaceTrans :: Rnd (Transformation a) -> Mutation a
replaceTrans rndTrans e = do let n = exprlen e
                             i  <- sampleTo (n-1)
                             tr <- rndTrans
                             return (replace i tr e)
  where
    change tr' (tr `After` i)  = (tr' `After` i)
    replace 0  tr Zero         = Zero
    replace 0  tr (t `Plus` e) = change tr t `Plus` e
    replace i  tr (t `Plus` e) = t `Plus` replace (i-1) tr e

combineInter :: (Int -> Int -> Int) -> Int -> Int -> Mutation a
combineInter op minExp maxExp e = do let n = exprlen e
                                     i <- sampleTo (n-1)
                                     j <- sampleTo (n-1)
                                     let ti = fromJust $ getIthTerm i e
                                         tj = fromJust $ getIthTerm j e
                                         e' = removeIthTerm i e
                                         ti'= combineBoth ti tj
                                     if  allZeros ti'
                                     then return e'
                                     else return . uniqueTerms $ ti' `Plus` e'
  where
    allZeros (_ `After` int) = allZeros' int
    allZeros' One = True
    allZeros' (0 `Times` i) = allZeros' i
    allZeros' (_ `Times` i) = False
    
    fromJust (Just x) = x
       
    combineBoth (tr1 `After` int1) (_ `After` int2) = tr1 `After` (applyInters int1 int2)
    
    applyInters One             _               = One
    applyInters _               One             = One
    applyInters (e1 `Times` i1) (e2 `Times` i2) = minmax (e1 `op` e2) `Times` (applyInters i1 i2)
      
    minmax x = min maxExp $ max minExp $ x


positiveInter = combineInter (+)
negativeInter = combineInter (-)

-- | Apply one of the mutation functions at random
mutFun :: (Int, Int)             -- ^ minExp, maxExp
       -> (Int, Int)             -- ^ minTerms, maxTerms
       -> Rnd (Term a)           -- ^ random term generator
       -> Rnd (Transformation a) -- ^ random term generator
       -> Expr a                 -- ^ Expression to be mutated
       -> Rnd (Expr a)           -- ^ Random Expression generator
mutFun (minExp, maxExp) (minTerms, maxTerms) rndTerm rndTrans e = sampleFromList muts >>= id
  where
    muts   = [replaceTerm rndTerm         e
             ,replaceTrans rndTrans       e
             ,positiveInter minExp maxExp e
             ,negativeInter minExp maxExp e] ++ addMut ++ dropMut
             
    addMut  = if len <= maxTerms then [addTerm rndTerm e] else []
    dropMut = if len >= minTerms then [dropTerm e]        else []
    len     = exprlen e
    
-- * Internal functions

removeIthTerm :: Int -> Expr a -> Expr a
removeIthTerm _ Zero         = Zero
removeIthTerm 0 (t `Plus` e) = e
removeIthTerm i (t `Plus` e) = t `Plus` removeIthTerm (i-1) e

getIthTerm :: Int -> Expr a -> Maybe (Term a)
getIthTerm _ Zero = Nothing
getIthTerm 0 (t `Plus` e) = Just t
getIthTerm i (t `Plus` e) = getIthTerm (i-1) e

exprlen Zero = 0
exprlen (t `Plus` e) = 1 + exprlen e
