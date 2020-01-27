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

import System.Random
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
addTerm :: Fitness a b -> Rnd (Term a) -> Mutation a b
addTerm fitFun rndTerm s@(Sol e _ _) = do t <- rndTerm
                                          if e `hasTerm` t
                                          then return s
                                          else return . fitFun $ t `Plus` e

-- | Create a Drop term mutation.
--
--      Drops a random term of the expression.
--
--      You need to provide a Fitness function.
--
dropTerm :: Fitness a b -> Mutation a b
dropTerm fitFun (Sol e f s) = do let n = exprlen e
                                 i <- sampleTo (n-1)
                                 return . fitFun $ removeIthTerm i e

replaceTerm :: Fitness a b -> Rnd (Term a) -> Mutation a b
replaceTerm fitFun rndTerm s@(Sol e _ _) = do let n = exprlen e
                                              i <- sampleTo (n-1)
                                              t <- rndTerm
                                              if   e `hasTerm` t
                                              then return s
                                              else return . fitFun $ t `Plus` (removeIthTerm i e)  
                                            
replaceTrans :: Fitness a b -> Rnd (Transformation a) -> Mutation a b
replaceTrans fitFun rndTrans s@(Sol e _ _) = do let n = exprlen e
                                                i  <- sampleTo (n-1)
                                                tr <- rndTrans
                                                return . fitFun $ replace i tr e
  where
    change tr' (tr `After` i)  = (tr' `After` i)
    replace 0  tr Zero         = Zero
    replace 0  tr (t `Plus` e) = change tr t `Plus` e
    replace i  tr (t `Plus` e) = t `Plus` replace (i-1) tr e

combineInter :: (Int -> Int -> Int) -> Fitness a b -> Int -> Int -> Mutation a b                                              
combineInter op fitFun minExp maxExp s@(Sol e _ _) = do let n = exprlen e
                                                        i <- sampleTo (n-1)
                                                        j <- sampleTo (n-1)
                                                        let ti = fromJust $ getIthTerm i e
                                                            tj = fromJust $ getIthTerm j e
                                                            e' = removeIthTerm i e
                                                            ti'= combineBoth ti tj
                                                        return . fitFun . uniqueTerms $ ti' `Plus` e'
  where
    fromJust (Just x) = x
       
    combineBoth (tr1 `After` int1) (_ `After` int2) = tr1 `After` (applyInters int1 int2)
    
    applyInters One             _               = One
    applyInters _               One             = One
    applyInters (e1 `Times` i1) (e2 `Times` i2) = minmax (e1 `op` e2) `Times` (applyInters i1 i2)
      
    minmax x = min maxExp $ max minExp $ x


positiveInter = combineInter (+)
negativeInter = combineInter (-)

-- | Apply one of the mutation functions at random
mutFun :: Int                  -- ^ minExp
       -> Int                  -- ^ maxExp
       -> Fitness a b          -- ^ fitness function
       -> Rnd (Term a)         -- ^ random term generator
       -> Rnd (Transformation a)         -- ^ random term generator
       -> Solution a b         -- ^ Solution
       -> Rnd (Solution a b)   -- ^ random mutation generator
mutFun minExp maxExp f rndTerm rndTrans s = sampleFromList muts >>= id
  where
    muts   = [replaceTerm f rndTerm s
             ,replaceTrans f rndTrans s
             ,positiveInter f minExp maxExp s
             ,negativeInter f minExp maxExp s] ++ addMut ++ dropMut
             
    addMut  = if len <= 6 then [addTerm f rndTerm s] else []
    dropMut = if len >= 2 then [dropTerm f s]        else []
    len     = exprlen $ _expr s
    
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
