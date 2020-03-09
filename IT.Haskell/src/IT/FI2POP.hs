{-|
Module      : IT.ITEA
Description : Interaction-Transformation Evolutionary Algorithm
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Generic implementation of Interaction-Transformation Evolutionary Algorithm
for any instance of IT expression.

To run itea you just need to call 'itea mutFun pop0', 
where 'mutFun' is a mutation function of the type 'Mutation a b',
and 'pop0' is the initial 'Population a b' of solutions.
This function will result in an infinite list of populations, with
the /i/-th element being the population of the /i/-th generation.

This library also provides some generic mutation function builders.

A sample example is provided by "ITEAReg" module.
-}
module IT.FI2POP where

import IT -- (itea, addTerm, dropTerm)
import IT.Algorithms
import IT.Random

import Control.Monad.Extra (iterateM)

import Control.DeepSeq

import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as S
-- ---------------------------------------------------------------------------

-- * ITEA

-- | Creates an infinite list of populations where the /i/-th 
-- element corresponds to t he /i/-th generation.
fi2pop :: (NFData a, NFData b) => Mutation a -> Fitness a b -> Constraint a b -> Population a b -> Rnd [(Population a b, Population a b)]
fi2pop f g h pop = let n  = length pop
                       (pf, pi) = splitPop h pop
                   in  iterateM (step f g h n) (pf, pi)

splitPop constFun pop = splitPop' constFun pop ([],[])

splitPop' _ [] (pf,pi) = (pf,pi)
splitPop' constFun (p@(Sol e f s):pop) (pf, pi)
    | c == 0    = splitPop' constFun pop (p:pf, pi)
    | otherwise = splitPop' constFun pop (pf, p':pi)
  where
    c  = constFun p
    p' = Sol e c s
    
-- | Generate an Initial Population at Random
initialPop :: Int                -- dimension
           -> Int                -- maxTerms
           -> Int                -- nPop
           -> Rnd (Term a)
           -> Fitness a b        -- fitness function
           -> Rnd (Population a b)
initialPop dim maxTerms nPop rndTerm fit = fit <$> initialPop' dim maxTerms nPop
  where
    rndExpr dim n = sampleExpr rndTerm n
    initialPop' dim maxTerms 0    = return []
    initialPop' dim maxTerms nPop = do n <- sampleRng 1 maxTerms
                                       e  <- rndExpr dim n
                                       let s = uniqueTerms e
                                       ss <- initialPop' dim maxTerms (nPop-1)
                                       return $ s : ss


-- | Tournament Selection
--
-- given the concatenation of the previous population
-- and the mutated children, it will return a sampled
-- selection of these combined population with
-- the same size as the original population.
--
tournament :: Population a b -> Int -> Rnd (Population a b)
tournament p 0 = return [] 
tournament p n = do pi <- chooseOne p
                    p' <- tournament p (n-1)
                    return $ pi:p' 
  where
    chooseOne :: Population a b -> Rnd (Solution a b)
    chooseOne p = do let n = length p
                     c1 <- sampleTo (n-1)
                     c2 <- sampleTo (n-1)
                     return (min (p !! c1) (p !! c2)) 

-- | Perform one iterative step of FI2POP
step :: (NFData a, NFData b) => Mutation a -> Fitness a b -> Constraint a b -> Int -> (Population a b, Population a b) -> Rnd (Population a b, Population a b)
step mutFun fitFun constFun nPop (pf, pi) = do
  exprs  <- sequence $ mutFun . _expr <$> (pf++pi)
  let pop = fitFun exprs
      (pf',pi') = splitPop constFun pop
  pf'' <- tournament (pf ++ pf') nPop
  pi'' <- tournament (pi ++ pi') nPop
  return (pf'', pi'')

