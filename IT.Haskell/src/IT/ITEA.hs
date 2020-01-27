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
module IT.ITEA where

import IT -- (itea, addTerm, dropTerm)
import IT.Algorithms
import IT.Random

import System.Random
import Control.Monad.Extra (iterateM)

import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as S

-- ---------------------------------------------------------------------------

-- * ITEA

-- | Creates an infinite list of populations where the /i/-th 
-- element corresponds to t he /i/-th generation.
itea :: Mutation a b -> Population a b -> Rnd [Population a b]
itea f = iterateM (step f)

-- | Tournament Selection
--
-- given the concatenation of the previous population
-- and the mutated children, it will return a sampled
-- selection of these combined population with
-- the same size as the original population.
--
tournament :: Population a b -> Rnd (Population a b)
tournament p = tournament' p (length p `div` 2)
  where
    tournament' p 0 = return Empty
    tournament' p n = do pi <- chooseOne p
                         p' <- tournament' p (n-1)
                         return $ pi :<| p' 

    chooseOne :: Population a b -> Rnd (Solution a b)
    chooseOne p = do let n = S.length p
                     c1 <- sampleTo (n-1)
                     c2 <- sampleTo (n-1)
                     return (min (p `S.index` c1) (p `S.index` c2))

-- | Perform one iterative step of ITEA
step :: Mutation a b -> Population a b -> Rnd (Population a b)
step mutFun pop = do
  pop'  <- sequence $ mutFun <$> pop
  tournament (pop >< pop')
