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
                                                            ti'= combineBoth minExp maxExp ti tj
                                                        return . fitFun $ ti' `Plus` e'
  where
    fromJust (Just x) = x
       
    combineBoth minExp maxExp (tr1 `After` int1) (tr2 `After` int2) = tr1 `After` (applyInters op minExp maxExp int1 int2)
    
positiveInter = combineInter (+)
negativeInter = combineInter (-)

-- | Apply one of the mutation functions at random
mutFun :: Int
       -> Int
       -> Fitness a b          -- ^ fitness function
       -> Rnd (Term a)         -- ^ random term generator
       -> Rnd (Transformation a)         -- ^ random term generator
       -> Solution a b         -- ^ Solution
       -> Rnd (Solution a b)   -- ^ random mutation generator
mutFun minExp maxExp f rndTerm rndTrans s@(Sol e _ _) 
  | len >= 6  = dropTerm f s
  | len <= 2  = addTerm  f rndTerm s
  | otherwise = do n <- sampleTo 5
                   case n of
                     0 -> dropTerm f s 
                     1 -> addTerm  f rndTerm s
                     2 -> replaceTerm f rndTerm s
                     3 -> replaceTrans f rndTrans s
                     4 -> positiveInter f minExp maxExp s
                     5 -> negativeInter f minExp maxExp s
    where
      len = exprlen e
    
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

applyInters op minExp maxExp One             _               = One
applyInters op minExp maxExp _               One             = One
applyInters op minExp maxExp (e1 `Times` i1) (e2 `Times` i2) = e'' `Times` (applyInters op minExp maxExp i1 i2)
  where e' = e1 `op` e2
        e'' = if e' > maxExp 
              then maxExp
              else if e' < minExp then minExp
                   else e'

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
