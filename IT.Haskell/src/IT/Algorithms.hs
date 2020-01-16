{-|
Module      : IT.Algorithms
Description : Basic algorithms data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Definitions  for a Solution, Population of solutions, 
fitness function and mutation function.
-}
module IT.Algorithms where

import IT

import Data.Sequence (Seq(..))

-- | data type containing a solution and some necessary stats
--  'a' refers to the type of 'Expr', 'b' refers to a container of statistics.
data Solution a b = Sol { _expr :: Expr a   -- ^ The IT expression of type a
                        , _fit  :: Double   -- ^ Fitness for evaluating the expression
                        , _stat :: b        -- ^ A set of problem-specific statistics (e.g. error, accuracy,..)
                        }

instance (Show a, Show b) => Show (Solution a b) where
  show (Sol e f s) = "Expression: " ++ show e ++ "\nStats: \n" ++  show s
  
-- | These instances are only to find the best and worst individuals
-- of a population.
instance Eq (Solution a b) where
  -- | 'Eq' instance to sort a sequence
  -- of solutions by fitness
  s1 == s2 = _fit s1 == _fit s2

instance Ord (Solution a b) where
  -- | 'Ord' instance to sort a sequence
  -- of solutions by fitness
  s1 <= s2 = _fit s1 <= _fit s2

-- | A population of 'Solution a b'
type Population a b = Seq (Solution a b)

-- | 'Fitness' function with signature 'Expr a -> Solution a b'
type Fitness    a b = Expr a -> Solution a b -- (Expr a, Double, b)

-- | 'Mutation' function with signature 'Solution a b -> Rnd (Solution a b)'
type Mutation   a b = Solution a b -> Rnd (Solution a b)
