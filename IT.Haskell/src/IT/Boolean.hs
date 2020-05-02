{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Boolean where

import Data.Semigroup

import IT
import IT.Algorithms

-- * IT specific stuff

-- | Regression problem will deal with inputs that are instances of 'Floating'
-- This makes it possible to evaluate the expression with a vector of Double
-- or a Vector of Intervals.
newtype Boolean = B {_unBool :: Bool}

-- | Simply shows the packed 'Double'
instance Show Boolean where
  show (B b) = show b

toB :: (Bool -> Bool -> Bool) -> (Boolean -> Boolean -> Boolean)
toB f b1 b2 = B (f (_unBool b1) (_unBool b2))

bOr  = toB (||)
bAnd = toB (&&)

-- | IT Instance for regression evals an expression to
-- sum(w_i * term_i) + w0
-- with
--      term_i = f_i(p_i(xs))
--  and p_i(xs) = prod(x^eij)
instance Floating a => IT (Regression a) where
  itTimes xs (Strength is) = product $ zipWith pow xs is
    where
      pow (Reg x) i = if i<0
                      then Reg (x**(fromIntegral i))  --  recip (x^(abs i))
                      else Reg (x^i)
    
  -- we can use the Sum Monoid to add the terms
  itAdd   = itAddDefault Sum
  itWeight x (Reg y) = Reg (realToFrac x * y)


instance IT Boolean where
  itTimes bs (Strength is) = B $ all $ zipWith pow bs is
    where
      pow (B b) i | i < 0  = not b
                  | i == 0 = True
                  | i > 0  = b
  itAdd   = itAddDefault   Any
  itWeight w by = by
  
-- | Transformation Functions
boolLinear    = [Transformation "id" id]

-- * Error measures

-- | Regression statitstics
data BoolStats = BS { _acc     :: Double
                    , _prec    :: Double
                    , _weights :: [Double]
                    }
                    
-- | Displaying the statistics, one per line
instance Show BoolStats where
  show (BS a p w) = stats ++ "\n\n"
    where
      stats = unlines $ zipWith (++) names vals
      names = ["Accuracy: ", "Precision: ", "Weights: "]
      vals  = [show a, show p, show w]
      
fitnessReg :: Int -> [[Regression Double]] -> Vector -> [Expr (Regression Double)] -> Population (Regression Double) RegStats
fitnessTest :: [[Regression Double]] -> Vector -> Solution (Regression Double) RegStats -> RegStats

