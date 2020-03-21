{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module      : IT.Knowledge
Description : Specific functions for Symbolic Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Definitions of evaluation of regression expressions with Interval.
-}
module IT.Knowledge where

import IT
import IT.Algorithms
import IT.Regression

import Numeric.Interval
import Data.Coerce

-- * IT specific stuff

-- | converts an expression of any Floating type to Interval
-- this is done to convince  the compiler that the same expression can 
-- also be used for Intervals
toInterval :: (Floating a, RealFloat b)  => Expr (Regression a) -> Expr (Regression (Interval b))
toInterval (Expr [])    = Expr []
toInterval (Expr (Term ts is:e)) = (Term (convert ts) is) `consTerm` toInterval (Expr e)
  where
    convert (Transformation "sin" _)      = regSin
    convert (Transformation "cos" _)      = regCos
    convert (Transformation "tan" _)      = regTan
    convert (Transformation "tanh" _)     = regTanh
    convert (Transformation "sqrt" _)     = regSqrt
    convert (Transformation "sqrt.abs" _) = regAbsSqrt
    convert (Transformation "log" _)      = regLog
    convert (Transformation "exp" _)      = regExp
    convert (Transformation "id" _)       = Transformation "id" id

-- | Check if one interval is inside the other
feasible :: RealFloat a => Interval a -> Interval a -> Bool
feasible range i = i `isSubsetOf` range

-- | Calculates the interval of an expression given the vector
-- of weights and the interval of each term of the expression
exprInterval :: RealFloat a => [a] -> [Interval a] -> Interval a
exprInterval ws ds = foldr comb (singleton 0) (zip ws ds)
  where
    comb (w,i) s = s + singleton w * i

-- | Calculates the intervals of each term
termIntervals :: RealFloat a => Expr (Regression a) -> [Interval a] -> [Interval a]
termIntervals expr ds = coerce $ evalExprToList (toInterval expr) (map Reg ds)

--  | checks if the expression is within a given interval
checkInterval :: RealFloat a => Interval a -> [a] -> Expr (Regression a) -> [Interval a] -> Bool
checkInterval range ws expr ds = (feasible range . exprInterval ws . termIntervals expr) ds

-- ** First order derivatives
evalDiffExpr :: RealFloat a => Expr (Regression a) -> [Regression a] -> [a] -> [Regression a]
evalDiffExpr (Expr ts) xs ws = foldr1 (zipWith (+)) wdtss
  where 
    dtss  = map (`evalDiffTerms` xs) ts
    wdtss = zipWith multWeight ws dtss
    multWeight w dts = map (`multReg` w) dts
    multReg (Reg x) w = Reg (x*w)

-- | Tries to apply the outer partial derivative
-- returns Nothing in case of error
applyDiff :: RealFloat a => (Transformation (Regression a)) -> Regression a -> Maybe (Regression a)
applyDiff (Transformation "sin" _) x      = Just (cos x)
applyDiff (Transformation "cos" _) x      = Just (- (sin x))
applyDiff (Transformation "tan" _) x      = Just ((recip . (^2) . cos) x)
applyDiff (Transformation "tanh" _) x     = Just ((recip . (^2) . cosh) x)
applyDiff (Transformation "sqrt" _) x     = Just ((recip . (*2) . sqrt) x)
applyDiff (Transformation "sqrt.abs" f) x = Just (x / (2 * (abs x)**(1.5)))
applyDiff (Transformation "log" _) x      = Just (recip x)
applyDiff (Transformation "exp" _) x      = Just (exp x)
applyDiff _ x = Nothing

-- | Evaluate the partial derivatives of each term
evalDiffTerms :: RealFloat a => Term (Regression a) -> [Regression a] -> [Regression a]
evalDiffTerms t@(Term tf is) xs = 
  case dt of
    Nothing -> di
    Just x  -> map (*x) di
  
    where 
      dt = evalDiffTerm t xs
      di = evalDiffInteractions is xs

-- | Evals the partial derivative of a term
evalDiffTerm :: RealFloat a => Term (Regression a) -> [Regression a] -> Maybe (Regression a)
evalDiffTerm (Term tf is) xs = applyDiff tf (itTimes xs is)

-- | Evals the partial derivatives of the interactions
evalDiffInteractions :: RealFloat a => Interaction -> [Regression a] -> [Regression a]
evalDiffInteractions ints@(Strength is) xs = map (evalPartialDiffInteractions ints xs) [0 .. n]
  where n = length is - 1

-- | Evals the partial derivative of the interaction w.r.t. the i-th variable
evalPartialDiffInteractions :: RealFloat a => Interaction -> [Regression a] -> Int -> Regression a
evalPartialDiffInteractions ints@(Strength is) xs i 
  | pi == 0   = 0
  | otherwise = fromIntegral pi * itTimes xs ints' 
  where
    pi = is !! i
    xi = xs !! i
    ints' = Strength (take i is ++ [pi-1] ++ drop (i+1) is)
