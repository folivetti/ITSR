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

Definitions of IT data structure and support functions.
-}
module IT.Knowledge where

import IT
import IT.Algorithms
import IT.Regression

import Numeric.Interval
import Data.Coerce

-- * IT specific stuff

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

feasible :: RealFloat a => Interval a -> Interval a -> Bool
feasible range i = i `isSubsetOf` range

exprInterval :: RealFloat a => [a] -> [Interval a] -> Interval a
exprInterval ws ds = foldr comb (singleton 0) (zip ws ds)
  where
    comb (w,i) s = s + singleton w * i

termIntervals :: RealFloat a => Expr (Regression a) -> [Interval a] -> [Interval a]
termIntervals expr ds = coerce $ evalExprToList (toInterval expr) (map Reg ds)

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

applyDiff :: RealFloat a => (Transformation (Regression a)) -> Regression a -> Maybe (Regression a)
applyDiff (Transformation "sin" _) x = Just (cos x)
applyDiff (Transformation "cos" _) x = Just (- (sin x))
applyDiff (Transformation "tan" _) x = Just ((recip . (^2) . cos) x)
applyDiff (Transformation "tanh" _) x = Just ((recip . (^2) . cosh) x)
applyDiff (Transformation "sqrt" _) x = Just ((recip . (*2) . sqrt) x)
applyDiff (Transformation "sqrt.abs" f) x = Just (x / (2 * (abs x)**(1.5)))
applyDiff (Transformation "log" _) x = Just (recip x)
applyDiff (Transformation "exp" _) x = Just (exp x)
applyDiff _ x = Nothing

evalDiffTerms :: RealFloat a => Term (Regression a) -> [Regression a] -> [Regression a]
evalDiffTerms t@(Term tf is) xs = 
  case dt of
    Nothing -> di
    Just x  -> map (*x) di
  
    where 
      dt = evalDiffTerm t xs
      di = evalDiffInteractions is xs

evalDiffTerm :: RealFloat a => Term (Regression a) -> [Regression a] -> Maybe (Regression a)
evalDiffTerm (Term tf is) xs = applyDiff tf (itTimes xs is)

evalDiffInteractions :: RealFloat a => Interaction -> [Regression a] -> [Regression a]
evalDiffInteractions ints@(Strength is) xs = map (evalPartialDiffInteractions ints xs) [0 .. n]
  where n = length is - 1

evalPartialDiffInteractions :: RealFloat a => Interaction -> [Regression a] -> Int -> Regression a
evalPartialDiffInteractions ints@(Strength is) xs i 
  | pi == 0   = 0
  | otherwise = fromIntegral pi * itTimes xs ints' 
  where
    pi = is !! i
    xi = xs !! i
    ints' = Strength (take i is ++ [pi-1] ++ drop (i+1) is)
