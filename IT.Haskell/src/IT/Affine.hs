{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
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
module IT.Affine where

import IT
import IT.Algorithms
import IT.Regression
import IT.Knowledge

import Numeric.Interval hiding (interval, singleton)
import Numeric.AffineForm
import qualified Numeric.AffineForm as AFF
import Numeric.AffineForm.ExplicitRounding

import Data.Coerce

-- * IT specific stuff
evalWithAffine :: (RealFloat a, ExplicitRounding a) => Expr (Regression a) -> [Interval a] -> [a] -> Interval a
evalWithAffine expr intervals ws = evalAFM (evalAffine expr intervals ws)

evalAffine :: (RealFloat a, ExplicitRounding a) => Expr (Regression a) -> [Interval a] -> [a] -> AFM s (Interval a)
evalAffine (Expr terms) is ws =
  do is' <- sequence $ map newFromInterval is
     let zs  = map (evalAffineTerm is') terms
         ws' = map singleton ws
     return . interval $ head zs --sum $ zipWith (*) ws' zs

evalAffineTerm :: (RealFloat a, ExplicitRounding a) => [AF t a] -> Term (Regression a) -> AF t a
evalAffineTerm is (Term tf (Strength ints)) = applyTrans tf $ product $ zipWith pow is ints
  where 
    pow x i = if i >= 0 then x^i else recip (x ^ (-i)) -- recip
    applyTrans (Transformation "sin" _)      = sin
    applyTrans (Transformation "cos" _)      = cos
    applyTrans (Transformation "tan" _)      = tan
    applyTrans (Transformation "tanh" _)     = tanh
    applyTrans (Transformation "sqrt" _)     = sqrt
    applyTrans (Transformation "sqrt.abs" _) = sqrt.abs
    applyTrans (Transformation "log" _)      = log 
    applyTrans (Transformation "exp" _)      = exp
    applyTrans (Transformation "id" _)       = id

-- ** First order derivatives
evalDiffExprAff :: (RealFloat a, ExplicitRounding a) => Expr (Regression a) -> [Interval a] -> [a] -> [Interval a]
evalDiffExprAff (Expr ts) is ws = map (\ix -> evalAFM $ evalDiffExprAff' ts is ws ix) [0 .. n]
  where n = length ws - 1

evalDiffExprAff' :: (RealFloat a, ExplicitRounding a) => [Term (Regression a)] -> [Interval a] -> [a] -> Int -> AFM s (Interval a)
evalDiffExprAff' ts is ws ix =
  do is' <- sequence $ map newFromInterval is
     let dts   = map (evalDiffTermsAff is' ix) ts
         ws'   = map singleton ws
         wdts  = zipWith (*) ws' dts
     return.interval $ foldr1 (+) wdts

evalDiffTermsAff :: (RealFloat a, ExplicitRounding a) => [AF t a] -> Int -> Term (Regression a) -> AF t a
evalDiffTermsAff xs ix t@(Term tf is) = 
  case dt of
    Nothing -> di
    Just x  -> di*x
  
    where 
      dt = evalDiffTermAff t xs
      di = evalDiffInteractionsAff is xs ix

evalDiffTermAff :: (RealFloat a, ExplicitRounding a) => Term (Regression a) -> [AF t a] -> Maybe (AF t a)
evalDiffTermAff (Term tf (Strength is)) xs = applyDiffAff tf (product $ zipWith pow xs is)
  where pow x i = if i >= 0 then x^i else recip (x ^ (-i))

evalDiffInteractionsAff :: (RealFloat a, ExplicitRounding a) => Interaction -> [AF t a] -> Int -> AF t a
evalDiffInteractionsAff ints@(Strength is) xs ix = evalPartialDiffInteractionsAff ints xs ix

evalPartialDiffInteractionsAff :: (RealFloat a, ExplicitRounding a) => Interaction -> [AF t a] -> Int -> AF t a
evalPartialDiffInteractionsAff (Strength is) xs i 
  | pi == 0   = 0
  | otherwise = fromIntegral pi * (product $ zipWith pow xs is')
  where
    pi = is !! i
    xi = xs !! i
    is' = take i is ++ [pi-1] ++ drop (i+1) is
    pow x i = if i >= 0 then x^i else recip (x ^ (-i))

applyDiffAff :: (RealFloat a, ExplicitRounding a) => (Transformation (Regression a)) -> AF t a -> Maybe (AF t a)
applyDiffAff (Transformation "sin" _) x = Just (cos x)
applyDiffAff (Transformation "cos" _) x = Just (- (sin x))
applyDiffAff (Transformation "tan" _) x = Just ((recip . (^2) . cos) x)
applyDiffAff (Transformation "tanh" _) x = Just ((recip . (^2) . cosh) x)
applyDiffAff (Transformation "sqrt" _) x = Just ((recip . (*2) . sqrt) x)
applyDiffAff (Transformation "sqrt.abs" f) x = Just (x / (2 * (abs x)**(1.5)))
applyDiffAff (Transformation "log" _) x = Just (recip x)
applyDiffAff (Transformation "exp" _) x = Just (exp x)
applyDiffAff _ x = Nothing
