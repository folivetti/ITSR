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
     return . interval $ sum $ zipWith (*) ws' zs

evalAffineTerm :: (RealFloat a, ExplicitRounding a) => [AF t a] -> Term (Regression a) -> AF t a
evalAffineTerm is (Term tf (Strength ints)) = applyTrans tf $ product $ zipWith pow is ints
  where 
    pow x i = if i >= 0 then x^i else recip (x ^ (-i))
    applyTrans (Transformation "sin" _)      = sin
    applyTrans (Transformation "cos" _)      = cos
    applyTrans (Transformation "tan" _)      = tan
    applyTrans (Transformation "tanh" _)     = tanh
    applyTrans (Transformation "sqrt" _)     = sqrt
    applyTrans (Transformation "sqrt.abs" _) = sqrt.abs
    applyTrans (Transformation "log" _)      = log
    applyTrans (Transformation "exp" _)      = exp
    applyTrans (Transformation "id" _)       = id

