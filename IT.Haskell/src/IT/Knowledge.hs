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
import Unsafe.Coerce

import Numeric.Interval

-- * IT specific stuff

toInterval :: Floating a => Expr (Regression a) -> Expr (Regression (Interval Double))
toInterval (Expr [])    = Expr []
toInterval (Expr (Term ts is:e)) = (Term (convert ts) is) `consTerm` toInterval (Expr e)
  where
    convert (Transformation "sin" _) = regSin
    convert (Transformation "cos" _) = regCos
    convert (Transformation "tan" _) = regTan
    convert (Transformation "tanh" _) = regTanh
    convert (Transformation "sqrt" _) = regSqrt
    convert (Transformation "sqrt.abs" _) = regAbsSqrt
    convert (Transformation "log" _) = regLog
    convert (Transformation "exp" _) = regExp
    convert (Transformation "id" _) = Transformation "id" id
