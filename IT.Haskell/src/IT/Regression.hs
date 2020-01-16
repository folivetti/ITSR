{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : IT.Regression
Description : Specific functions for Symbolic Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Definitions of IT data structure and support functions.
-}
module IT.Regression where

import IT
import IT.ITEA
import IT.Algorithms

import qualified Data.Vector.Unboxed as V
import Statistics.Regression
import Statistics.Matrix.Types

import Data.List (foldl1')

-- * IT specific stuff

-- | Regression problem will deal with inputs of type 'Double'
newtype Regression = Reg {_unReg :: Double}
                       deriving (Num, Floating, Fractional)

-- | Simply shows the packed 'Double'
instance Show Regression where
  show (Reg x) = show x

-- | IT Instance for regression evals an expression to
-- sum(w_i * term_i) + w0
-- with
-- term_i = f_i(p_i(xs))
-- and p_i(xs) = prod(x^eij)
instance IT Regression where
  type Rep Regression = Double

  evalExpr Zero _            = []
  evalExpr (t `Plus` es) xs  = evalTerm t xs : evalExpr es xs

  evalTerm (t `After` i) xs  = evalTrans t $ evalInter i xs

  evalTrans (T _ f) (Reg x)  = Reg (f x)

  evalInter One _            = Reg 1.0
  evalInter (e `Times` i) xs 
    | V.null xs = error "Error: Vector of variables is smaller than the number of interactions!"
    | otherwise = Reg x ** fromIntegral e * evalInter i xs'
        where
          x   = V.head xs
          xs' = V.tail xs

-- | Transformation Functions
regSin     = T "sin" sin
regCos     = T "cos" cos
regTan     = T "tan" tan
regTanh    = T "tanh" tanh

regSqrt    = T "sqrt" sqrt
regAbsSqrt = T "sqrt.abs" (sqrt.abs)
regLog     = T "log" log
regExp     = T "exp" exp

regTrig      = [regSin, regCos, regTan, regTanh]
regNonLinear = [regSqrt, regAbsSqrt, regLog, regExp]
regLinear    = [T "id" id]

regAll = regTrig ++ regNonLinear ++ regLinear

-- * Error measures

-- | Mean and variance for vector of doubles
mean :: Vector -> Double
mean xs = V.sum xs / fromIntegral (V.length xs)

var :: Vector -> Double
var xs = sum' / fromIntegral (V.length xs)
  where
    mu   = mean xs
    sum' = V.foldl (\s x -> s + (x-mu)^2) 0 xs

-- | generic mean error measure
meanError :: (Double -> Double) -- ^ a function to be applied to the error terms (abs, square,...)
          -> Vector             -- ^ fitted values
          -> Vector             -- ^ target values
          -> Double
meanError op ysHat ys = mean $ V.map op $ V.zipWith (-) ysHat ys

-- | Common error measures for regression: 
-- MSE, MAE, RMSE, NMSE, r^2
mse           = meanError (^2)
mae           = meanError abs
nmse ysHat ys = mse ysHat ys / var ys
rmse ysHat ys = sqrt $ mse ysHat ys
rSq ysHat ys  = 1 - r/t
  where
    ym      = mean ys
    t       = sumOfSq $ V.map (\yi -> yi -ym) ys
    r       = sumOfSq $ V.zipWith (-) ys ysHat
    sumOfSq = V.foldl (\s di -> s + di^2) 0

-- | Predict a linear model
-- the input matrix is transpose because of Statistics.Regression
(.+.) = V.zipWith (+)

predict :: [Vector] -> Vector -> Vector
predict xss ws = V.map (+b) $ foldl1' (.+.) zss
  where
    vss = xss
    ws' = V.toList $ V.init ws
    b   = V.last ws    
    zss = zipWith (\w vs -> V.map (*w) vs) ws' vss
    
-- | Regression statitstics
data RegStats = RS { _rmse    :: Double
                   , _mae     :: Double
                   , _nmse    :: Double
                   , _r2      :: Double
                   , _weights :: Vector  -- ^ the fitted weights for the IT expression
                   } 

-- | Displaying the statistics, one per line
instance Show RegStats where
  show (RS r m n r2 w) = stats ++ "\n\n"
    where
      stats = unlines $ zipWith (++) names vals
      names = ["RMSE: ", "MAE: ", "NMSE: ", "r^2: ", "Weights: "]
      vals  = [show r, show m, show n, show r2, show w]


-- * Utility functions
    
-- | Clean the expression removing invalid terms w.r.t. the training data
-- it may return an empty expression
cleanExpr :: Expr Double -> [Vector] -> (Expr Double, [[Double]])
cleanExpr e xs = removeInvalid' e (Zero, [])
  where
    removeInvalid' Zero (e,l) = (e, reverse l)
    removeInvalid' (t `Plus` e) (e',l) 
      | anyInvalid = removeInvalid' e (e',l)
      | otherwise  = removeInvalid' e (t `Plus` e', zs : l)
        where
          zs          = fmap (_unReg . evalTerm @Regression t) xs
          anyInvalid  = any isInvalid zs
          isInvalid x = isNaN x || isInfinite x

-- | Fitness function for regression
-- 
-- First we generate a cleaned expression without any invalid term w.r.t. the training data
-- Then 
fitnessReg :: [Vector] -> Vector -> Expr Double -> Solution Double RegStats
fitnessReg xss ys expr = let (expr', zss) = cleanExpr expr xss
                             zss'         = map V.fromList zss
                             (ws, r2)     = olsRegress zss' ys
                             ysHat        = predict zss' ws
                             rs           = RS (rmse ysHat ys) (mae ysHat ys) (nmse ysHat ys) r2 ws
                             inf          = 1/0
                             rs'          = RS inf inf inf inf (V.singleton 0.0)
                          in case expr' of
                               Zero -> Sol expr  (_rmse rs') rs'
                               _    -> Sol expr' (_rmse rs) rs
