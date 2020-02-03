{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
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

import Control.Exception

import IT
import IT.Algorithms

import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as LA
import qualified MachineLearning as ML
import qualified MachineLearning.Regression as MLR

import Data.List (foldl1', foldl')

import Control.Parallel.Strategies
import Control.DeepSeq

type Vector = LA.Vector Double

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

  evalInter inter xs = Reg $ parseInter inter xs 1
    where
      parseInter One   _  !r = r
      parseInter (e `Times` i) (y:ys) !r = parseInter i ys (r * pow y e)

      pow x e = if e >= 0 then x^e else recip (x^(-e))

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
meanError op ysHat ys = mean $ V.map op $ ysHat - ys

-- | Common error measures for regression: 
-- MSE, MAE, RMSE, NMSE, r^2
mse           = meanError (^2)
mae           = meanError abs
nmse ysHat ys = mse ysHat ys / var ys
rmse ysHat ys = sqrt $ mse ysHat ys
rSq ysHat ys  = 1 - r/t
  where
    ym      = mean ys
    t       = sumOfSq $ V.map (\yi -> yi - ym) ys
    r       = sumOfSq $ ys - ysHat
    sumOfSq = V.foldl (\s di -> s + di^2) 0

-- | Predict a linear model
predict :: LA.Matrix Double -> Vector -> Vector
predict = MLR.hypothesis MLR.LeastSquares 
    
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

instance NFData RegStats where
  rnf a = ()
  
-- * Utility functions
    
-- | Clean the expression removing invalid terms w.r.t. the training data
-- it may return an empty expression
isInvalid :: Double -> Bool
isInvalid x = isNaN x || isInfinite x

isValid :: [Double] -> Bool
isValid xs = all (not.isInvalid) xs -- && (maximum xs < 1e6) -- var > 1e-4 &&
  where
    mu  = sum(xs) / n
    var = (*(1/n)) . sum $ map (\x -> (x-mu)*(x-mu)) xs
    n   = fromIntegral (length xs)

exprToMatrix :: [[Double]] -> Expr Double -> LA.Matrix Double
exprToMatrix xss e = (ML.addBiasDimension . LA.tr . LA.fromLists) $ filter isValid zss
  where
    zss = mapterms e
    mapterms Zero          = []
    mapterms (t `Plus` e') = zs : mapterms e'
      where zs = map (_unReg . evalTerm @Regression t) xss
    
cleanExpr :: [[Double]] -> Expr Double -> Expr Double
cleanExpr _    Zero        = Zero
cleanExpr xss (t `Plus` e) = if (any isInvalid . map evalT) xss
                             then cleanExpr xss e
                             else t `Plus` cleanExpr xss e
  where
    evalT = _unReg . evalTerm @Regression t
  
notInfNan :: Solution Double RegStats -> Bool
notInfNan s = not (isInfinite f || isNaN f)
  where f = _fit s

parMapChunk :: Int -> (Expr Double -> LA.Matrix Double) -> [Expr Double] -> [LA.Matrix Double]
parMapChunk n f xs = map f xs `using` parListChunk n rdeepseq

-- | Fitness function for regression
-- 
-- First we generate a cleaned expression without any invalid term w.r.t. the training data
-- Then 
fitnessReg :: [[Double]] -> Vector -> [Expr Double] -> Population Double RegStats
fitnessReg xss ys exprs = let zs = parMapChunk 100 (exprToMatrix xss) exprs
                          in  filter notInfNan $ zipWith (regress xss ys) exprs zs

fitnessTest :: [[Double]] -> Vector -> Solution Double RegStats -> RegStats
fitnessTest xss ys sol = let zs = exprToMatrix xss (_expr sol)
                             ws = (_weights . _stat) sol
                             ysHat = predict zs ws 
                             rs    = RS (rmse ysHat ys) (mae ysHat ys) (nmse ysHat ys) (rSq ysHat ys) ws
                             inf   = 1/0
                          in if V.length ws == LA.cols zs
                             then rs
                             else RS inf inf inf inf (V.singleton 0.0)
                             

regress :: [[Double]] -> Vector -> Expr Double -> LA.Matrix Double -> Solution Double RegStats
regress xss ys expr zss 
  | LA.rows zss == 0 = let rs  = RS inf inf inf inf (V.singleton 0.0)
                           inf = 1/0
                       in  Sol expr (_rmse rs ) rs
  | otherwise        = let ws    = MLR.normalEquation_p zss ys 
                           ysHat = predict zss ws 
                           rs    = RS (rmse ysHat ys) (mae ysHat ys) (nmse ysHat ys) (rSq ysHat ys) ws
                       in  Sol expr (_rmse rs ) rs
