{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DatatypeContexts #-}
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

import Data.Coerce
import Data.Foldable
import Data.Semigroup

import Control.Exception
import GHC.Conc (numCapabilities)

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
newtype Floating a => Regression a = Reg {_unReg :: a}
                         deriving (Num, Floating, Fractional)

-- | Simply shows the packed 'Double'
instance (Show a, Floating a) => Show (Regression a) where
  show (Reg x) = show x

instance (NFData a, Floating a) => NFData (Regression a) where
  rnf a = ()
  
-- | IT Instance for regression evals an expression to
-- sum(w_i * term_i) + w0
-- with
-- term_i = f_i(p_i(xs))
-- and p_i(xs) = prod(x^eij)
instance Floating a => IT (Regression a) where
  itTimes xs (Strength is) = product $ zipWith pow xs is
    where
      pow x i = if i<0
                then recip (x^(abs i))
                else x^i
    
  itAdd   = itAddDefault Sum
  itWeight x (Reg y) = Reg (realToFrac x * y)

-- | Transformation Functions
regSin, regCos, regTan, regTanh, regSqrt, regAbsSqrt, regLog, regExp :: Floating a => Transformation (Regression a)
regSin     = Transformation "sin" sin
regCos     = Transformation "cos" cos
regTan     = Transformation "tan" tan
regTanh    = Transformation "tanh" tanh

regSqrt    = Transformation "sqrt" sqrt
regAbsSqrt = Transformation "sqrt.abs" (sqrt.abs)
regLog     = Transformation "log" log
regExp     = Transformation "exp" exp

regTrig      = [regSin, regCos, regTan, regTanh]
regNonLinear = [regSqrt, regAbsSqrt, regLog, regExp]
regLinear    = [Transformation "id" id]

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
isValid xs = all (not.isInvalid) xs && (maximum (map abs xs) < 1e150)  -- && var > 1e-4
  where
    mu  = sum(xs) / n
    var = (*(1/n)) . sum $ map (\x -> (x-mu)*(x-mu)) xs
    n   = fromIntegral (length xs)

exprToMatrix :: [[Regression Double]] -> Expr (Regression Double) -> LA.Matrix Double
exprToMatrix rss (Expr e) = (ML.addBiasDimension . LA.tr . LA.fromLists) zss -- $ filter isValid zss
  where
    zss = map (\t -> map (_unReg . evalTerm t) rss) e
    
cleanExpr :: [[Regression Double]] -> Expr (Regression Double) -> Expr (Regression Double)
cleanExpr rss (Expr e) = Expr (cleanExpr' e)
  where
    cleanExpr' (t:e) = if (any isInvalid . map (evalT t)) rss
                       then cleanExpr' e
                       else t : cleanExpr' e
    evalT t = _unReg . evalTerm t
  
notInfNan :: Solution (Regression Double) RegStats -> Bool
notInfNan s = not (isInfinite f || isNaN f)
  where f = _fit s

parMapChunk :: Int -> (Expr (Regression Double) -> LA.Matrix Double) -> [Expr (Regression Double)] -> [LA.Matrix Double]
parMapChunk n f xs = map f xs `using` parListChunk n rpar -- rpar or rdeepseq

-- | Fitness function for regression
-- 
-- First we generate a cleaned expression without any invalid term w.r.t. the training data
-- Then 
fitnessReg :: Int -> [[Regression Double]] -> Vector -> [Expr (Regression Double)] -> Population (Regression Double) RegStats
fitnessReg nPop xss ys exprs = let n  = nPop `div` (2*numCapabilities)
                                   zs = parMapChunk n (exprToMatrix xss) exprs
                                   ps = zipWith (regress ys) exprs zs
                               in  filter notInfNan ps -- map notInfNan ps `deepseq` ps -- filter notInfNan ps

fitnessTest :: [[Regression Double]] -> Vector -> Solution (Regression Double) RegStats -> RegStats
fitnessTest xss ys sol = let zs = exprToMatrix xss (_expr sol)
                             ws = (_weights . _stat) sol
                             ysHat = predict zs ws 
                             rs    = RS (rmse ysHat ys) (mae ysHat ys) (nmse ysHat ys) (rSq ysHat ys) ws
                             inf   = 1/0
                          in if V.length ws == LA.cols zs
                             then rs
                             else RS inf inf inf inf (V.singleton 0.0)
                             

regress :: Vector -> Expr (Regression Double) -> LA.Matrix Double -> Solution (Regression Double) RegStats
regress ys expr zss 
  | LA.rows zss == 0 = let rs  = RS inf inf inf inf (V.singleton 0.0)
                           inf = 1/0
                       in  Sol expr (_rmse rs ) rs
  | any (not.isValid) (LA.toLists zss) = let rs  = RS inf inf inf inf (V.singleton 0.0)
                                             inf = 1/0
                                         in  Sol expr (_rmse rs ) rs
  | otherwise        = let ws    = LA.flatten $ LA.linearSolveSVD zss (LA.asColumn ys)  -- MLR.normalEquation_p
                           ysHat = predict zss ws 
                           rs    = RS (rmse ysHat ys) (mae ysHat ys) (nmse ysHat ys) (rSq ysHat ys) ws
                       in  Sol expr (_rmse rs ) rs
