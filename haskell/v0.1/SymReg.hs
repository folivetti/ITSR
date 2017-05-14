{- |
Module      :  $Header$
Description :  main command line for Recursive Symbolic Regression
Copyright   :  (c) Fabrício Olivetti de França, 2017
License     :  GPL-3

Maintainer  :  fabricio.olivetti@gmail.com
Stability   :  experimental
Portability :  portable

The main module for Recursive Symbolic Regression algorithm. 
The algorithm takes as input five parameters:

filename    :  name of the file containing the data to be fit
intStepsArg :  # of steps performing only positive interactions
invStepsArg :  # of steps performing only positive and negative interactions
tranStepsArg:  # of steps performing interactions and transformations
thrArg      :  used to remove terms with coefficient smaller than this value
-}

module Main where

import System.Environment
import Text.Printf
import Numeric.LinearAlgebra
import Data.List hiding (find)

import Eval
import Alg

getDataPoints :: (Matrix Double) -> DataPoints
-- create DataPoints from a matrix
getDataPoints dat = (x,y)
  where
    x = subMatrix (0, 0) (rows dat, cols dat - 1) dat
    y = subMatrix (0, cols dat - 1) (rows dat, 1) dat


polyToStr :: Poly -> String
polyToStr poly  = intercalate " * " polyString
  where
    polyString       = [ expString i e | (i,e) <- indexedPolys, e/=0.0]
    expString i e = "x_" ++ (show i) ++ "^" ++ (show e) 
    indexedPolys     = zip [1..] listPolys
    listPolys        = (toLists poly) !! 0

-- create a String representing a given expression and its coefficients
exprToStr :: Expr -> [Double] -> Double -> String
exprToStr expr (bias:coefs) thr
    | bias > thr = (show bias) ++ " + " ++ exprStr
    | otherwise  = exprStr
  where
    exprStr     = intercalate " + " formatExpr
    formatExpr = [printf "%0.2f*%s(%s)" c f p | (c, f, p) <- names, c>thr]    
    names      = [(coef, (funName f), (polyToStr p)) | ((p,f), coef) <- zExpr]
    zExpr      = zip expr coefs

main = do

  args <- getArgs
  case args of
    [fName, intStepsArg, invStepsArg, tranStepsArg, thrArg] -> do
        dat <- loadMatrix fName

        let
          -- arguments parsing
          nInter = read intStepsArg :: Int
          nInv   = read invStepsArg :: Int
          nTran  = read tranStepsArg :: Int
          thr    = read thrArg :: Double

          -- initial solution
          dataPts = getDataPoints dat
          dim     = toInteger (cols dat - 1)
          iExpr   = [genInitialSolution dim]

          -- execute main algorithm
          (fstIt, tabu1) = (repExpand iExpr dataPts nInter OnlyPos thr [])
          (sndIt, tabu2) = (repExpand fstIt dataPts nInv OnlyInter thr tabu1)
          (final, _)     = (repExpand sndIt dataPts nTran AllOp thr tabu2)

          -- get best expression and the coefficients
          mseList             = map (\expr -> mse expr dataPts) final
          (bestExpr, bestMSE) = getBestExpr final mseList
          bestCoefs           = solveLR bestExpr dataPts
          listCoefs           = (toLists (tr bestCoefs)) !! 0
          nterms              = show (length (find (>thr) bestCoefs))

        printf "\nMSE: %e, nterms: %s\nFinal expression: " bestMSE nterms
        print (exprToStr bestExpr listCoefs thr)
        printf "\n"
  
    _ -> putStrLn "Usage: ./symreg interactionsSteps inverseSteps transformationSteps thr"
