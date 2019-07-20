{- |
Module      :  $Header$
Description :  main command line SymTree algorithm
Copyright   :  (c) Fabrício Olivetti de França, 2017
License     :  GPL-3

Maintainer  :  fabricio.olivetti@gmail.com
Stability   :  experimental
Portability :  portable

The main module for Greedy Tree Search for Symbolic Regression (SymTree). 
The algorithm takes as input five parameters:

filename    :  name of the file containing the data to be fit
intStepsArg :  # of steps performing only positive interactions
invStepsArg :  # of steps performing only positive and negative interactions
tranStepsArg:  # of steps performing interactions and transformations
thrArg      :  used to remove terms with coefficient smaller than this value
-}

module Main where

import Prelude hiding ((<>))
import System.Environment
import Text.Printf
import Numeric.LinearAlgebra
import qualified Data.List as List
import qualified Data.Set as Set

import Eval
import Alg

getDataPoints :: (Matrix Double) -> DataPoints
-- create DataPoints from a matrix
getDataPoints dat = (x,y)
  where
    x = subMatrix (0, 0) (rows dat, cols dat - 1) dat
    y = subMatrix (0, cols dat - 1) (rows dat, 1) dat


polyToStr :: Poly -> String
polyToStr poly  = List.intercalate " * " polyString
  where
    polyString       = [ expString i e | (i,e) <- indexedPolys, e/=0.0]
    expString i e = "x_" ++ (show i) ++ "^" ++ (show e) 
    indexedPolys     = zip [1..] poly

-- create a String representing a given expression and its coefficients
exprToStr :: Terms -> [Double] -> Double -> String
exprToStr terms (bias:coefs) thr
    | bias > thr = (show bias) ++ " + " ++ exprStr
    | otherwise  = exprStr
  where
    exprStr     = List.intercalate " + " formatExpr
    formatExpr = [printf "%0.6f*%s(%s)" c f p | (c, f, p) <- names]    
    names      = [(coef, (funName f), (polyToStr p)) | ((p,f), coef) <- zExpr]
    zExpr      = zip (Set.toList terms) coefs

main = do

  args <- getArgs
  case args of
    [fName, fTest, intStepsArg, invStepsArg, tranStepsArg, thrArg] -> do
        dat <- loadMatrix fName
        tst <- loadMatrix fTest

        let
          -- arguments parsing
          nInter = read intStepsArg :: Int
          nInv   = read invStepsArg :: Int
          nTran  = read tranStepsArg :: Int
          thr    = read thrArg :: Double

          -- create data points
          dataPts = getDataPoints dat
          testPts = getDataPoints tst

          -- execute main algorithm
          bestExpr = runSymTree dataPts (Params nInter nInv nTran thr)

          -- get best expression and the coefficients
          bestTerms = getTerms bestExpr
          bestMSE   = getMSE bestExpr
          bestCoefs           = solveLR bestTerms dataPts
          listCoefs           = (toLists (tr bestCoefs)) !! 0
          nterms              = show (length (find (>thr) (abs bestCoefs)))
          
          transTest = transformData bestTerms (fst testPts)
          fhat      = transTest <> bestCoefs
          err       = fhat - (snd testPts)
          sumSq     = sumElements $ err ^ 2
          sumAbs    = sumElements $ abs err
          mse       = sumSq  / (fromIntegral $ rows (fst testPts))
          mae       = sumAbs / (fromIntegral $ rows (fst testPts))

        printf "\nMSE: %e, nterms: %s\nFinal expression: " bestMSE nterms
        print (exprToStr bestTerms listCoefs thr)
        printf "\n"
        
        printf "\nMSE test data: %e\n" (sqrt mse)
        printf "\nMAE test data: %.4f\n" mae
  
    _ -> putStrLn "Usage: ./symtree trainFile testFile interactionsSteps inverseSteps transformationSteps thr"
