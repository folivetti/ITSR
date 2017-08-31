{- |
Module      :  $Header$
Description :  types and evaluation functions for a symbolic expression
Copyright   :  (c) Fabrício Olivetti de França, 2017
License     :  GPL-3

Maintainer  :  fabricio.olivetti@gmail.com
Stability   :  experimental
Portability :  portable

The types used and evaluation functions of the
Recursive Symbolic Regression algorithm.
-}

module Eval where

import Numeric.LinearAlgebra

-- types used to compose an expression
type Poly  = Matrix Double
type FunId = Int
type Term  = (Poly, FunId)
type Expr  = [Term]

-- types representing data points 
type X          = Matrix Double
type Y          = Matrix Double
type Coefs      = Matrix Double
type DataPoints = (X, Y)

-- nan and inf representation
nan = 0/0 :: Double
inf = 1/0 :: Double

-- evaluation functions, change here to include more functions
-- TODO: change to hash table
maxId = 7
names = ["undefined", "id", "sin", "cos", "tan", "sqrt", "log", "log1p"]

applyFun :: FunId -> Double -> Double
applyFun fId x
    | fId == 1 = id x 
    | fId == 2 = sin x 
    | fId == 3 = cos x 
    | fId == 4 = tan x
    | fId == 5 = sqrt (abs x)
    | fId == 6 = log x
    | fId == 7 = log (x+1)
    
funName :: FunId -> String
funName id
    | id <= maxId = names !! id
    | otherwise = "undefined"
    

evalTerm :: Term -> X ->  [Double] -- Vector
-- apply the function to the polynomial expression of a term
evalTerm (poly, fId) x =  polyVec
  where
    polyVec = map funProd polyX
    polyX   = toRows $ x**poly
    funProd = (applyFun fId)  . prodElements

transformData :: Expr -> X -> X
-- transform the variables as instructed by the expression
transformData expr x = tr $ fromLists transformedData
  where
    transformedData = oneVec : [evalTerm term x | term <- expr]
    oneVec          = [1.0 | i <- [1..rows x]]

solveLR :: Expr -> DataPoints -> Coefs
-- solve a linear regression given an expression
solveLR expr points = linearSolveSVD genX genY
  where
    genX       = transformData expr (fst points)
    genY       = snd points

safeLinearSolver :: X -> Y -> Maybe Coefs
-- safe linear regression that returns Nothing if X contains NaN
safeLinearSolver x y
    | anyNanInf = Nothing
    | otherwise = Just (linearSolveSVD x y)
      where
        anyNanInf = anyNan || anyInf
        anyNan    = sumX /= sumX
        anyInf    = countInfs > 0
        sumX      = sumElements x
        countInfs = length $ find (==inf) (abs x)
      
mse :: Expr -> DataPoints -> Double
-- calculates Mean Squared Error
mse expr points = case linearReg  of
    Nothing -> inf
    Just coefs  -> (sumSq coefs) / npoints
    where
      linearReg   = safeLinearSolver trainX trainY
      sumSq coefs = sumElements $ err coefs ^ 2
      npoints     = (fromIntegral . length) points
      
      err coefs   = fhat coefs - testY
      fhat coefs  = testX <> coefs

      trainX      = subMatrix (0, 0) (halfX, cols genX) genX
      trainY      = subMatrix (0, 0) (halfY, cols genY) genY
      testX       = subMatrix (halfX, 0) (halfX, cols genX) genX
      testY       = subMatrix (halfY, 0) (halfY, cols genY) genY     
      genX        = transformData expr (fst points)
      genY        = snd points
      halfX       = (rows genX) `div` 2
      halfY       = (rows genY) `div` 2
