{- |
Module      :  $Header$
Description :  types and evaluation functions for a symbolic expression
Copyright   :  (c) Fabrício Olivetti de França, 2017
License     :  GPL-3

Maintainer  :  fabricio.olivetti@gmail.com
Stability   :  experimental
Portability :  portable

The types used and evaluation functions of the
Greedy Tree Search for Symbolic Regression (SymTree).
-}

module Eval where

import Numeric.LinearAlgebra
import qualified Data.Set as Set

-- types used to compose an expression
type Poly  = [Double] -- Matrix Double
type FunId = Int
type Term  = (Poly, FunId)
type Terms = Set.Set Term
type MSE   = Double

-- types representing data points 
type X          = Matrix Double
type Y          = Matrix Double
type Coefs      = Matrix Double
type DataPoints = (X, Y)

-- Data type representing the expression
newtype Expr  = Expr (Terms, MSE) deriving (Eq, Ord, Show)

getMSE :: Expr -> Double
getMSE (Expr (terms, mse)) = mse

getTerms :: Expr -> Terms
getTerms (Expr (terms, mse)) = terms

-- nan and inf representation
nan = 0/0 :: Double
inf = 1/0 :: Double

-- evaluation functions, change here to include more functions
-- TODO: change to hash table
maxId = 7
names = ["undefined", "id", "sin", "cos", "tan", "sqrt", "log", "log1p"]

funName :: FunId -> String
funName id = names !! id

fIdList = [1..maxId] :: [Int]

applyFun :: FunId -> Double -> Double
applyFun fId x
    | fId == 1 = id x 
    | fId == 2 = sin x 
    | fId == 3 = cos x 
    | fId == 4 = tan x
    | fId == 5 = sqrt (abs x)
    | fId == 6 = log x
    | fId == 7 = log (x+1.0)   

evalTerm :: Term -> X ->  [Double] -- Vector
-- apply the function to the polynomial expression of a term
evalTerm (poly, fId) x =  polyVec
  where
    polyVec = map funProd polyX
    polyX   = toRows $ x**(fromLists [poly])
    funProd = (applyFun fId)  . prodElements

transformData :: Terms -> X -> X
-- transform the variables as instructed by the expression
transformData terms x = tr $ fromLists transformedData
  where
    transformedData = oneVec : [evalTerm term x | term <- Set.toList terms]
    oneVec          = [1.0 | i <- [1..rows x]]

evalExpr :: Terms -> DataPoints -> Double
evalExpr terms points = calcMSE x y
  where
    x = transformData terms $ fst points
    y = snd points

solveLR :: Terms -> DataPoints -> Coefs
-- solve a linear regression given an expression
solveLR terms points = linearSolveSVD genX genY
  where
    genX       = transformData terms (fst points)
    genY       = snd points

solveRidge :: Matrix Double -> Matrix Double -> Matrix Double
-- Ridge regression, just testing...
solveRidge a b = oA <\> oB
  where
   mu = 0.1

   oA = (a <> (tr a)) + (mu * (ident $ rows a))
   oB = a <> (tr b)

safeLinearSolver :: X -> Y -> Maybe Coefs
-- safe linear regression that returns Nothing if X contains NaN
safeLinearSolver x y
    | anyNanInf = Nothing
    | otherwise = Just (linearSolveSVD x y) -- linearSolveSVD
      where
        anyNanInf = anyNan || anyInf
        anyNan    = sumX /= sumX
        anyInf    = countInfs > 0
        sumX      = sumElements x
        countInfs = length $ find (==inf) (abs x)
      
calcMSE :: X -> Y -> Double
-- calculates Mean Squared Error
calcMSE x y = case linearReg  of
    Nothing -> inf
    Just coefs  -> (sumSq coefs) / npoints
    where
      linearReg   = safeLinearSolver trainX trainY
      sumSq coefs = sumElements $ err coefs ^ 2
      npoints     = fromIntegral $ rows x
      
      err coefs   = fhat coefs - testY
      fhat coefs  = testX <> coefs

      trainX      = subMatrix (0, 0) (halfX, cols x) x
      trainY      = subMatrix (0, 0) (halfY, cols y) y
      testX       = subMatrix (halfX, 0) (halfX, cols x) x
      testY       = subMatrix (halfY, 0) (halfY, cols y) y     
      halfX       = (rows x) `div` 2
      halfY       = (rows y) `div` 2
