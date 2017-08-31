{- |
Module      :  $Header$
Description :  Recursive Symbolic Regression algorithm
Copyright   :  (c) Fabrício Olivetti de França, 2017
License     :  GPL-3

Maintainer  :  fabricio.olivetti@gmail.com
Stability   :  experimental
Portability :  portable
-}

module Alg where

import Numeric.LinearAlgebra
import Eval
import Data.List hiding (find)

data Op = OnlyPos | OnlyInter | AllOp deriving (Eq)

flatten' list = foldr (++) [] list 

notAllZeros :: Poly -> Bool
-- check if the polynomial has at least one nonzero exponent
notAllZeros poly = (nrows*ncols) /= numZeros
  where
    numZeros = length $ find (==0) poly
    nrows    = rows poly
    ncols    = cols poly

genInitialSolution :: Integer -> Expr
-- generate initial solution of dimension n
genInitialSolution n = [genIdTerm i  n | i <- [1..n]]
  where
    genIdTerm i n = (genIdPoly i n, 1)
    genIdPoly i n = fromLists [[oneZero j i | j <- [1..n]]]
    oneZero   j i
        | j==i      = 1
        | otherwise = 0

transformations :: Expr -> [Term]
-- generate all transformations available for a given expression
transformations expr = flatten' [transform term | term <- expr]
  where
    transform (poly, fId) = [ (poly, fId') |  fId' <- [1..maxId], fId' /= fId]

interactions :: Expr -> Op 
                -> [Term]
-- generate all interactions available of a given expression
interactions terms op
    | op == OnlyPos = nub pos           -- (nub = unique elements)
    | otherwise     = nub $ pos ++ neg
  where
    pos = [ (p1 + p2, f1) | (p1, f1) <- terms, (p2, f2) <- terms, f1 == f2]
    neg = [ (p1 - p2, f1) | (p1, f1) <- terms, (p2, f2) <- terms, f1 == f2]

genCandidates :: Expr -> DataPoints -> [Term] 
                 -> [Term] 
-- generate candidate terms that improve the current solution 
genCandidates expr points terms = candidates
  where
    candidates = filter (\term -> improvedMse term) terms
    improvedMse term = (mse (term : expr) points) < currentMSE
    currentMSE       = mse expr points

insertTerms :: Expr -> [Term] -> [Term] -> DataPoints 
               -> (Expr, [Term])
-- insert each term of a list sequentially if it reduces the mse of the 
-- current expression, returns a tuple containing the new expression
-- and the unused terms.
insertTerms expr [] unused points = (expr, unused)    
insertTerms expr (term:terms) unused points
    | expr == unused = (expr, unused)
    | improvedMse term = insertTerms (term : expr) terms unused points
    | otherwise        = insertTerms expr terms (term : unused) points 
    where
      improvedMse term = mse (term:expr) points < currentMSE
      currentMSE       = mse expr points

simplify :: Expr -> DataPoints -> Double 
            -> Expr
-- simplify an expression removing irrelevant terms
simplify expr points thr = nub simplifiedList
  where
    simplifiedList = filter (notAllZeros . fst) improvedTerms
    improvedTerms  = [expr !! i | i <- indices] --[term | (term, coef) <- termCoefs, abs coef > thr]
    --termCoefs = zip expr coefs
    indices   = find (>thr) (abs coefs)
    coefs     = subVector 1 (size coefList - 1) coefList -- remove the bias coefficient
    coefList  =  flatten coefMtx 
    coefMtx   = solveLR expr points

expandExpr :: Expr -> [Term] -> DataPoints -> Double 
              -> [Expr]
-- expand an expression generating a list of expressions
expandExpr expr [] points thr = []
expandExpr expr terms points thr = newExpr : next 
  where    
    newExpr  = simplify (fst inserted) points thr
    excluded = snd inserted
    inserted = insertTerms expr terms [] points
    next     = expandExpr expr excluded points thr

expand :: [Expr] -> [Term] -> DataPoints -> Op -> Double
          -> ([Expr], [Term])
-- apply the expression expansion to each expression, 
-- returning a tuple of the new expressions and the tabu list
expand exprs tabu points op thr = (expanded, newTabu)
  where
    expanded      = flatten' newExprs
    newTabu       = tabu ++ (flatten' newCandidates)
    
    newExprs      = filter nonempty expandedExprs
    expandedExprs = [expandExpr expr cand points thr | (expr,cand) <- zExpCand]
    zExpCand      = zip exprs newCandidates
    nonempty      = not . null
    newCandidates = [filterTabu $ filteredCandidates expr | expr <- exprs]
    
    filteredCandidates expr = genCandidates expr points (candidates expr op)
    filterTabu terms        = [term | term <- terms, not (term `elem` tabu)]
    candidates expr AllOp   = (interactions expr op) ++ (transformations expr)
    candidates expr op      = interactions expr op
        


getBestExpr :: [Expr] -> [Double] 
               -> (Expr, Double)
-- returns the best expression and its mse
getBestExpr exprs mseList = minExpr $ zip exprs mseList
  where
    minExpr []       = error "Empty list"
    minExpr [x]      = x
    minExpr (x:y:xs)
        | (snd x) < (snd y) = minExpr (x:xs)
        | otherwise         = minExpr (y:xs)

repExpand :: [Expr] -> DataPoints -> Int -> Op -> Double -> [Term]
             -> ([Expr], [Term])
-- apply the expansion function n times
repExpand exprs points 0 op thr tabu = (exprs, tabu)
repExpand exprs points n op thr tabu
    | bestExprMse < 1e-20 = (exprs, tabu)
    | null expansion      = (exprs, tabu)
    | otherwise           = repExpand expansion points (n-1) op thr newTabu
      where
        (expansion, newTabu) = expand exprs tabu points op thr
        bestExprMse = snd (getBestExpr exprs mseList)
        mseList = [mse expr points | expr <- exprs]
