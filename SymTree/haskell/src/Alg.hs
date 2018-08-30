{- |
Module      :  $Header$
Description :  Greedy Tree Search for Symbolic Regression (SymTree)
Copyright   :  (c) Fabrício Olivetti de França, 2017
License     :  GPL-3

Maintainer  :  fabricio.olivetti@gmail.com
Stability   :  experimental
Portability :  portable

TODO: test Data.Set instead of using nub
-}

module Alg where

import Numeric.LinearAlgebra
import Eval
import qualified Data.List as List
import qualified Data.Set as Set

data Op = OnlyPos | OnlyInter | AllOp deriving (Eq)
data Params = Params {getInter :: Int, getInv :: Int, getTran :: Int, getThr :: Double}

type Expansion = ([Expr], Terms)
type Tabu = Set.Set Terms

flatten' list = foldr (++) [] list 

notAllZeros :: Poly -> Bool
-- check if the polynomial has at least one nonzero exponent
notAllZeros poly = nElems /= numZeros
  where
    numZeros = length [p | p <- poly, p==0]
    nElems   = length poly

genInitialSolution :: Integer -> DataPoints -> Expr
-- generate initial solution of dimension n
genInitialSolution n points = Expr (terms, error)
  where
    terms         = Set.fromList [genIdTerm i  n | i <- [1..n]]
    error         = evalExpr terms points

    genIdTerm i n = (genIdPoly i n, 1)
    genIdPoly i n = [oneZero j i | j <- [1..n]]
    oneZero   j i
        | j==i      = 1
        | otherwise = 0
        
transformations :: Terms -> Terms
-- generate all transformations available for a given expression
transformations terms  = Set.unions setTerms
  where
    setTerms  = Set.toList $ Set.map (Set.fromList . transform) terms
    transform (poly, fId) = [(poly, fId') | fId' <- fIdList, fId' /= fId]

interactions :: Terms -> Op -> Terms
-- generate all interactions available of a given expression
interactions terms op
    | op == OnlyPos = Set.fromList pos
    | otherwise     = Set.fromList $ pos ++ neg
  where
    pos       = [(sumPoly p1 p2, f) | (p1,p2,f) <- combTerms]
    neg       = [(subPoly p1 p2, f) | (p1,p2,f) <- combTerms]
    combTerms = [(p1, p2, f1) | (p1,f1) <- lTerms, (p2,f2) <- lTerms, f1==f2]
    lTerms    = Set.toList terms
    sumPoly p1 p2 = map (\(x, y) -> x+y) $ zip p1 p2
    subPoly p1 p2 = map (\(x, y) -> x-y) $ zip p1 p2

insertTerms :: Expr -> Terms -> Terms -> DataPoints 
               -> (Expr, Terms)
-- insert each term of a list sequentially if it reduces the mse of the 
-- current expression, returns a tuple containing the new expression
-- and the unused terms.
insertTerms expr candTerms unused points 
    | Set.null candTerms  = (expr, unused)
    | improvedMse = insertTerms newExpr ts unused points
    | otherwise   = insertTerms expr ts newUnused points 
    where
      improvedMse = newMSE < mse
      newMSE = evalExpr newTerms points
      newExpr = Expr (newTerms, newMSE)
      newTerms = Set.insert newTerm terms
      newUnused = Set.insert newTerm unused
      terms     = getTerms expr
      mse       = getMSE expr
      newTerm   = head $ termsList
      ts        = Set.fromList $ tail termsList
      termsList = Set.elems candTerms

simplifyExpr :: Double -> Expr -> DataPoints -> Expr
-- simplify an expression removing irrelevant terms
simplifyExpr thr expr points = Expr (simplifiedList, newMSE)
  where
    newMSE = evalExpr simplifiedList points
    simplifiedList = Set.filter (notAllZeros . fst) improvedTerms
    improvedTerms  = Set.fromList [Set.elemAt i terms | i <- indices]
    terms     = getTerms expr
    indices   = find (>thr) (abs coefs)
    coefs     = subVector 1 (size coefList - 1) coefList
    coefList  = flatten coefMtx 
    coefMtx   = solveLR terms points
    
expansionList :: Expr -> Terms -> DataPoints -> Double -> [Expr]
-- generates a list of expanded expressions
expansionList expr terms points thr
    | Set.null terms  = []
    | unused == terms = []
    | otherwise  = simplifiedExpr : next 
      where
        simplifiedExpr    = simplifyExpr thr newExpr points
        (newExpr, unused) = insertTerms expr terms Set.empty points
        next              = expansionList expr unused points thr
    
getBestExpr :: [Expr] -> Expr
-- returns the best expression and its mse
getBestExpr exprs = minExpr exprs
  where
    minExpr []       = error "Empty list"
    minExpr [x]      = x
    minExpr (x:y:xs)
        | noDiff      = if lenX < lenY then minExpr (x:xs) else minExpr (y:xs)
        | mseX < mseY = minExpr (x:xs)
        | otherwise   = minExpr (y:xs)
          where
            noDiff = abs (mseX - mseY) < 1e-15
            mseX = getMSE x
            mseY = getMSE y
            lenX = length (getTerms x)
            lenY = length (getTerms y)

filterPositiveGains :: Expr -> Terms -> DataPoints -> Terms
-- generate candidate terms that improve the current solution 
filterPositiveGains (Expr (terms, mse)) candTerms (x,y) = filteredTerms
  where
    filteredTerms  = Set.filter improved candTerms
    improved term  = (newMSE (transform term))  <= mse
    newMSE newX    = calcMSE (newX ||| xt) y
    xt             = transformData terms x
    transform term = (rows xt >< 1) $ evalTerm term x

getCandidates :: Op -> Expr -> Terms
-- return the candidate terms with restricted operations
getCandidates AllOp (Expr (terms, mse)) = Set.union allInter allTran
  where
    allInter = interactions terms OnlyInter
    allTran  = transformations terms
getCandidates op (Expr (terms, mse)) = interactions terms op

expandExpr :: DataPoints -> Double -> (Expr -> Terms) -> [Expr]
              -> [Expr]
-- expand each expression from a list of expressions
expandExpr points thr getcand exprs = newExprs
  where
    newExprs = flatten' $ filter nonempty expandedExprs
        
    expandedExprs = map genExpansion zippedExpCand
    genExpansion (expr, cand) = expansionList expr cand points thr
        
    zippedExpCand      = zip exprs candList
    nonempty           = not . null

    candList           = [posGain expr $ getcand expr | expr <- exprs]
    posGain expr terms = filterPositiveGains expr terms points

iterExpansion :: ([Expr] -> [Expr]) -> [Expr] -> Tabu -> Int -> ([Expr], Tabu)
-- expand the current expression list and returns a new 
-- expression list with an updated tabu list
iterExpansion expand exprs tabu iter
    | (iter == 0) || (bestExprMSE < 1e-20) || (bestExprMSE < bestNewExprMSE) || (null newExprs) = (exprs, tabu)
    | otherwise = iterExpansion expand newExprs newTabu newIter
      where
        newTabu  = Set.union tabu newTabuList
        newTabuList = Set.fromList [getTerms expr | expr <- newExprs]
        newExprs = [expr | expr <- expand exprs, Set.notMember (getTerms expr) tabu]
        newIter  = iter - 1
        bestExprMSE    = getMSE $ getBestExpr exprs
        bestNewExprMSE = getMSE $ getBestExpr newExprs


runSymTree :: DataPoints -> Params -> Expr
-- run the SymTree algorithm
runSymTree points params = getBestExpr finalExprs
  where
    (finalExprs, _) = iterExpansion fnAllCand sndIt tabu2 nTran

    (sndIt, tabu2)  = iterExpansion fnAllInter fstIt tabu1 nInv
    (fstIt, tabu1)  = iterExpansion fnOnlyPos iExpr Set.empty nInter
    
    fnAllCand  = expandExpr points thr allCandidates
    fnAllInter = expandExpr points thr interCandidates
    fnOnlyPos  = expandExpr points thr posCandidates
   
    nInter  = getInter params
    nInv    = getInv params
    nTran   = getTran params
    thr     = getThr params
    iExpr   = [genInitialSolution dim points]
    dim     = toInteger (cols $ fst points)
    
    posCandidates = getCandidates OnlyPos
    interCandidates = getCandidates OnlyInter
    allCandidates = getCandidates AllOp
