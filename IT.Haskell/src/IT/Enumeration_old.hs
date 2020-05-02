module IT.Enumeration where

import IT
import IT.Regression (Regression(Reg,_unReg))

import qualified Numeric.LinearAlgebra as LA
import Data.List
import Data.Ord

type Vector = LA.Vector Double
type Matrix = LA.Matrix Double

combinations :: [Double] -> Int -> Int -> [[Double]]
combinations exps dim n_inters = go dim n_inters
  where
    nonzeros d n = concatMap (\x -> map (x:) $ go d n) exps
    zeros d n    = map (0:) (go d n)
    
    go 0 n = [[]]
    go d 0 = map (0:) (go (d-1) 0)
    go d n | d == n    = nonzeros (d-1) (n-1)
           | otherwise = zeros (d-1) n ++ nonzeros (d-1) (n-1)

dotProd :: Vector -> Vector -> Double
dotProd xs ys = LA.sumElements (xs * ys)

mean :: Vector -> Double
mean xs = LA.sumElements  xs / (fromIntegral $ LA.size xs)

sumOfSq :: Vector -> Double
sumOfSq = LA.sumElements . LA.cmap (^2)

correlation :: Vector -> Vector -> Double
correlation ys xs = (dotProd devX devY) / (std devX * std devY)
  where
    muX = mean xs
    muY = mean ys
    devX = LA.cmap (subtract muX) xs
    devY = LA.cmap (subtract muY) ys
    std = sqrt . sumOfSq

stdCorrelation :: Vector -> Vector -> Double
stdCorrelation ys xs = (dotProd devX devY) / std devX
  where
    muX = mean xs
    muY = mean ys
    devX = LA.cmap (subtract muX) xs
    devY = LA.cmap (subtract muY) ys
    std = sqrt . sumOfSq
    
rSq :: Vector -> Vector -> Double
rSq ysHat ys  = 1 - r/t
  where
    ym      = mean ys
    t       = sumOfSq $ LA.cmap (subtract ym) ys
    r       = sumOfSq $ ys - ysHat
    
regression :: Matrix -> Matrix -> Matrix
regression xss ys = LA.linearSolveSVD xss ys

predict :: Matrix -> Matrix -> Vector
predict ws xss = LA.flatten $ xss <> ws

safeRule :: Double -> [Vector] -> Vector -> [Double]
safeRule lambda xss ys = xy--filter (>=thr) xy
  where
    thr = 2*lambda - lambdaMax
    lambdaMax = maximum xy
    xy = map (abs . stdCorrelation ys) xss
center xs = LA.cmap (subtract (mean xs)) xs
    
getMaxLambda :: [Vector] -> Vector -> Double
getMaxLambda xss ys = maximum $ map (abs . stdCorrelation ys) xss

ys    = LA.fromList [5.0, 3.001, 11.0] :: Vector
xss   = map LA.fromList [[1.0, 2.0],[3.0, 0.1], [2.0, -3.0]] :: [Vector]
ws    = regression (LA.fromColumns xss) (LA.asColumn ys)
yhat  = predict ws (LA.fromColumns xss)
iss   = map LA.fromList $ combinations [-3.0, -2.0, -1.0, 1.0, 2.0, 3.0] 2 1

zss = map 
    (\is -> LA.fromList $ map (LA.prodElements . (**is)) xss)
    $ iss

res = sortBy (comparing (negate.fst))
    $ zip (safeRule 0.1 zss ys)
    $ iss

type Lambda = Double
type Interactions = Vector
type Expr = (Lambda, Interactions, Vector)

multicolinearity :: (Double, Vector) -> [(Double, Vector)] -> [(Double, Vector)]
multicolinearity (lamb, is) zss = undefined

main = do
  --print ws
  --print yhat
  --print $ rSq yhat ys
  --print $ map (correlation ys) xss
  let mLambda = getMaxLambda zss ys
  print 10 -- $ foldr multicolinearity [] $ filter (\(a,b) -> a >= mLambda/2.0) res

-- $> :t iss

{-
enumerate :: Int -> Int -> Int -> [Transformation a] -> [Term a]
enumerate minDegree maxDegree dim funcs = 
  let createTerm (tf,strengths) = Term tf (Strength strengths)
      combStrengths             = combinations dim [minDegree .. maxDegree]
      nonZeros                  = any (/=0)
      cartesian                 = [(,)] <*> funcs <*> filter nonZeros combStrengths
  in  map createTerm cartesian
  
-- vif = 1/(1 - r2)
regMtxToMtx xss = LA.fromLists $ map (map _unReg) xss
regVecToMtx ys  = LA.asColumn $ regVecToVec ys
regVecToVec ys  = LA.fromList (map _unReg ys)

regression :: [[Regression Double] ]-> [Regression Double] -> Matrix
regression xss ys = LA.linearSolveSVD (regMtxToMtx xss) (regVecToMtx ys)

predict :: Matrix -> Matrix -> Vector
predict ws xss = LA.flatten $ xss <> ws

saferule :: (Ord a, Floating a) => [[a]] -> [a] -> a -> [a]
saferule xs y lambda = filter (>=thr) xy
  where
    thr       = 2*lambda - lambdaMax
    lambdaMax = maximum xy
    xy        = map (abs . dotProd) xs
    dotProd   = sum . (zipWith (*) y)

terms = enumerate 0 2 2 [Transformation "id" id, Transformation "log" log]
exprs = map (\t -> Expr [t]) terms
zss   = map (\e -> concatMap (evalExprToList e) xss) exprs
c     = map (correlation ys) zss
zss' = LA.fromColumns $ map LA.fromList $ map (map _unReg) zss

All	 
Range Int Int Int	 
Pos (Vector I)	 
PosCyc (Vector I)	 
Take Int	 
TakeLast Int	 
Drop Int	 
DropLast Int

t = zss' LA.?? (LA.All, LA.DropLast 1)
u = zss' LA.?? (LA.All, LA.TakeLast 1)
ww = LA.linearSolveSVD t u
yy = predict ww t
-}
{-
  mapM_ print $ zip zss c
  print $ ws
  print $ rSq yy (LA.flatten u)
  -}
  
{-
range = filter (/=0) [-3 .. 3]
predictors = map (combinations' range dim) [1 .. dim]
lambda = getMaxLambda (head predictors)
ps' = map (filter (saferule lambda)) predictors
ps'' = map (fold combine) ps'
-}

{- old code :-)


combinations :: Int -> [a] -> [[a]]
combinations n [] = [[]]
combinations 0 xs = [[]]
combinations n xs = let tails     = combinations (n-1) xs
                        insHead x = map (x:) tails
                    in  concatMap insHead xs
                    
-}
