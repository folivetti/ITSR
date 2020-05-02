module Main where
--module IT.Enumeration where

import System.Environment

import IT
import IT.Regression (Regression(Reg, _unReg))
import ITEA.Config

import Data.List
import Data.Ord
import qualified Numeric.LinearAlgebra as LA

type Vector = LA.Vector Double
type Matrix = LA.Matrix Double

type Lambda       = Double
type Interactions = Vector
type Predictor    = (Lambda, Interactions, Vector)

-- | scalar product of two vectors
dotProd :: Vector -> Vector -> Double
dotProd xs ys = LA.sumElements (xs * ys)

-- | arithmetic mean of elements from a vector
mean :: Vector -> Double
mean xs = LA.sumElements xs / (fromIntegral $ LA.size xs)

-- | sum of squared values of a vector
sumOfSq :: Vector -> Double
sumOfSq = LA.sumElements . LA.cmap (^ 2)

center :: Vector -> Vector
center xs = LA.cmap (subtract mu) xs
  where
    mu = mean xs

-- | correlation between two vectors
correlation :: Vector -> Vector -> Double
correlation xs ys = dotProd devX devY / (std devX * std devY)
  where
    devX = center xs
    devY = center ys
    std  = sqrt . sumOfSq

-- | given a vector of targets ys
-- | generates all interactions of n_inters predictors
-- from a set of dim predictors and exponents from exps
combinations :: [Double] -> Int -> Int -> [[Double]]
combinations exps dim n_inters = go dim n_inters
  where
    nonzeros d n = concatMap (\x -> map (x:) $ go d n) exps
    zeros d n    = map (0:) (go d n)

    go 0 n = [[]]
    go d 0 = map (0:) (go (d - 1) 0)
    go d n
      | d == n     = nonzeros (d - 1) (n - 1)
      | otherwise  = zeros (d - 1) n ++ nonzeros (d - 1) (n - 1)

-- | r^2 of the predicted values (ysHat) and true values (ys)
rSq :: Vector -> Vector -> Double
rSq ysHat ys = 1 -  sumOfSq (ys - ysHat) / sumOfSq (center ys)

-- | returns the coefficients of OLS for predictors xss
-- and targets ys
regression :: Matrix -> Matrix -> Matrix
regression xss ys = LA.linearSolveSVD (1 LA.||| xss) ys

-- | return the estimative ysHat for coefficents ws
-- on predictors xss
predict :: Matrix -> Matrix -> Vector
predict ws xss = LA.flatten $ (1 LA.||| xss) <> ws

-- calculates the strong rule for the predictor in xs
strongCorr :: Vector -> Vector -> Double
strongCorr ys xs = abs $ dotProd devX devY / std devX
  where
    devX = center xs
    devY = center ys
    std  = sqrt . sumOfSq

-- | applies the safe rule for a set of predictors
-- cutsoff all predictors with strong rule less than
-- 2*ratio*maxlambda - maxlambda
safeRule :: Double -> [Predictor] -> [Predictor]
safeRule ratio ps = filter ((>= thr) . getLamb) ps
  where
    thr = 2 * ratio * maxLambda - maxLambda
    maxLambda = maximum (map getLamb ps)

-- | acessors for the Predictor type
getLamb :: Predictor -> Lambda
getLamb (l, _, _) = l

getInteractions :: Predictor -> Interactions
getInteractions (_, is, _) = is

getVec :: Predictor -> Vector
getVec (_, _, zs) = zs

-- | calculate Variance inflation factor of a new predictor
-- w.r.t. the current set of predictors
vif :: Vector -> [Vector] -> Double
vif zs zss = 1.0 / (1.0 - r2 + 1e-10)
  where
    r2    = rSq zsHat zs
    zsHat = predict (regression zss' zs') zss'
    zss'  = LA.fromColumns zss
    zs'   = LA.asColumn zs

-- | inserts new predictor if its VIF is larger than a threshold
insertNewPredictor :: Double -> [Predictor] -> Predictor -> [Predictor]
insertNewPredictor thr [] p = [p]

insertNewPredictor thr ps p@(_, _, zs)
  | vif zs (map getVec ps) > thr = ps
  | otherwise                    = p : ps

-- | evaluates an interaction (see IT)
evalInteraction :: [Vector] -> Vector -> Vector
evalInteraction xss is = LA.fromList $ map (LA.prodElements . (** is)) xss

-- | creates a predictor type from a new interaction
createPredictor :: Vector -> [Vector] -> Vector -> Predictor
createPredictor ys xss is = (lambda, is, zs)
  where
    lambda = strongCorr ys zs
    zs     = evalInteraction xss is

applySafeRule :: Double -> [Predictor] -> [Predictor]
applySafeRule thr ps = safeRule thr (sortPredictors ps)
  where
    sortPredictors = sortBy (comparing (negate.getLamb))

updateInteractions thrSafe thrVIF cur new = foldl' (insertNewPredictor thrVIF) [] newPredictors
  where
    newPredictors = applySafeRule thrSafe (cur ++ new)


main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run (trainname:testname:_) = do
  (trainX, trainY) <- parseFile <$> readFile trainname
  (testX,  testY ) <- parseFile <$> readFile testname
  putStrLn trainname
    
  let 
    -- | dummy data set
    --ys  = LA.fromList [5.0, 3.001, 11.0]
    --xss = map LA.fromList [[1.0, 2.0], [3.0, 0.1], [2.0, -3.0]]

    -- | results for a simple linear regression
    ws   = regression trainX (LA.asColumn trainY) -- (LA.fromColumns trainX)
    yHat = predict ws testX -- (LA.fromColumns testX)

    -- | create all combinations of interactions from
    -- 1 to dim variables within a range of exponents
    dim            = LA.cols trainX
    range          = [-3.0, -2.0, -1.0, 1.0, 2.0, 3.0]
    mkCombinations = map LA.fromList . combinations range dim
    iss            = map mkCombinations [1 .. dim]
    zss            = map (map (createPredictor trainY (LA.toRows trainX))) iss

    ps  = foldl' (updateInteractions 0.8 10) [] zss
    
    opt = map Main.getInteractions $ take 10 ps
    trainZ = LA.fromColumns $ map (evalInteraction (LA.toRows trainX)) opt   
    testZ = LA.fromColumns $ map (evalInteraction (LA.toRows testX)) opt
    zHat  = predict (regression trainZ (LA.asColumn trainY)) testZ

  --mapM_ print $ map Main.getInteractions $ take 5 ps
  print $ sqrt $ mean $ LA.cmap (^ 2) (zHat - testY)
  print $ sqrt $ mean $ LA.cmap (^ 2) (yHat - testY)
-- $> main
