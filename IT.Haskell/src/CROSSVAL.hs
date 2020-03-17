{-|
Module      : CROSSVAL
Description : Hyperparameter tuning for ITEA
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Hyperparameter tuning for ITEA
-}
module Main where

import ITEA.Regression
import ITEA.Config
import IT.ITEA
import IT.Regression
import IT.Algorithms

import qualified Numeric.LinearAlgebra as LA

import Data.List
import Data.Ord

import Control.Monad.State
import System.Random.SplitMix
import System.Environment


-- | Creates a mutation configuration instance
createMutCfg (e1,e2) tmax =  validateConfig
                          $  exponents e1 e2
                          <> termLimit 2 tmax
                          <> nonzeroExps 10
                          <> transFunctions FAll

-- | Validates the program arguments
validateArgs :: [String] -> (String, Int)
validateArgs (x:y:_) = (x, read y)
validateArgs _ = error "Usage: crossval dataname fold"

-- | Runs a single experiment with a given configuration
runITEARegCV :: Fitness (Regression Double) RegStats                -- ^ Training fitness function
             -> (Solution (Regression Double) RegStats -> RegStats) -- ^ Test fitness function
             -> Int                                                 -- ^ Problem dimension
             -> MutationCfg                                         -- ^ Mutation configuration
             -> Int                                                 -- ^ population size
             -> Int                                                 -- ^ number of generations
             -> IO Double
runITEARegCV fitTrain fitTest dim mcfg nPop nGens = do
  -- random seed
  g <- initSMGen

  -- run ITEA with given configuration
  let (mutFun, rndTerm)  = withMutation mcfg dim
      p0                 = initialPop dim (getMaxTerms mcfg) nPop rndTerm fitTrain
      gens               = (p0 >>= itea mutFun fitTrain) `evalState` g
      best               = getBest nGens gens
  (return._rmse.fitTest)  best

-- | runs a configuration for a given data set
runCfg :: String -> Int -> MutationCfg -> IO Double
runCfg dname fold mutCfg = do
  let fname = "datasets/" ++ dname ++ "/" ++ dname ++ "-train-" ++ show 0 ++ ".dat" 
  
  (trainX, trainY) <- parseFile <$> readFile fname

  let nRows = LA.rows trainX
      dim   = LA.cols trainX
      
      trY   = LA.subVector 0 (nRows `div` 2) trainY
      tvY   = LA.subVector (nRows `div` 2) (nRows `div` 2 + nRows `mod` 2) trainY
      trX   = trainX LA.?? (LA.Take (nRows `div` 2), LA.All)
      tvX   = trainX LA.?? (LA.Drop (nRows `div` 2), LA.All)
      
      toRegMtx = map (map Reg) .  LA.toLists
      
      fitTrain1 = fitnessReg 100 (toRegMtx trX) trY          
      fitTest1  = fitnessTest (toRegMtx tvX) tvY
      
      fitTrain2 = fitnessReg 100 (toRegMtx tvX) tvY
      fitTest2  = fitnessTest (toRegMtx trX) trY
      
      runFive = sequence . replicate 5
      
  rmses1 <- runFive (runITEARegCV fitTrain1 fitTest1 dim mutCfg 100 100)
  rmses2 <- runFive (runITEARegCV fitTrain2 fitTest2 dim mutCfg 100 100)

  return $ (sum rmses1 + sum rmses2) / 10.0
  
-- | Main function
main :: IO ()
main = do
  args <- getArgs  
  let
    -- generate all combination of configurations
    allCfgs        =  [createMutCfg] 
                  <*> [(-1,1),(-2,2),(-3,3), (0,1), (0,2),(0,3)] 
                  <*> [5,10,15]

    (dname, nfold) = validateArgs args
    
  tests <- mapM (runCfg dname nfold) allCfgs
  
  let (bestCfg, bestRMSE) = minimumBy (comparing snd) (zip allCfgs tests)
  print $ dname ++ "," ++ show nfold ++ "," ++ show bestCfg ++ "," ++ show bestRMSE
