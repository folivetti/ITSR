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

createMutCfg (e1,e2) tmin tmax =  validateConfig
                               $  exponents e1 e2
                               <> termLimit tmin tmax
                               <> nonzeroExps 3
                               <> transFunctions FAll

argmin xs = snd $ minimumBy (comparing fst) (zip xs [0..])

validateArgs :: [String] -> (String, Int)
validateArgs (x:y:_) = (x, read y)
validateArgs _ = error "Usage: crossval dataname fold"

main :: IO ()
main = do
  args <- getArgs  
  let 
    allCfgs = [createMutCfg] <*> [(-1,1),(-2,2),(-3,3)] <*> [2,3] <*> [5,10]
    (dname, nfold) = validateArgs args
    
  tests <- mapM (runCfg dname nfold) allCfgs
  let (bestCfg, bestRMSE) = minimumBy (comparing snd) (zip allCfgs tests)
  print $ dname ++ "," ++ show nfold ++ "," ++ show bestCfg ++ "," ++ show bestRMSE

runCfg :: String -> Int -> MutationCfg -> IO Double
runCfg dname fold mutCfg = do
  let fname = "datasets/" ++ dname ++ "/" ++ dname ++ "-train-" ++ show 0 ++ ".dat" 
  
  (trainX, trainY) <- parseFile <$> readFile fname

  let nRows = LA.rows trainX
      dim      = LA.cols trainX
      
      trY   = LA.subVector 0 (nRows `div` 2) trainY
      tvY   = LA.subVector (nRows `div` 2) (nRows `div` 2 + nRows `mod` 2) trainY
      trX   = trainX LA.?? (LA.Take (nRows `div` 2), LA.All)
      tvX   = trainX LA.?? (LA.Drop (nRows `div` 2), LA.All)
      
      toRegMtx = map (map Reg) .  LA.toLists
      
      fitTrain1 = fitnessReg 100 (toRegMtx trX) trY          
      fitTest1  = fitnessTest (toRegMtx tvX) tvY
      
      fitTrain2 = fitnessReg 100 (toRegMtx tvX) tvY
      fitTest2  = fitnessTest (toRegMtx trX) trY
      
  rmses1 <- fmap (map (_rmse.fitTest1)) $ sequence $ replicate 5 (runITEARegCV fitTrain1 dim mutCfg Screen 100 100)
  rmses2 <- fmap (map (_rmse.fitTest2)) $ sequence $ replicate 5 (runITEARegCV fitTrain2 dim mutCfg Screen 100 100)

  return $ (sum rmses1 + sum rmses2) / 10.0

runITEARegCV :: Fitness (Regression Double) RegStats -> Int -> MutationCfg -> Output ->  Int -> Int -> IO (Solution (Regression Double) RegStats)
runITEARegCV fitTrain dim mcfg output nPop nGens = do
  g <- initSMGen

  let (mutFun, rndTerm)  = withMutation mcfg dim
      p0       = initialPop dim (getMaxTerms mcfg) nPop rndTerm fitTrain
      gens     = (p0 >>= itea mutFun fitTrain) `evalState` g
      best     = getBest nGens gens
  return best
