module Main where

import ITEA.Regression
import ITEA.Config
import IT.ITEA
import IT.Regression
import IT.Algorithms
import IT.Random

import qualified Numeric.LinearAlgebra as LA

import Control.Monad.State
import System.Random.SplitMix
import Control.Exception

main :: IO ()
main = do 
  let mutCfg = validateConfig
             $  exponents (-1) 1
             <> termLimit 2 5
             <> nonzeroExps 3
             <> transFunctions FAll
  
  runCfg mutCfg

runCfg mutCfg = do
  (trainX, trainY) <- parseFile <$> readFile "datasets/airfoil/airfoil-train-0.dat" 
  let nRows = LA.rows trainX
      dim      = LA.cols trainX
      
      trY   = LA.subVector 0 (nRows `div` 2) trainY
      tvY   = LA.subVector (nRows `div` 2) (nRows `div` 2 + nRows `mod` 2) trainY
      trX   = trainX LA.?? (LA.Take (nRows `div` 2), LA.All)
      tvX   = trainX LA.?? (LA.Drop (nRows `div` 2), LA.All)
      
      toRegMtx = map (map Reg) .  LA.toLists
      
      fitTrain = fitnessReg 1000 (toRegMtx trX) trY          
      
  runITEARegCV fitTrain dim mutCfg 1000

runITEARegCV :: Fitness (Regression Double) RegStats -> Int -> MutationCfg -> Int -> IO ()
runITEARegCV fitTrain dim mcfg nPop = do
  g <- initSMGen

  let (mutFun, rndTerm)  = withMutation mcfg dim
      p0       = initialPop dim (getMaxTerms mcfg) nPop rndTerm fitTrain
      gens     = (p0 >>= itea mutFun fitTrain) `evalState` g
  catchBug gens  

catchBug [] = return ()
catchBug (g:gs) = do
  catchBug' g
  catchBug gs

f :: SomeException -> IO ()  
f exc = return ()

catchBug' [] = return ()
catchBug' (e:es) = do
  print (_expr e)
  catch (print (_fit e)) f
  -- (\exc -> print $ show (_expr e) ++ " " ++ show (exc :: SomeException))
  -- print $ try (_fit e) (_expr e)
  catchBug' es
