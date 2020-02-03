module Main where

import ITEA.Regression
import ITEA.Config

main :: IO ()
main = do
  let mutCfg =  validateConfig
             $  exponents (-3) 3
             <> termLimit 2 10
             <> nonzeroExps 3
             <> transFunctions FAll

      datasetCfg = validateConfig 
                 $ trainingset "airfoil-train-0.dat" 
                 <> testset "airfoil-test-0.dat"

  runITEAReg datasetCfg mutCfg (File "airfoil") 100 10
