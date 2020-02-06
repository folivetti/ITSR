{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Example.Regression
Description : Example of usage for Symbolic Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

-}
module ITEA.Regression where

import IT.ITEA
import IT.Regression
import ITEA.Config

import qualified Numeric.LinearAlgebra as LA

import Control.Monad.State
import System.Random.SplitMix

runITEAReg :: Datasets -> MutationCfg -> Output ->  Int -> Int -> IO ()
runITEAReg (D tr te) mcfg output nPop nGens = do
  g <- initSMGen
  (trainX, trainY) <- parseFile <$> readFile tr
  (testX,  testY ) <- parseFile <$> readFile te
  let fitTrain = fitnessReg nPop (LA.toLists trainX) trainY          
      fitTest  = fitnessTest (LA.toLists testX ) testY
      dim      = LA.cols trainX
      (mutFun, rndTerm)   = withMutation mcfg dim
      p0       = initialPop dim (getMaxTerms mcfg) nPop rndTerm fitTrain
      gens     = (p0 >>= itea mutFun fitTrain) `evalState` g
      best     = getBest nGens gens
  genReports output gens nGens fitTest
  

