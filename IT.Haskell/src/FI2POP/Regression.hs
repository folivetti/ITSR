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
module FI2POP.Regression where

import IT.FI2POP
import IT.Regression
import FI2POP.Config
import IT.Knowledge
import IT.Algorithms
import qualified Numeric.LinearAlgebra as LA

import Control.Monad.State
import System.Random.SplitMix
import Numeric.Interval

constraintFun :: [(Interval Double)] 
              -> (Interval Double) 
              -> [(Interval Double)] 
              -> Solution (Regression Double) RegStats 
              -> Double
constraintFun domain codomain diffcodomains (Sol e f s)
  = let ws          = (LA.toList._weights) s
        diffdomains = map _unReg $ evalDiffExpr (toInterval e) (map Reg $ domain) (map singleton ws)
        inDomain    = checkInterval codomain ws e domain
        inDiffs     = zipWith isSubsetOf diffdomains diffcodomains
        violated    = filter id (inDomain : inDiffs)
    in  fromIntegral $ length violated
    
runFI2POPReg :: Datasets -> MutationCfg -> ConstraintsCfg -> Output ->  Int -> Int -> IO ()
runFI2POPReg (D tr te) mcfg (C ds cd cds) output nPop nGens = do
  g <- initSMGen
  (trainX, trainY) <- parseFile <$> readFile tr
  (testX,  testY ) <- parseFile <$> readFile te
  let toRegMtx = map (map Reg) .  LA.toLists
      fitTrain = fitnessReg nPop (toRegMtx trainX) trainY          
      fitTest  = fitnessTest (toRegMtx testX ) testY
      dim      = LA.cols trainX
      (mutFun, rndTerm)   = withMutation mcfg dim
      constFun = constraintFun ds cd cds
      p0       = initialPop dim (getMaxTerms mcfg) nPop rndTerm fitTrain
      gens     = (p0 >>= fi2pop mutFun fitTrain constFun) `evalState` g
      feas     = map fst gens
      infeas   = map snd gens
      best     = getBest nGens feas
  genReports output feas nGens fitTest
  

