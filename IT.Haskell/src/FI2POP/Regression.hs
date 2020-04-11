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

See ITEA.Regression
-}
module FI2POP.Regression where

import IT.FI2POP     hiding (parMapChunk)
import IT.Regression hiding (parMapChunk)
import FI2POP.Config
import IT.Knowledge
import IT.Algorithms
import qualified Numeric.LinearAlgebra as LA

import Control.Monad.State
import System.Random.SplitMix
import Numeric.Interval

import Data.List (foldl')


-- | slices the domain
sliceDomains :: [(Interval Double)] -> Double -> Int -> [[(Interval Double)]]
sliceDomains domains max_width max_depth = sequence $ map sliceSingleDomain domains
  where
    sliceSingleDomain domain = (iterate nextDepth [domain]) !! max_depth
    nextDepth xs             = concatMap splitInterval xs
    splitInterval x          = if   width x <= max_width
                               then [x]
                               else [(inf x ... m), (m ... sup x)]
      where m = midpoint x
 
constraintFunSliced ::[ [(Interval Double)] ]
              -> (Interval Double) 
              -> [(Interval Double)] 
              -> Solution (Regression Double) RegStats 
              -> Double
constraintFunSliced slices codomain diffCodomains (Sol e f s)
  = let ws          = (LA.toList._weights) s
        e'          = toInterval e
        ws'         = map singleton ws
        
        calcDiffDomains ds = evalDiffExpr e' (map Reg $ ds) ws'
        calcDomains        = exprInterval ws . termIntervals e
                
        merge        = foldr hull empty
        mergeDiff xs = foldr (zipWith hull) (repeat empty) xs

        diffDomains = mergeDiff $ map (map _unReg . calcDiffDomains) slices 
        funDomains  = merge $ map calcDomains slices
        
        inDomain    = isSubsetOf funDomains codomain
        inDiffs     = zipWith isSubsetOf diffDomains diffCodomains
        
        violated    = filter not (inDomain:inDiffs)
    in  fromIntegral $ length violated + length (filter (==empty) (funDomains:diffDomains))
    
-- | creates the constraint evaluation function

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
        calcDomains = exprInterval ws . termIntervals e
        violated    = filter not (inDomain : inDiffs)
    in  fromIntegral $ length violated + length (filter (==empty) (calcDomains domain:diffdomains))
    
runFI2POPReg :: Datasets -> MutationCfg -> ConstraintsCfg -> Output ->  Int -> Int -> Bool -> [String] -> IO ()
runFI2POPReg (D tr te) mcfg (C ds cd cds) output nPop nGens useSlice varnames = do
  g <- initSMGen
  (trainX, trainY) <- parseFile <$> readFile tr
  (testX,  testY ) <- parseFile <$> readFile te
  let toRegMtx = map (map Reg) .  LA.toLists
      fitTrain = fitnessReg nPop (toRegMtx trainX) trainY          
      fitTest  = fitnessTest (toRegMtx testX ) testY
      dim      = LA.cols trainX
      (mutFun, rndTerm)   = withMutation mcfg dim
      
      slices   = sliceDomains ds 0.4 2

  let constFun = if   useSlice
                 then slices `seq` constraintFunSliced slices cd cds
                 else constraintFun ds cd cds
      
      p0       = initialPop dim (getMaxTerms mcfg) nPop rndTerm fitTrain
      
      gens     = (p0 >>= fi2pop mutFun fitTrain constFun) `evalState` g
      feas     = map fst gens
      infeas   = map snd gens
      best     = getBest nGens feas
  print (length slices)    
  genReports output feas nGens fitTest varnames
  

