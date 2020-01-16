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
module Example.Regression where

import IT
import IT.ITEA
import IT.Algorithms
import IT.Regression

import qualified Data.Vector.Unboxed as V
import qualified Data.Sequence as S

import Statistics.Regression
import Statistics.Matrix.Types

import Control.Monad.State
import System.Random
import Data.List       (transpose)
import Data.List.Split (splitOn)
import Data.Foldable   (toList)

-- * Test functions

-- | Parse a numerical csv file into predictors and target variables
parseFile :: String -> ([Vector], Vector)
parseFile s = (xss, ys)
  where
    dat = map (splitOn ",") $ lines s
    xss = map (V.fromList . map read . init) dat
    ys  = V.fromList $ map (read.last) dat

-- | sample from all provided transformation functions
rndTrans = sampleTrans regAll

-- | sample an interaction with 'dim' variables, 
-- a maximum of 3 variables per interaction and
-- the exponent range between 1 and 3.
rndInter dim = sampleInterMax dim 3 (-3) 3

-- | sample a term
rndTerm dim = sampleTerm rndTrans (rndInter dim)

-- | sample an expression with n terms
rndExpr dim n = sampleExpr (rndTerm dim) n

initialPop :: Fitness Double RegStats
           -> Int -- dimension
           -> Int -- maxTerms
           -> Int -- nPop
           -> Rnd [Solution Double RegStats]
initialPop fit dim maxTerms 0    = return []
initialPop fit dim maxTerms nPop = do n <- sampleRng 1 maxTerms
                                      e  <- rndExpr dim n
                                      let s = (fit.uniqueTerms) e
                                      ss <- initialPop fit dim maxTerms (nPop-1)
                                      return $ s : ss

test :: IO ()
test = do
  (xss, ys) <- parseFile <$> readFile "yacht.dat"  -- read and parse the file
  
  -- Linear Regression result
  let xss' = map V.fromList $ transpose (map V.toList xss)

  putStrLn "\n\nLinear  Reg.:"
  print $ olsRegress xss' ys

  -- ITEA result  
  g         <- getStdGen          
  
  let fitReg = fitnessReg xss ys           -- fitness function
      dim    = V.length $ head xss         -- problem dimension
      mf     = mutFun (-3) 3 fitReg (rndTerm dim) rndTrans -- mutation function
      nPop   = 1000                        -- population size

      genPop  = S.fromList <$> initialPop fitReg dim 4 nPop
      runAlg  = (flip evalState) g
      gens    = runAlg (genPop >>= itea mf)
      best    = minimum $ concat $ fmap toList $ take 1000 gens
      bestOfGens = fmap (_rmse._stat.minimum.toList) $ take 1000 gens
  
  putStrLn "\n\nSymb.  Reg.:"
  print best
  --print bestOfGens
