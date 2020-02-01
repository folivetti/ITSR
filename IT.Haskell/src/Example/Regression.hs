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
import IT.Random
import IT.ITEA
import IT.Algorithms
import IT.Mutation
import IT.Regression

import qualified Data.Sequence as S

import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as LA
import qualified MachineLearning as ML
import qualified MachineLearning.Regression as MLR

import Control.Monad.State
import System.Random.SplitMix
import Data.List       (transpose)
import Data.List.Split (splitOn)
import Data.Foldable   (toList)

-- * Test functions

-- | Parse a numerical csv file into predictors and target variables
parseFile :: String -> (LA.Matrix Double, Vector)
parseFile css = ML.splitToXY . LA.fromLists $ map (map read) dat
  where
    dat = map (splitOn ",") $ lines css

-- | sample an interaction with 'dim' variables, 
-- a maximum of 3 variables per interaction and
-- the exponent range between 1 and 3.
rndInter dim = sampleInterMax dim 3 (-3) 3

rndTrans = sampleTrans regAll

-- | sample a term
rndTerm dim = sampleTerm rndTrans (rndInter dim)

-- | sample an expression with n terms
rndExpr dim n = sampleExpr (rndTerm dim) n


test :: IO ()
test = do
  (xss, ys)      <- parseFile <$> readFile "airfoil-train-0.dat"  -- read and parse the file
  (testX, testY) <- parseFile <$> readFile "airfoil-test-0.dat"
  
  -- Linear Regression result
  let xss'  = ML.addBiasDimension xss
      theta = MLR.normalEquation xss' ys
      yhat  = predict xss' theta
      r2    = rSq yhat ys
  putStrLn "\n\nLinear  Reg.:"
  print (theta, r2)

  -- ITEA result  
  g         <- initSMGen          

--ConfigMutation = expRng termsRng dim TransFun
--ConfigFitness =  xss ys
--ConfigITEA = Mutation Fitness nPop

  let fitReg  = fitnessReg (LA.toLists xss) ys           -- fitness function
      fitTest = fitnessReg (LA.toLists testX) testY
      dim    = LA.cols xss                              -- problem dimension
      mf     = mutFun (-3) 3 6 2 (rndTerm dim) rndTrans -- mutation function
      nPop   = 1000                                     -- population size
      nGens  = 100

      genPop  = initialPop dim 4 nPop fitReg
      runAlg  = flip evalState g
      gens    = runAlg (genPop >>= itea mf fitReg)
      -- best    = minimum $ concatMap toList $ take nGens gens
      best    = minimum $ map minimum $  take nGens gens
      --bestOfGens = fmap (_rmse._stat.minimum.toList.take nGens) gens
      zss = map (map _unReg . evalExpr @Regression (cleanExpr (_expr best) (LA.toLists xss))) (LA.toLists testX)
      theta' = _weights $ _stat best
      yhat' = predict (ML.addBiasDimension (LA.fromLists zss)) theta'
      stBest = RS (rmse yhat' testY) (mae yhat' testY) (nmse yhat' testY) (rSq yhat' testY) theta'
      solBest = Sol (_expr best) (_rmse stBest) stBest
  
  putStrLn "\n\nSymb.  Reg.:"
  print best
  --print solBest
  --print bestOfGens
{-  
validateConfig :: Config -> IO ()
validateConfig conf = print "Ok!"

getRandomFuncs :: Config -> Int -> _
getRandomFuncs conf dim = (rndInter, rndTerm, rndTrans, rndExpr)
  where
    rndInter = sampleInterMax dim (_maxInter_ conf) (_minExp_ conf) (_maxExp_ conf)
    rndTerm  = sampleTerm rndTrans (rndInter dim)
    rndTrans = sampleTrans (_funcs_ conf)
    rndExpr  = sampleExpr (rndTerm dim) n

getMutationList :: Config -> Int -> Fitness a b -> [Solution a b -> Rnd (Solution a b)]
getMutationList conf dim f =
  let mp                                     = _mutations_ conf
      (rndInter, rndTerm, rndTrans, rndExpr) = getRandomFuncs
      
      len     = exprLen $ _expr s
      -- Special case
      addMut  = [(addTerm f rndTerm, _add_ mp) | len <= (_maxTerms_ conf)]
      dropMut = [(dropTerm f, _drop_ mp)       | len >= (_minTerms_ conf)]
      
      mutList = [(replaceTerm   f rndTerm      , _term_     mp)
                ,(replaceTrans  f rndTrans     , _trans_    mp)
                ,(positiveInter f minExp maxExp, _posInter_ mp)
                ,(negativeInter f minExp maxExp, _negInter_ mp)
                ] ++ addMut ++ dropMut
  in  concatMap (\f n -> replicate n f) mutList

mutation :: [Solution a b -> Rnd (Solution a b)] -> Solution a b -> Rnd (Solution a b)
mutation muts s = sampleFromList muts >>= s

with :: (Mutation a b -> Population a b -> Rnd [Population a b]) -> Config -> IO [Population a b]
with alg conf = do 
  validateConfig conf
  g         <- getStdGen 
  (xss, ys) <- parseFile <$> readFile (_dataset_ conf)
  let dim    = V.length $ head xss
      fit    = fitnessReg xss ys
      (_,_,_, rndExpr) = getRandomFuncs
      muts   = getMutationList conf dim
      mutFun = mutation muts
      genPop = S.fromList <$> initialPop rndExpr fit dim (_maxTerms conf) (_nPop_ conf)
  return $ evalState (genPop >>= itea mutFun) g

data Param a = None | Has a

instance Semigroup (Param a) where
  p <> None = p
  _ <> p    = p
instance Monoid (Param a) where
  mempty = None
  
data Config { _dataset_   :: Param String
            , _nPop_      :: Param Int
            , _minExp_    :: Param Int
            , _maxExp_    :: Param Int
            , _mutations_ :: Param MutProb
            , _minTerms_  :: Param Int
            , _maxTerms_  :: Param Int
            , _maxInter_  :: Param Int
            , _functions_ :: Param (Transformation Double)
            }
            
instance Semigroup Config where
  (Conf x1 x2 x3 x4 x5 x6 x7 x8 x9) <> (Conf y1 y2 y3 y4 y5 y6 y7 y8 y9) = Conf (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4) (x5  <> y5) (x6 <> y6) (x7 <> y7) (x8 <> y8) (x9 <> y9)
instance Monoid Config where
  mempty = Conf mempty mempty mempty mempty mempty mempty mempty mempty mempty
            
_dataset   x = mempty { _dataset_   = Has x }
_nPop      x = mempty { _nPop_      = Has x }
_minExp    x = mempty { _minExp_    = Has x }
_maxExp    x = mempty { _maxExp_    = Has x }
_mutations x = mempty { _mutations_ = Has x }
_minTerms  x = mempty { _minTerms_  = Has x }
_maxTerms  x = mempty { _maxTerms_  = Has x }
_maxTerms  x = mempty { _maxTerms_  = Has x }
_maxInter  x = mempty { _maxInter_  = Has x }
_functions x = mempty { _functions_ = Has x }

data MutProb = MP { _term_     :: Param Int
                  , _trans_    :: Param Int
                  , _posInter_ :: Param Int
                  , _negInter_ :: Param Int
                  , _add_      :: Param Int
                  , _drop_     :: Param Int
                  }

-- arrghhh!
instance Semigroup MutProb where
  (Conf x1 x2 x3 x4 x5 x6) <> (Conf y1 y2 y3 y4 y5 y6) = Conf (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4) (x5  <> y5) (x6 <> y6)
instance Monoid MutProb where
  mempty = Conf mempty mempty mempty mempty mempty mempty

_term     x = mempty { _term_     = Has x }
_trans    x = mempty { _trans_    = Has x }
_posInter x = mempty { _posInter_ = Has x }
_negInter x = mempty { _negInter_ = Has x }
_add      x = mempty { _add_      = Has x }
_drop     x = mempty { _drop_     = Has x }


-- Mutation Options
data Muts a = TERM a | TRANS a | POS a | NEG a | ADD a | DROP a | Muts a :> Muts a 
                deriving Show
-- IT options
minExp, maxExp, minTerms, maxTerms, maxInter, functions

-- ITEA options
train, test, npop

test2 :: IO ()
test2 = do
  let muts   =   TERM  1
             :>  TRANS 1
             :>  POS   1
             :>  NEG   1
             :>  ADD   2
             :>  ADD   1
             :>> DROP  1
             
  let muts   =  _term     1
             <> _trans    1
             <> _posInter 1
             <> _negInter 1
             <> _add      1
             <> _drop     1
      
      config = Conf { _train = fname,  _minExp = (-3), _maxExp = 3,... }
      
      config =  _dataset  fname 
             <> _minExp    (-3) 
             <> _maxExp      3 
             <> _mutations muts 
             <> _minTerms    2 
             <> _maxTerms    4
             <> _maxInter    3
             <> _functions regAll

  gens <- itea `with` config
  print $ bestUntil 100 gens
-}
