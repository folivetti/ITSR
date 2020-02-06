{-# LANGUAGE FunctionalDependencies #-}
module ITEA.Config where

import System.Directory
import System.IO
import System.Clock

import IT
import IT.ITEA
import IT.Algorithms
import IT.Mutation
import IT.Regression
import IT.Random

import qualified Numeric.LinearAlgebra as LA
import Control.Monad.State
import System.Random.SplitMix
import qualified MachineLearning as ML
import Data.List.Split (splitOn)
import Data.List (intersperse, foldl')

import Control.Parallel.Strategies
import Control.DeepSeq

-- | Configuration Validation
class Monoid a => Valid a b | a -> b, b -> a where
  validateConfig :: a -> b

data Param a = None | Has a deriving Show

fromParam :: Param a -> a
fromParam (Has x) = x

instance Semigroup (Param a) where
  p <> None = p
  _ <> p    = p
instance Monoid (Param a) where
  mempty = None
  
-- | Mutation configuration
data Funcs = FLinear | FNonLinear | FTrig | FAll deriving Read

data UncheckedMutationCfg = UMCfg { _expLim   :: (Param (Int, Int))
                                  , _termLim  :: (Param (Int, Int))
                                  , _nzExp    :: Param Int
                                  , _transFun :: (Param Funcs)
                                  }
data MutationCfg = MCfg (Int, Int) (Int, Int) Int Funcs

instance Semigroup UncheckedMutationCfg where
  (UMCfg p1 p2 p3 p4) <> (UMCfg q1 q2 q3 q4) = UMCfg (p1<>q1) (p2<>q2) (p3<>q3) (p4<>q4)
instance Monoid UncheckedMutationCfg where
  mempty = UMCfg mempty mempty mempty mempty
  
exponents :: Int -> Int -> UncheckedMutationCfg
exponents x y = mempty { _expLim   = Has (x,y) }

termLimit :: Int -> Int -> UncheckedMutationCfg
termLimit   x y = mempty { _termLim  = Has (x,y) }

nonzeroExps :: Int -> UncheckedMutationCfg
nonzeroExps x = mempty { _nzExp = Has x }

transFunctions :: Funcs -> UncheckedMutationCfg
transFunctions  fs  = mempty { _transFun = Has fs    }

instance Valid UncheckedMutationCfg MutationCfg where
  -- validateConfig :: UncheckedMutationCfg -> MutationCfg
  validateConfig (UMCfg None _ _ _) = error "No exponent limits set"
  validateConfig (UMCfg _ None _ _) = error "No expression size limits set"
  validateConfig (UMCfg _ _ None _) = error "No maximum non-zero exponents set"
  validateConfig (UMCfg _ _ _ None) = error "No transformation functions chosen"
  validateConfig c = MCfg (pexpLim c) (ptermLim c) (pnzExp c) (ptransFun c)
    where
      pexpLim   = fromParam . _expLim
      ptermLim  = fromParam . _termLim
      pnzExp    = fromParam . _nzExp
      ptransFun = fromParam . _transFun      

getMaxTerms (MCfg _ (_, maxTerms) _ _) = maxTerms

-- | Parse a numerical csv file into predictors and target variables
parseFile :: String -> (LA.Matrix Double, Vector)
parseFile css = ML.splitToXY . LA.fromLists $ map (map read) dat
  where
    dat = map (splitOn ",") $ lines css
    
withMutation :: MutationCfg -> Int -> (Mutation Double, Rnd (Term Double))
withMutation (MCfg elim tlim nzExp transfun) dim = (mutFun elim tlim rndTerm rndTrans, rndTerm)
  where
    trans FLinear = regLinear
    trans FNonLinear = regNonLinear
    trans FTrig = regTrig
    trans FAll = regAll
    (minExp, maxExp) = elim
    rndInter = sampleInterMax dim nzExp minExp maxExp
    rndTrans = sampleTrans (trans transfun)
    rndTerm  = sampleTerm rndTrans rndInter

-- | Datasets configuration

data UncheckedDatasets = UD { _trainset :: Param String, _testset :: Param String }  deriving Show
data Datasets = D String String deriving Show

trainingset, testset :: String -> UncheckedDatasets
trainingset name = mempty { _trainset = Has name }
testset     name = mempty { _testset = Has name }

instance Semigroup UncheckedDatasets where
  (UD p1 p2) <> (UD q1 q2) = UD (p1<>q1) (p2<>q2)
instance Monoid UncheckedDatasets where
  mempty = UD mempty mempty

instance Valid UncheckedDatasets Datasets where
  validateConfig (UD None _) = error "No training data was set"
  validateConfig (UD _ None) = error "No test data was set"
  validateConfig (UD tr te) = D (fromParam tr) (fromParam te)
  
-- | Output configuration  
data Output = Screen | PartialLog String | FullLog String deriving Read

getBest :: Int  -> [Population Double RegStats] -> Solution Double RegStats
getBest n p     = minimum $ getAllBests n p
getAllBests n p = map minimum (take n p) 

data StatType = Best | Worst | Avg

applyStat f n p = map (map f) $ take n p

getAll :: StatType -> (RegStats -> Double) -> Int -> [[RegStats]] -> [Double]
getAll Best f n p  = map minimum (applyStat f n p) 
getAll Worst f n p = map maximum (applyStat f n p)
getAll Avg   f n p = map mean    (applyStat f n p)
  where mean xs = sum xs / fromIntegral (length xs)

getAllStats :: Int -> [[RegStats]] -> (RegStats -> Double) -> [(Double, Double, Double)]
getAllStats n p f = map myfold (take n p) `using` parListChunk 100 rdeepseq
  where
    myfold :: [RegStats] -> (Double, Double, Double)
    myfold pi = let xs = map f pi
                    y  = head xs
                    g (mi,ma,s,n) x    = (min mi x, max ma x, s+x,n+1)
                    (mini, maxi, s, n) = foldl' g (y,y,0,0) xs
                in  (mini, maxi, s/n)

createIfDoesNotExist fname = do
  isCreated <- doesFileExist fname
  h <- if   isCreated
       then openFile fname AppendMode
       else openFile fname WriteMode
  if isCreated then hPutStrLn h "" else hPutStrLn h headReport
  return h

genReports :: Output -> [Population Double RegStats] -> Int -> (Solution Double RegStats -> RegStats) -> IO ()
genReports Screen pop n fitTest = do
  let best = getBest n pop
  putStrLn "Best expression applied to the training set:\n"
  print best
  putStrLn "Best expression applied to the test set:\n"
  print (fitTest best)
  
genReports (PartialLog dirname) pop n fitTest = do
  createDirectoryIfMissing True dirname
  let fname = dirname ++ "/stats.csv"
  
  hStats <- createIfDoesNotExist fname

  let best = getBest n pop
  
  t0 <- getTime Realtime
  print best
  t1 <- getTime Realtime
  
  let bestTest = fitTest best
      stats = concat $ intersperse "," $ [dirname, show (sec t1 - sec t0)] ++ resultsToStr best bestTest      
  
  hPutStr hStats stats
  hClose hStats

genReports (FullLog dirname) pop n fitTest = do
  createDirectoryIfMissing True dirname
  let fname = dirname ++ "/stats.csv"
  
  hStats <- createIfDoesNotExist fname

  let best = getBest n pop
  
  t0 <- getTime Realtime
  print best
  t1 <- getTime Realtime
  
  let bestTest = fitTest best
      stats = concat $ intersperse "," $ [dirname, show (sec t1 - sec t0)] ++ resultsToStr best bestTest      
  
  hPutStr hStats stats
  hClose hStats
  
  let statTrain = map (map _stat) pop
      statTest  = map (map fitTest) pop 
      evoTrain  = getAllStats n statTrain
      evoTest   = getAllStats n statTest 

      evoTrainStats = map evoTrain [_rmse, _mae, _nmse, _r2]
      evoTestStats  = map evoTest  [_rmse, _mae, _nmse, _r2]

      combinations = (++) <$> ["Rmse", "Mae", "Nmse", "R2"] <*> ["Best", "Worst", "Avg"]
      params       = (,)  <$> ["Rmse", "Mae", "Nmse", "R2"] <*> ["Best", "Worst", "Avg"]
      trainnames   = map (\s -> dirname ++ "/train" ++ s ++ ".csv") combinations
      testnames    = map (\s -> dirname ++ "/test"  ++ s ++ ".csv") combinations

  mapM_ (genEvoReport evoTrainStats) $ zip trainnames params
  mapM_ (genEvoReport evoTestStats)  $ zip testnames  params

genEvoReport :: [[(Double, Double, Double)]] -> (String, (String, String)) -> IO ()
genEvoReport stats (fname, (p1,p2)) = do
  let
    datavalues = case p1 of
                   "Rmse" -> stats !! 0
                   "Mae"  -> stats !! 1
                   "Nmse" -> stats !! 2
                   "R2"   -> stats !! 3
    statsvals  = case p2 of
                   "Best"  -> map (\(x,_,_) -> x) datavalues
                   "Worst" -> map (\(_,x,_) -> x) datavalues
                   "Avg"   -> map (\(_,_,x) -> x) datavalues
    s = concat $ intersperse "," $ map show statsvals
  h <- openFile fname AppendMode
  hPutStr h s
  hClose h

resultsToStr :: Solution Double RegStats -> RegStats -> [String]
resultsToStr train stest = (map show statlist) ++ [show (_expr train)]
  where 
    strain = _stat train
    statlist = [_rmse strain, _rmse stest, _mae strain, _mae stest, _nmse strain, _nmse stest, _r2 strain, _r2 stest]

headReport = "name,time,RMSE_train,RMSE_test,MAE_train,MAE_test,NMSE_train,NMSE_test,r2_train,r2_test,expr"
