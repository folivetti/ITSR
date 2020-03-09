module Main where

import System.Environment

import Data.ConfigFile
import Data.Either.Utils

import FI2POP.Regression
import FI2POP.Config

import Numeric.Interval

--instance (Ord a, Read a) => Read (Interval a) where
--  readsPrec _ cs = let (inf, sup) = read cs
--                   in  [((inf ... sup), "")]
                   
tuple2interval :: (Double, Double) -> Interval Double
tuple2interval (x,y) = (x ... y)
                     
getSetting cp cat x = forceEither $ get cp cat x

readConfig :: IO ()
readConfig = do
  args <- getArgs
  let
    fname = case args of
              (name:_) -> name
              _        -> error "Usage: ./fi2pop config-file-name"

  cp <- return . forceEither =<< readfile emptyCP fname 
  let 
    (expmin, expmax)   = getSetting cp "Mutation" "exponents"
    (termmin, termmax) = getSetting cp "Mutation" "termlimit"
    nzExps             = getSetting cp "Mutation" "nonzeroexps"
    tfuncs             = getSetting cp "Mutation" "transfunctions"
    trainname          = getSetting cp "Dataset" "train"
    testname           = getSetting cp "Dataset" "test"
    nPop               = getSetting cp "Algorithm" "npop"
    nGens              = getSetting cp "Algorithm" "ngens"
    log                = getSetting cp "Algorithm" "log"
    cdm                = getSetting cp "Knowledge" "codomain"
    dcdm               = getSetting cp "Knowledge" "diffcodomains"
    dm                 = getSetting cp "Knowledge" "domains"
    
    mutCfg =  validateConfig
           $  exponents expmin expmax
           <> termLimit termmin termmax
           <> nonzeroExps nzExps
           <> transFunctions tfuncs

    datasetCfg =  validateConfig
               $  trainingset trainname
               <> testset testname

    constCfg = validateConfig
             $  (diffcodomains . map tuple2interval) dcdm
             <> (codomain . tuple2interval) cdm
             <> (domains . map tuple2interval) dm
             
  f <- readFile trainname
  runFI2POPReg datasetCfg mutCfg constCfg log nPop nGens

parse [name] = readConfig
parse _ = putStrLn "Usage: ./fi2pop config-file-name"

main :: IO ()
main = getArgs  >>= parse
