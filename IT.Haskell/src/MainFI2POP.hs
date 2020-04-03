{-|
Module      : FI2POP
Description : Main program to run FI2POP with a configuration file.
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Main program to run FI2POP with a configuration file.
-}
module Main where

import System.Environment

import Data.ConfigFile
import Data.Either.Utils

import FI2POP.Regression
import FI2POP.Config

import Numeric.Interval

-- | converts a tuple to an Interval                 
tuple2interval :: (Double, Double) -> Interval Double
tuple2interval (x,y) = (x ... y)
                     
getSetting cp cat x = forceEither $ get cp cat x

readConfig :: String -> IO ()
readConfig fname = do
  cp <- return . forceEither =<< readfile emptyCP fname 
  let 
    (expmin, expmax)   = getSetting cp "Mutation"  "exponents"
    (termmin, termmax) = getSetting cp "Mutation"  "termlimit"
    nzExps             = getSetting cp "Mutation"  "nonzeroexps"
    tfuncs             = getSetting cp "Mutation"  "transfunctions"
    trainname          = getSetting cp "Dataset"   "train"
    testname           = getSetting cp "Dataset"   "test"
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
    useSlice = True
             
  f <- readFile trainname
  runFI2POPReg datasetCfg mutCfg constCfg log nPop nGens useSlice

parse :: [String] -> IO  ()
parse [fname] = readConfig fname
parse _       = putStrLn "Usage: ./fi2pop config-file-name"

main :: IO ()
main = getArgs  >>= parse
