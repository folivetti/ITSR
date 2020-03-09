module Main where

import System.Environment

import Data.ConfigFile
import Data.Either.Utils

import ITEA.Regression
import ITEA.Config

getSetting cp cat x = forceEither $ get cp cat x

readConfig :: IO ()
readConfig = do
  args <- getArgs
  let
    fname = case args of
              (name:_) -> name
              _        -> error "Usage: ./itea config-file-name"

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

    mutCfg =  validateConfig
           $  exponents expmin expmax
           <> termLimit termmin termmax
           <> nonzeroExps nzExps
           <> transFunctions tfuncs

    datasetCfg =  validateConfig
               $  trainingset trainname
               <> testset testname
  f <- readFile trainname
  runITEAReg datasetCfg mutCfg log nPop nGens

parse [name] = readConfig
parse _ = putStrLn "Usage: ./itea config-file-name"

main :: IO ()
main = getArgs  >>= parse
