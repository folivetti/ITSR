{-|
Module      : ParseExpr
Description : Parse the expressions and derivatives for using with R script
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

-}

module Main where

import System.Environment
import Data.List
import Data.List.Split

parenthize s = "("++s++"),\n"
process      = map (concatMap parenthize) . transpose . map (splitOn ",") . lines

main = do
  [fname] <- getArgs
  content <- process <$> readFile fname
  mapM_ putStrLn content
