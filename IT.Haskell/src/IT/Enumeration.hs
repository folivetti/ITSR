module IT.Enumeration where

import IT
import IT.Regression

combinations :: Int -> [a] -> [[a]]
combinations n [] = [[]]
combinations 0 xs = [[]]
combinations n xs = let tails     = combinations (n-1) xs
                        insHead x = map (x:) tails
                    in  concatMap insHead xs
                    
enumerate :: Int -> Int -> Int -> [Transformation a] -> [Term a]
enumerate minDegree maxDegree dim funcs = 
  let createTerm (tf,strengths) = Term tf (Strength strengths)
      cartesian                 = [(,)] <*> funcs <*> combinations dim [minDegree .. maxDegree]
  in  map createTerm cartesian
  
saferule :: (Ord a, Floating a) => [[a]] -> [a] -> a -> [a]
saferule xs y lambda = filter (>=thr) xy
  where
    thr       = 2*lambda - lambdaMax
    lambdaMax = maximum xy
    xy        = map (abs . dotProd) xs
    dotProd   = sum . (zipWith (*) y)

main = do
  let terms = enumerate 0 2 6 [Transformation "id" id, Transformation "log" log]
  print $ map (evalExprToList (Expr terms)) [[Reg 1.0, Reg 2.0],[Reg 3.0, Reg 0.1]]
