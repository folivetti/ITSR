{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Boolean where

import IT

newtype Boolean = B {_unBool :: Bool}

toB :: (Bool -> Bool -> Bool) -> (Boolean -> Boolean -> Boolean)
toB f b1 b2 = B (f (_unBool b1) (_unBool b2))

bOr  = toB (||)
bAnd = toB (&&)

instance Show Boolean where
  show (B x) = show x

instance IT Boolean where
  itTimes = itTimesDefault All
  itAdd   = itAddDefault   Any
  itWeight x by = by
  

