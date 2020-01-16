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
  type Rep Boolean = Bool

  evalExpr (Const c) _             = if c>=0 then B True else B False
  evalExpr ((w,t) `Plus` es) xs    = evalTerm t xs `bOr` evalExpr es xs

  evalTerm (t `After` i) xs        = evalTrans t $ evalInter i xs

  evalTrans (T _ f) (B x)          = B (f x)

  evalInter One xs                 = B True
  evalInter ((ix, e) `Times` i) xs = B (xs !! ix) `bAnd` evalInter i xs

i1, i2 :: Interaction
i1 = (1,1) `Times` (2, -3) `Times` One
i2 = (3, 2) `Times` One

t1, t2 :: Term Bool
t1 = T "not" not `After` i1
t2 = T "id"  id  `After` i2

sample :: Expr Bool
sample = (1.0,t1) `Plus` (-3.0,t2) `Plus` Const 2

test :: IO ()
test = do
  print sample
  print (evalExpr @Boolean sample [True, True, False, True])
