module Main where

import Data.Semigroup
import Data.Foldable
import GHC.Exts
import Data.List

import Numeric.Interval hiding (elem)

newtype Interaction   = Strength [Int]
data Transformation a = Transformation String (a -> a)
data (Term a)         = Term (Transformation a) Interaction
newtype (Expr a)      = Expr [Term a]

newtype Reg a = Reg {_unReg :: a} deriving Show

instance Num a => Num (Reg a) where
  (Reg x) + (Reg y) = Reg (x+y)
  (Reg x) * (Reg y) = Reg (x*y)
  abs (Reg x)       = Reg (abs x)
  signum (Reg x)    = Reg (signum x)
  fromInteger x     = Reg (fromInteger x)
  negate (Reg x)    = Reg (negate x)


itTimesDefault p xs (Strength is) = coerce . fold $ zipWith stimesMonoid is (map p xs)
itAddDefault p xs                 = coerce $ foldMap p xs

class IT a where
  itTimes  :: [a] -> Interaction -> a
  itAdd    :: [a] -> a
  itWeight :: Double -> a -> a

evalTerm :: IT a => [a] -> Term a -> a
evalTerm xs (Term (Transformation _ f) is) = f (itTimes xs is)

evalExprToList :: (IT a) => [a] -> Expr a -> [a]
evalExprToList xs (Expr es) = map (evalTerm xs) es

evalExpr :: (IT a) => [a] -> Expr a -> [Double] -> a
evalExpr xs (Expr es) ws = itAdd $ zipWith itWeight ws (map (evalTerm xs) es)

instance Floating a => IT (Reg a) where
  itTimes = itTimesDefault Product
  itAdd   = itAddDefault   Sum
  itWeight x (Reg y) = Reg (realToFrac x * y)

-- * 'Show' and 'Eq' instances

instance (Show a) => Show (Expr a) where
  show (Expr es) = intercalate " + " $ map show es

instance Show a => Show (Term a) where
   show (Term tr i)   = show tr ++ "(" ++ show i ++ ")" 

instance Show a => Show (Transformation a) where
  show (Transformation s _) = s

instance Show Interaction where
  show (Strength es) = intercalate "*" $ filter (/="") $ zipWith show' [0..] es
    where show' n 0 = ""
          show' n 1 = 'x' : show n
          show' n e = ('x' : show n) ++ "^(" ++  show e ++ ")"
          
instance Eq (Term a) where
  (Term tr1 (Strength i1)) == (Term tr2 (Strength  i2)) = i1 == i2

instance Eq (Transformation a) where
  (Transformation s1 _) == (Transformation s2 _) = s1==s2
  
uniqueTerms :: Expr a -> Expr a
uniqueTerms (Expr ts) = Expr (nub ts)

hasTerm :: Expr a -> Term a -> Bool
hasTerm (Expr ts) t = t `elem` ts

-----------
tr1, tr2 :: Floating a => Transformation (Reg a)
tr1 = Transformation "sin" (Reg . sin . _unReg)
tr2 = Transformation "id"  id

t1, t2 :: Floating a => Term (Reg a)
t1 = Term tr1 (Strength [1, 3])
t2 = Term tr2 (Strength [0, 2])

e :: Floating a => Expr (Reg a)
e  = Expr [t1,t1,t2]

x = Product 10
y = 0.1*sin (1.0*3.0^3) + 0.3*3.0^2

xs = fmap Reg [1.0, 3.0] :: [Reg Double]
zs = fmap Reg [(1.0 ... 2.0), (-2.0 ...  3.0)] :: [Reg (Interval Double)]
ws = [0.1, 0.3, 0.2]

main = do  
  print $ evalExpr xs e ws
  print $ evalExpr zs e ws
  print y
