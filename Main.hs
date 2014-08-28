{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.List
import qualified Data.Vector as V
import Control.DeepSeq.TH
import Criterion.Main
import Foreign.C

foreign import ccall "hugo.h mandyloop" mandyloop :: CInt -> CDouble -> CDouble -> CDouble -> CDouble

--data Val = Val {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Eq, Show)
data Val = Val !Double !Double !Double deriving (Eq, Show)

deriveNFData ''Val

f :: Val -> Val 
f (Val x y z) = Val (x*x - y*y) (2*x*y) (z+1)
{-# INLINE f #-}

g :: Val -> Val
g (Val x y z) = Val (x+0.01) (y+0.1) z 
{-# INLINE g #-}

h :: Val -> Val
h = (f.g)  
{-# INLINE h #-}

iter1 :: Int -> (a->a) -> a -> a

iter1 n f s = foldl' (\x y -> f x) s (take n [1..] :: [Int])
benchIter1 = iter1 benchN h 

iter2 :: Int -> (a->a) -> a -> a
iter2 0 f !v = f v 
iter2 !n !f !s = iter2 (n-1) f (f s)
benchIter2 = iter2 benchN h

benchIter3 = V.iterateN benchN h  

fd :: (Double, Double, Double) -> (Double, Double, Double)
fd (x,y,z) = (x*x - y*y, 2*x*y, z+1)
gd :: (Double, Double, Double) -> (Double, Double, Double)
gd (x,y,z) = (x+0.01, y+0.1, z)
hd :: (Double, Double, Double) -> (Double, Double, Double)
hd = fd . gd

benchIter4 = V.iterateN benchN hd 

mainmain = do 
  let result = iter1 benchN h (Val 0.0 0.0 0.0)
  print result 
  let v = V.iterateN benchN h  (Val 0.0 0.0 0.0)
  print (V.last v)

mandybench :: Int -> CDouble
mandybench n =  mandyloop (fromIntegral n) 0.0 0.0 0.0 
benchN :: Int
benchN = 500000

benchS :: Val
benchS = Val 0.0 0.0 0.0

main = defaultMain  
  [ bench "iter1 folding " $ nf benchIter1 benchS
  , bench "iter2 recursing" $ nf benchIter2 benchS
  , bench "iter3 vectors" $ nf benchIter3 benchS
  , bench "iter3 vectors of tuples" $ nf benchIter4 ((0.0,0.0,0.0) :: (Double, Double, Double))
  , bench "mandybench C code" $ whnf mandybench benchN 
  ]

