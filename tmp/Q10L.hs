{-# LANGUAGE MultiWayIf #-}
module Q10L where

import           Control.Monad.ST    (stToIO)
import           Control.Monad.State
import           Data.STRef          (newSTRef, readSTRef, writeSTRef)
import           Prelude             hiding (gcd)

gcd x y
  | x == y = x
  | x < y = gcd x (y - x)
  | x > y = gcd (x - y) y

gcdM :: State (Int, Int) Int
gcdM = do
  (x,y) <- get
  if
    | x == y -> pure x
    | x < y -> put (x, y-x) >> gcdM
    | x > y -> put (x-y, y) >> gcdM

runGcdM = evalState gcdM

gcdST r = do
  (x,y) <- readSTRef r
  if
    | x == y -> pure x
    | x < y -> writeSTRef r (x, y-x) >> gcdST r
    | x > y -> writeSTRef r (x-y, y) >> gcdST r

runGcdST x = stToIO $ newSTRef x >>= gcdST
