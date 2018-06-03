{-# LANGUAGE DeriveGeneric, DeriveTraversable, Rank2Types #-}
module Data.Diff where

import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

data Move = L | R | B deriving (Show, Read, Eq, Ord, Generic)

diffV :: Eq a => V.Vector a -> V.Vector a -> [(Move, a)]
diffV vecL vecR = trace 0 0 $ go
  $ Map.singleton (0 :: Int, 0 :: Int) (lenL - 1, lenR - 1, [])
  where
    go xs = either id (go . Map.unionsWith merge)
      $ traverse step $ Map.toList xs
    merge p@(a,b,_) q@(c,d,_)
      | a + b < c + d = p
      | otherwise = q
    step ((r, c), (i, j, trail))
      | i < 0 && j < 0 = Left trail
      | j < 0 = Right tl
      | i < 0 = Right tr
      | getL i == getR j = Right
        $ Map.singleton (r, c) $ diag (i - 1) (j - 1) (B : trail)
      | otherwise = Right $ Map.union tl tr
      where
        tl = Map.singleton (r, c + 1) (i - 1, j, L : trail)
        tr = Map.singleton (r + 1, c) (i, j - 1, R : trail)

    diag i j trail
      | i < 0 || j < 0 || getL i /= getR j = (i, j, trail)
      | otherwise = diag (i - 1) (j - 1) (B : trail)

    getL = V.unsafeIndex vecL
    getR = V.unsafeIndex vecR
    lenL = V.length vecL
    lenR = V.length vecR

    trace i j (B : ms) = (B, getL i) : trace (i + 1) (j + 1) ms
    trace i j (L : ms) = (L, getL i) : trace (i + 1) j ms
    trace i j (R : ms) = (R, getR j) : trace i (j + 1) ms
    trace _ _ [] = []

diff :: Eq a => [a] -> [a] -> [(Move, a)]
diff xs ys = diffV (V.fromList xs) (V.fromList ys)
