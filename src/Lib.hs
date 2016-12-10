module Lib where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import           Data.Maybe

--
-- A "dice spec" is a pair (m,n) which represents rolling a n-sided dice
-- m times.
--

--
-- `dist` gives you the probability distribution of rolling an n-sided dice
-- m times.
--
dist :: Int -> Int -> IntMap Rational
dist 0 _ = M.fromList [(0,1)]
dist m n = step n (dist (m-1) n)


--
-- Takes a list of dice specs and gives you the combined probability
-- distribution of rolling them all.
--
multiDist :: [(Int,Int)] -> IntMap Rational
multiDist [] = M.fromList [(0,1)]
multiDist ((0,n):rest) = multiDist rest
multiDist ((m,n):rest) = step n (multiDist ((m-1,n):rest))

--
-- `step n distrib` takes a dice with `n` sides and
-- an existing distribution and returns a distribution
-- for the result of rolling the dice with the other
-- "dice" with that distribution
--
step :: Int -> IntMap Rational -> IntMap Rational
step n d = M.foldlWithKey addRolls M.empty d
  where
    addRolls :: IntMap Rational -> Int -> Rational -> IntMap Rational
    addRolls d' i p = foldl ins d' [1..n]
      where
        ins :: IntMap Rational -> Int -> IntMap Rational
        ins m i' = let idx = i + i'
                   in M.insert idx (m `lk` idx + (p * 1/(fromIntegral n))) m

lk ::  IntMap Rational -> Int -> Rational
lk m i = maybe 0 id (M.lookup i m)