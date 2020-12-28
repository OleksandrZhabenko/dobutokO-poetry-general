-- |
-- Module      :  DobutokO.Poetry.Norms.Extended
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less Ukrainian words (or their concatenations) 
-- to obtain (to some extent) suitable for poetry or music text. The norms here 
-- are more complex and/or use other ones and can be controlled by some additional 
-- parameters.

module DobutokO.Poetry.Norms.Extended (
  norm7
  , norm7n
  , norm54n
  , norm45n
) where

import qualified Data.Vector as V
import Data.List ((\\),sort)
import DobutokO.Poetry.Norms

-- | The seventh norm 'norm7' has more complex behaviour. The possible range of the first argument is [1..100]. Is provided for exploration.
norm7 :: Int -> [Int] -> Int
norm7 l xs 
 | null xs = 0
 | otherwise = 
    let n = ((abs l `rem` 101) * length xs) `quot` 10
        m = abs ((length xs `quot` 3) - (n `quot` 2))
        ys = if compare (minimum xs) 1 /= GT then filter (\t -> compare t 1 == GT) xs else xs \\ [minimum xs] in 
     if null ys then sum xs else (n * sum xs) `quot` ((sum . take n . sort $ xs) + m * maximum xs)
{-# INLINE norm7 #-}   

-- | The seventh norm 'norm7n' (the variant of the 'norm7' without the maximum evaluation) has more complex behaviour. The possible range of the 
-- first argument is [1..100]. Is provided for exploration.
norm7n :: Int -> [Int] -> Int
norm7n l xs 
 | null xs = 0
 | otherwise = 
    let n = ((abs l `rem` 101) * length xs) `quot` 10 
        ys = if compare (minimum xs) 1 /= GT then filter (\t -> compare t 1 == GT) xs else xs \\ [minimum xs] in 
     if null ys then sum xs else (n * sum xs) `quot` (sum . take n . sort $ xs)
{-# INLINE norm7n #-}   

-- | The fifth-fourth norm for the list of non-negative 'Int'. Is provided for exploration.
norm54n :: Int -> Int -> [Int] -> Int
norm54n n m xs = (norm513 xs ^ n * norm6r xs) `quot` (norm4 xs ^ m)
{-# INLINE norm54n #-}

-- | The fourth-fifth norm for the list of non-negative 'Int'. Is provided for exploration.
norm45n :: Int -> Int -> [Int] -> Int
norm45n n m xs = (norm4 xs ^ n * norm6r xs) `quot` (norm513 xs ^ m)
{-# INLINE norm45n #-}

