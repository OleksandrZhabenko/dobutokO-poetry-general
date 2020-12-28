-- |
-- Module      :  DobutokO.Poetry.Norms
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less (in the first version the Ukrainian) words (or their concatenations) 
-- to obtain (to some extent) suitable for poetry or music text. This module 
-- provides several different norms that allow to research the text and 
-- to create interesting sequences.

module DobutokO.Poetry.Norms (
  -- * Different norms
  norm1
  , norm2
  , norm3
  , norm4
  , norm5
  , norm51
  , norm513
  , norm54
  , norm6
  , norm6r
  , norm8
  -- * Norms combining
  , splitNorm
  -- ** More complex combining
  , combineNorms
  , flSplitNorms
  , normSplit
  , normSplitWeighted
  , normSplitShifted
  , normSplitWeightedG
) where

import qualified Data.Vector as V
import Data.List ((\\))

-- | The first norm for the list of non-negative 'Int'. For not empty lists equals to the maximum element.
norm1 :: [Int] -> Int 
norm1 xs 
  | null xs = 0
  | otherwise = maximum xs
{-# INLINE norm1 #-}

-- | The second norm for the list of non-negative 'Int'. For not empty lists equals to the sum of the elements.
norm2 :: [Int] -> Int
norm2 xs = sum xs
{-# INLINE norm2 #-}

-- | The third norm for the list of non-negative 'Int'. For not empty lists equals to the sum of the doubled maximum element and the rest elements of the list.
norm3 :: [Int] -> Int
norm3 xs 
 | null xs = 0
 | otherwise = maximum xs + sum xs
{-# INLINE norm3 #-} 

-- | The fourth norm for the list of non-negative 'Int'. Equals to the sum of the 'norm3' and 'norm2'.
norm4 :: [Int] -> Int
norm4 xs 
 | null xs = 0
 | length xs == 1 = head xs
 | otherwise = maximum xs + sum xs + maximum (xs \\ [maximum xs])
{-# INLINE norm4 #-} 

-- | The fifth norm for the list of non-negative 'Int'. For not empty lists equals to the sum of the elements quoted with sum of the two most minimum elements.
norm5 :: [Int] -> Int
norm5 xs 
 | null xs = 0
 | length xs == 1 = head xs
 | minimum xs == 0 = norm5 . filter (/= 0) $ xs
 | otherwise = sum xs `quot` (minimum xs + minimum (xs \\ [minimum xs]))
{-# INLINE norm5 #-} 

-- | The fifth modified norm for the list of non-negative 'Int'. Tries to take into account doubled and prolonged sounds to reduce their influence on the 'norm5'.
norm51 :: [Int] -> Int
norm51 xs 
 | null xs = 0
 | compare (minimum xs) 1 /= GT = let ys = filter (\t -> compare t 1 == GT) xs in 
    case length ys of
      0 -> sum xs
      1 -> (3 * sum xs) `quot` (minimum ys + maximum ys)
      _ -> (3 * sum xs) `quot` (minimum ys + minimum (ys \\ [minimum ys]))
 | length xs == 1 = 1
 | otherwise = (3 * sum xs) `quot` (minimum xs + minimum (xs \\ [minimum xs]))
{-# INLINE norm51 #-} 

-- | The fifth modified (with three minimums) norm for the list of non-negative 'Int'. Tries to take into account doubled and prolonged sounds 
-- to reduce their influence on the 'norm5'.
norm513 :: [Int] -> Int
norm513 xs 
 | null xs = 0
 | compare (minimum xs) 1 /= GT = 
   let ys = filter (\t -> compare t 1 == GT) xs in 
     case length ys of 
       0 -> sum xs
       1 -> (3 * sum xs) `quot` (minimum ys + maximum ys)
       2 -> let zs = ys \\ [minimum ys] in (3 * sum xs) `quot` (minimum ys + 2 * minimum zs)
       _ -> let zs = ys \\ [minimum ys] in (3 * sum xs) `quot` (minimum ys + minimum zs + minimum (zs \\ [minimum zs]))
 | otherwise = 
   let zs = xs \\ [minimum xs] in 
     case length zs of 
       0 -> sum xs
       1 -> (3 * sum xs) `quot` (minimum zs + maximum zs)
       2 -> let ts = zs \\ [minimum zs] in (3 * sum xs) `quot` (minimum zs + 2 * minimum ts)
       _ -> let ts = zs \\ [minimum zs] in (3 * sum xs) `quot` (minimum zs + minimum ts + minimum (ts \\ [minimum ts]))
{-# INLINE norm513 #-}   

-- | The sixth norm for the list of non-negative 'Int'.
norm6 :: [Int] -> Int
norm6 xs = (norm5 xs * sum xs) `quot` norm3 xs
{-# INLINE norm6 #-}

-- | The sixth modified norm for the list of non-negative 'Int'.
norm6r :: [Int] -> Int
norm6r xs = (norm513 xs * norm3 xs) `quot` sum xs
{-# INLINE norm6r #-}

-- | The fifth-fourth norm for the list of non-negative 'Int'. Tries to generate more suitable for poetry text.
norm54 :: [Int] -> Int
norm54 xs = (norm513 xs * norm6r xs) `quot` norm4 xs
{-# INLINE norm54 #-}

-- | The eigth norm for the list of the non-negative 'Int'. Similarly to 'norm4' is used to quickly evaluate the possibly more diverse texts, 
-- which are not simple to quickly pronounce. Is not informative (and therefore, is not intended to be used) for the lists of no more than 1 element.
norm8 :: [Int] -> Int
norm8 xs 
 | null xs = 0
 | length xs == 1 = 1
 | otherwise =  (5040 * maximum xs) `quot` minimum xs
{-# INLINE norm8 #-} 

-- | Splits a given list of non-negative integers into lists of elements not equal to zero and then applies to them the norms from the 'V.Vector' starting 
-- from the last element in the vector right-to-left.
splitNorm :: [Int] -> V.Vector ([Int] -> Int) -> [Int]
splitNorm xs vN 
 | null (filter (/=0) xs) || V.length vN /= length (filter (== 0) xs) + 1 = []
 | otherwise = 
    let (ys,zs) = break (== 0) xs 
        zzs = drop 1 zs
        in (V.unsafeIndex vN (V.length vN - 1)) ys:splitNorm zzs (V.unsafeSlice 0 (V.length vN - 1) vN)
        
-- | Applies all the given norms in the 'V.Vector' to the first argument and collects the result as a list.
combineNorms :: [Int] -> V.Vector ([Int] -> Int) -> [Int]
combineNorms xs vN 
 | null xs = []
 | otherwise = V.toList . V.map (\f -> f xs) $ vN

-- | The flipped variant of the 'combineNorms' (can be more convenient for applications).
flSplitNorms :: V.Vector ([Int] -> Int) -> [Int] -> [Int]
flSplitNorms = flip combineNorms
{-# INLINE flSplitNorms #-}

-- | Using 'combineNorms' and given a group of norms (represented as a 'V.Vector') returns the sum of their applications. These norms are equally important 
-- and their order can be volatile.
normSplit :: V.Vector ([Int] -> Int) -> ([Int] -> Int)
normSplit v = sum . flSplitNorms v
{-# INLINE normSplit #-}

-- | Using 'combineNorms' and given a group of norms (represented as a 'V.Vector') returns the sum of their applications. The importance of these norms can be 
-- easily controlled by the first function argument. The corresponding greater numbers in the first argument list signify the greater importance of the 
-- norm in the 'V.Vector' of norms all being applied (so these numbers are multiplicative weights in the sum). If some of the numbers are equal to zero then 
-- the corresponding norm is not taken into account. If some of the numbers are negative then the corresponding norms are weightly subtracted from the sum.
normSplitWeighted :: [Int] -> V.Vector ([Int] -> Int) -> ([Int] -> Int)
normSplitWeighted = normSplitWeightedG (*)
{-# INLINE normSplitWeighted #-}

-- | Similar to 'normSplitWeighted', but uses more complex modification instead of multiplication. 
-- Using 'splitNorm' and given a group of norms (represented as a 'V.Vector') returns the sum of their applications. The importance of these norms can be 
-- controlled by the first function argument. The corresponding greater numbers in the first argument list signify the greater importance of the 
-- norm in the 'V.Vector' of norms all being applied. If some of the numbers are negative then the corresponding norms reduces the sum in some way.
normSplitShifted :: [Int] -> V.Vector ([Int] -> Int) -> ([Int] -> Int)
normSplitShifted = normSplitWeightedG (\x y -> x * (x + y))
{-# INLINE normSplitShifted #-}

-- | Generalization for the 'normSplitWeighted' and 'normSplitShifted' with the possibility to define and use the volatile function that influences the 
-- weights for the norms in the 'V.Vector'. This function used in the 'zipWith' is given as the first argument.
normSplitWeightedG :: (Int -> Int -> Int) -> [Int] -> V.Vector ([Int] -> Int) -> ([Int] -> Int)
normSplitWeightedG h xs v
 | null xs = \ys -> 0
 | otherwise = let g ts = zipWith h (flSplitNorms v ts) xs in sum . g
