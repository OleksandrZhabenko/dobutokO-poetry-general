-- |
-- Module      :  DobutokO.Poetry.StrictV
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less words (or their concatenations) 
-- to obtain (to some extent) suitable for poetry or music text. 

{-# LANGUAGE CPP, BangPatterns #-}

module DobutokO.Poetry.StrictV where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=804
/* code that applies only to GHC 8.4.* and higher versions */
import Data.Semigroup ((<>))
import Prelude hiding ((<>))
#endif
#endif

import qualified Data.Vector as V
import qualified Data.List as L (permutations)

-- | Given a 'String' consisting of no more than 7 words [some of them can be created by concatenation with preserving the 
-- pronunciation of the parts, e. g. \"так як\" (actually two correct Ukrainian words and a single conjunction) can be written \"такйак\" 
-- (one phonetical Ukrainian word transformed literally with preserving phonetical structure), if you would not like to treat them separately], 
-- it returns a 'V.Vector' of possible combinations without repeating of the words in different order and for each of them appends also 
-- the information about \"uniqueness periods\" (see @uniqueness-periods-general@ and @uniqueness-periods@ packages) to it and finds out 
-- three different metrics -- named \"norms\". 
-- 
-- Afterwards, depending on these norms some phonetical properties of the words can be specified that 
-- allow to use them poetically or to create a varied melody with them. 
uniquenessVariants2GN :: V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> V.Vector ([Int],V.Vector Int, String)
uniquenessVariants2GN vN g !xs = uniquenessVariants2GNP [] [] vN g xs 
{-# INLINE uniquenessVariants2GN #-}
        
-- | Generalized variant of 'uniquenessVariants2GN' with prepending and appending 'String' (given as the first and the second argument). 
uniquenessVariants2GNP :: String -> String -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> V.Vector ([Int],V.Vector Int, String)
uniquenessVariants2GNP !ts !us vN g !xs 
  | null . words $ xs = V.empty
  | otherwise = let !v0 = V.fromList . take 8 . words $ xs in
     V.fromList . map ((\vs -> let !rs = g vs in (rs, (V.map (\f -> f rs) vN), vs)) . unwords . preAppend ts [us] . V.toList . 
       V.backpermute v0 . V.fromList) . L.permutations $ ([0..(V.length v0 - 1)]::[Int])


preAppend :: [a] -> [[a]] -> [[a]] -> [[a]]
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=804
preAppend ts !uss tss = ts:tss <> uss
#endif
#endif
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__<804
preAppend ts !uss tss = ts:tss ++ uss
#endif
#endif
{-# INLINE preAppend #-}
