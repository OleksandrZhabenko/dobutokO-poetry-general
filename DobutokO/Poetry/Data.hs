-- |
-- Module      :  DobutokO.Poetry.Data
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less (in the first version Ukrainian) words (or their concatenations) 
-- to obtain (to some extent) suitable for poetry or music text. 
-- This module contains data types needed for some generalizations.
-- 

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module DobutokO.Poetry.Data where

import Data.Maybe (fromJust)
import Data.Char (isPunctuation)
import qualified Data.Vector as V
import DobutokO.Poetry.Auxiliary (lastFrom3)

type Uniqueness = ([Int],V.Vector Int,String)

-- | The list in the 'PA' variant represent the prepending 'String' and the postpending one respectively. 'K' constuctor actually means no prepending and 
-- postpending of the text. Are used basically to control the behaviour of the functions.
data PreApp a = K | PA [a] [a] deriving Eq

class G1 a b where
  get1m :: a -> [b]
  get2m :: a -> [b]
  getm :: Bool -> a -> [b]
  getm True = get1m
  getm _ = get2m
  preapp :: a -> [[b]] -> [[b]]
  setm :: [b] -> [b] -> a
  
instance G1 (PreApp Char) Char where
  get1m K = []
  get1m (PA xs _) = xs
  get2m K = []
  get2m (PA _ ys) = ys
  preapp K xss = xss
  preapp (PA xs ys) yss = xs:yss ++ [ys]
  setm [] [] = K
  setm xs ys = PA xs ys

type Preapp = PreApp Char

isPA :: PreApp a -> Bool
isPA K = False
isPA _ = True

isK :: PreApp a -> Bool
isK K = True
isK _ = False

-- | Is used to control whether to return data or only to print the needed information. The 'U' contstuctor corresponds to the information printing and 'UL' to 
-- returning also data. The last one so can be further used.
data UniquenessG a b = U b | UL ([a],b) deriving Eq

instance Show (UniquenessG String (V.Vector Uniqueness)) where
  show (U v) = show . V.map (filter (not . isPunctuation) . lastFrom3) $ v
  show (UL (wss,_)) = show . map (filter (not . isPunctuation)) $ wss

type UniqG = UniquenessG String (V.Vector Uniqueness)  

-- | Decomposes the data type 'UniqG' into its components. The inverse to the 'set2'.
get2 :: UniqG -> (Maybe [String], V.Vector Uniqueness)
get2 (U v) = (Nothing,v)
get2 (UL (wss,v)) = (Just wss,v)

-- | Compose the data type 'UniqG' from its components. The inverse to the 'get2'.
set2 :: (Maybe [String], V.Vector Uniqueness) -> UniqG
set2 (Just wss, v) = UL (wss,v)
set2 (Nothing, v) = U v

isU :: UniqG -> Bool
isU (U _) = True
isU _ = False

isUL :: UniqG -> Bool
isUL (UL _) = True
isUL _ = False

