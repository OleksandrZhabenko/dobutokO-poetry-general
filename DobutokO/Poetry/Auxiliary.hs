-- |
-- Module      :  DobutokO.Poetry.Auxiliary
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less (in the first version Ukrainian) words (or their concatenations) 
-- to obtain (to some extent) suitable for poetry or music text. 
-- Similar functionality is provided by the packages MissingH, extra, ghc 
-- and other packages, but they have a lot of dependencies, so here there are 
-- lessel dependencies module and package.

module DobutokO.Poetry.Auxiliary (
  -- * Help functions
  lastFrom3
  , firstFrom3
  , secondFrom3
) where

lastFrom3 :: (a,b,c) -> c
lastFrom3 (_,_,z) = z
{-# INLINE lastFrom3 #-}

firstFrom3 :: (a, b, c) -> a
firstFrom3 (x, _, _) = x
{-# INLINE firstFrom3 #-}

secondFrom3 :: (a, b, c) -> b
secondFrom3 (_, y, _) = y
{-# INLINE secondFrom3 #-}
