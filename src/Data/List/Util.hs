{-# LANGUAGE Safe #-}


module Data.List.Util (
  elemPermutation
) where


import Data.List (elemIndex)


-- Find the permutation of one list relative to another.
elemPermutation :: Eq a => [a] -> [a] -> Maybe [Int]
elemPermutation = mapM . flip elemIndex
