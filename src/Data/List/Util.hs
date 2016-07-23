{-# LANGUAGE Safe #-}


module Data.List.Util (
  elemPermutation
, groupOn
, nubOn
, sortOn
, sortedGroups
, sortedGroupsOn
) where


import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (elemIndex, groupBy, nubBy, sortBy)


-- Find the permutation of one list relative to another.
elemPermutation :: Eq a => [a] -> [a] -> Maybe [Int]
elemPermutation = mapM . flip elemIndex


groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f) 


nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = nubBy ((==) `on` f)


sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)


sortedGroups :: Ord b => [(b, c)] -> [(b, [c])]
sortedGroups = sortedGroupsOn fst snd 


sortedGroupsOn :: Ord b => (a -> b) -> (a -> c) -> [a] -> [(b, [c])]
sortedGroupsOn f g =
  fmap ((f . head) &&& fmap g)
    . groupOn f
    . sortOn f
