{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}


module Data.Daft.Lookup {-# DEPRECATED "This module will be removed in a future release." #-} (
  LookupTable
, keysSet
, keys
, elems
, assocs
, Lookupable(..)
, lookupOrd
, lookupReal
, lookupRealFrac
) where


import Data.Daft.Keyed (Keyed(..), asPair)
import Data.Map.Strict (Map, (!), assocs, elems, fromList, keysSet, keys, lookupLE, lookupGE)


type LookupTable k v = Map k v


class Lookupable a k v | a -> k, a -> v where
  asLookupTable :: Ord k => a -> LookupTable k v


instance Lookupable [(k, v)] k v where
  asLookupTable = fromList


instance Lookupable [Keyed k v] k v where
  asLookupTable = asLookupTable . fmap asPair


lookupOrd :: Ord k => k -> LookupTable k v -> v
lookupOrd = flip (!)


lookupReal :: Real k => k -> LookupTable k v -> v
lookupReal x t =
  case (x `lookupGE` t, x `lookupLE` t) of
    (Nothing      , Nothing      ) -> error "Data.Daft.Lookup.lookupReal: empty table"
    (Just (_ , y0), Nothing      ) -> y0
    (Nothing      , Just (_ , y1)) -> y1
    (Just (x0, y0), Just (x1, y1)) -> if x - x0 > x1 - x
                                        then y0
                                        else y1


lookupRealFrac :: (Real k, RealFrac v) => k -> LookupTable k v -> v
lookupRealFrac x t =
  case (x `lookupGE` t, x `lookupLE` t) of
    (Nothing      , Nothing      ) -> error "Data.Daft.Lookup.lookupRealFrac: empty table"
    (Just (_ , y0), Nothing      ) -> y0
    (Nothing      , Just (_ , y1)) -> y1
    (Just (x0, y0), Just (x1, y1)) -> y0 + realToFrac (x - x0) / realToFrac (x1 - x0) * (y1 - y0)
