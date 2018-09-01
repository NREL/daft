{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeFamilies     #-}


module Data.Daft.DataCube.Util (
  knownDomainSet
, knownDomainList
) where


import Data.Daft.DataCube (DataCube(..))
import Data.List (nub)
import Data.Set (Set)
import GHC.Exts (IsList(..))

import qualified Data.Set as S (map)


knownDomainSet :: (DataCube cube, Keys cube ~ Set, Ord k') => (k -> k') -> cube k v -> Set k'
knownDomainSet = (. knownKeys) . S.map


knownDomainList :: (DataCube cube, IsList (Keys cube k), Item (Keys cube k) ~ k, Ord k') => (k -> k') -> cube k v -> [k']
knownDomainList = (nub .) . (. (toList . knownKeys)) . fmap
