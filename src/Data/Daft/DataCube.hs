{-# LANGUAGE TypeFamilies              #-}


module Data.Daft.DataCube (
-- * Types
  DataCube(..)
, Rekeyer(..)
, Gregator(..)
, Joiner(..)
) where


import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Exts (IsList(Item))

import qualified Data.Set as S (null, size)
import qualified GHC.Exts as L (IsList(..))


class DataCube (cube :: * -> * -> *) where

  evaluate :: Ord k => cube k v -> k -> Maybe v

  evaluable :: Ord k => cube k v -> k -> Bool
  evaluable = (isJust .) . evaluate

  select :: (v -> Bool) -> cube k v -> cube k v
  select = selectWithKey . const

  selectWithKey :: (k -> v -> Bool) -> cube k v -> cube k v

  selectKeys :: (k -> Bool) -> cube k v -> cube k v
  selectKeys = selectWithKey . (const .)

  selectRange :: Ord k => Maybe k -> Maybe k -> cube k v -> cube k v

  toTable :: (IsList ks, k ~ Item ks, IsList as, a ~ Item as, Ord k) => (k -> v -> a) -> ks -> cube k v -> as
  toTable combiner ks cube = L.fromList . mapMaybe (evaluate $ projectWithKey combiner cube) $ L.toList ks

  knownKeys :: cube k v -> Set k

  withKnown :: cube k v -> (Set k -> cube k v -> r) -> r
  withKnown cube f = f (knownKeys cube) cube

  knownSize :: cube k v -> Int
  knownSize = S.size . knownKeys

  knownEmpty :: cube k v -> Bool
  knownEmpty = S.null . knownKeys

  selectKnownMinimum :: Ord k => cube k v -> Maybe (k, v)

  selectKnownMaximum :: Ord k => cube k v -> Maybe (k, v)

  project :: (v -> v') -> cube k v -> cube k v'
  project = projectWithKey . const

  projectWithKey :: (k -> v -> v') -> cube k v -> cube k v'

  projectKnownKeys :: (IsList ks', k' ~ Item ks') => (k -> k') -> cube k v -> ks'

  rekey :: Ord k' => Rekeyer k k' -> cube k v -> cube k' v

  aggregate :: Ord k' => Gregator k k' -> ([v] -> v') -> cube k v -> cube k' v'
  aggregate = (. const) . aggregateWithKey

  aggregateWithKey :: Ord k' => Gregator k k' -> (k' -> [v] -> v') -> cube k v -> cube k' v'

  disaggregate :: Ord k' => Gregator k' k -> (v -> v') -> cube k v -> cube k' v'
  disaggregate = (. const) . disaggregateWithKey
  
  disaggregateWithKey :: Ord k' => Gregator k' k -> (k' -> v -> v') -> cube k v -> cube k' v'

  joinSelf :: (Typeable k1, Typeable k2, Typeable k3, Typeable v1, Typeable v2, Typeable v3, Ord k1, Ord k2, Ord k3) => Joiner k1 k2 k3 -> (v1 -> v2 -> v3) -> cube k1 v1 -> cube k2 v2 -> cube k3 v3


data Rekeyer a b =
  Rekeyer
  {
    rekeyer   :: a -> b
  , unrekeyer :: b -> a
  }


data Gregator a b =
  Gregator
  {
    aggregator    :: a -> b
  , disaggregator :: b -> [a]
  }


data Joiner kLeft kRight k =
  Joiner
  {
    joiner    :: kLeft -> kRight -> Maybe k
  , castLeft  :: k -> kLeft
  , castRight :: k -> kRight
  }
