{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}


module Data.Daft.DataCube (
-- * Types
  DataCube(..)
, Rekeyer(..)
, Gregator(..)
, Joiner(..)
) where


import Control.Arrow ((&&&))
import Data.Maybe (isJust, mapMaybe)
import GHC.Exts (Constraint)


class DataCube cube k where

  type Key k :: Constraint

  cmap :: (v -> v') -> cube k v -> cube k v'

  cempty :: Key k => cube k v

  cappend :: Key k => cube k v -> cube k v -> cube k v

  evaluate :: Key k => cube k v -> k -> Maybe v

  evaluable :: Key k => cube k v -> k -> Bool
  evaluable = (isJust .) . evaluate

  select :: (v -> Bool) -> cube k v -> cube k v
  select = selectWithKey . const

  selectWithKey :: (k -> v -> Bool) -> cube k v -> cube k v

  selectKeys :: (k -> Bool) -> cube k v -> cube k v
  selectKeys = selectWithKey . (const .)

  selectRange :: Ord k => Maybe k -> Maybe k -> cube k v -> cube k v

  toList :: Key k => (k -> v -> a) -> [k] -> cube k v -> [a]
  toList combiner ks cube = mapMaybe (evaluate $ projectWithKey combiner cube) ks

  knownKeys :: cube k v -> [k]

  withKnown :: cube k v -> ([k] -> cube k v -> r) -> r
  withKnown cube f = f (knownKeys cube) cube

  knownSize :: cube k v -> Int
  knownSize = length . knownKeys

  knownEmpty :: cube k v -> Bool
  knownEmpty = null . knownKeys

  toKnownList :: Key k => (k -> v -> a) -> cube k v -> [a]
  toKnownList f = uncurry (toList f) . (knownKeys &&& id)

  selectKnownMinimum :: Ord k => cube k v -> Maybe (k, v)

  selectKnownMaximum :: Ord k => cube k v -> Maybe (k, v)

  project :: (v -> v') -> cube k v -> cube k v'
  project = projectWithKey . const

  projectWithKey :: (k -> v -> v') -> cube k v -> cube k v'

  projectKnownKeys :: (k -> k') -> cube k v -> [k']

  rekey :: Key k' => Rekeyer k k' -> cube k v -> cube k' v

  aggregate :: Key k' => Gregator k k' -> ([v] -> v') -> cube k v -> cube k' v'
  aggregate = (. const) . aggregateWithKey

  aggregateWithKey :: Key k' => Gregator k k' -> (k' -> [v] -> v') -> cube k v -> cube k' v'

  disaggregate :: Key k' => Gregator k' k -> (v -> v') -> cube k v -> cube k' v'
  disaggregate = (. const) . disaggregateWithKey
  
  disaggregateWithKey :: Key k' => Gregator k' k -> (k' -> v -> v') -> cube k v -> cube k' v'

  joinSelf :: (Key k1, Key k2, Key k3) => Joiner k1 k2 k3 -> (v1 -> v2 -> v3) -> cube k1 v1 -> cube k2 v2 -> cube k3 v3


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
