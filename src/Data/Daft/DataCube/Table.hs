{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}


module Data.Daft.DataCube.Table (
-- * Types
  TableCube
-- * Conversion
, fromTable
, fromTableM
, toKnownTable
, reify
) where


import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (***))
import Control.Monad (guard)
import Data.Daft.DataCube (DataCube(..), Gregator(..), Joiner(..), Rekeyer(..))
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import GHC.Exts (IsList(Item))

import qualified Data.Map.Strict as M (assocs, filterWithKey, findMin, findMax, fromList, fromListWith, keys, keysSet, lookup, mapKeysWith, mapWithKey, member, null, split, size)
import qualified GHC.Exts as L (IsList(..))


type TableCube = Map


instance DataCube TableCube where

  evaluate = flip M.lookup

  evaluable = flip M.member

  knownKeys = M.keysSet

  knownSize = M.size

  knownEmpty = M.null

  selectWithKey = M.filterWithKey

  selectRange Nothing Nothing     = id
  selectRange (Just k0) Nothing   =                    snd . M.split k0
  selectRange Nothing (Just k1)   = fst . M.split k1
  selectRange (Just k0) (Just k1) = fst . M.split k1 . snd . M.split k0

  selectKnownMinimum table =
    do
      guard . not
        $ M.null table
      return
        $ M.findMin table

  selectKnownMaximum table =
    do
      guard . not
        $ M.null table
      return
        $ M.findMax table

  projectWithKey = M.mapWithKey

  projectKnownKeys = (L.fromList .) . (. M.keys) . fmap

  rekey Rekeyer{..} = M.mapKeysWith (const id) rekeyer

  aggregateWithKey Gregator{..} reducer =
    M.mapWithKey reducer
      . M.fromListWith (++)
      . fmap (aggregator *** return)
      . M.assocs

  disaggregateWithKey Gregator{..} expander =
    M.fromList
      . concatMap (\(k, v) -> map (\k' -> (k', expander k' v)) $ disaggregator k)
      . M.assocs

  joinSelf Joiner{..} combiner table1 table2 =
    M.fromList
      $ catMaybes
      [
        (, v1 `combiner` v2) <$> k1 `joiner` k2
      |
        (k1, v1) <- M.assocs table1
      , (k2, v2) <- M.assocs table2
      ]


fromTable :: (IsList as, a ~ Item as, Ord k) => (a -> k) -> (a -> v) -> as -> TableCube k v
fromTable keyer valuer = M.fromList . fmap (keyer &&& valuer) . L.toList


fromTableM :: (Monad m, IsList as, a ~ Item as, Ord k) => (a -> m k) -> (a -> m v) -> as -> m (TableCube k v)
fromTableM keyer valuer =
  fmap M.fromList
    . mapM (liftA2 (,) . keyer <*> valuer)
    . L.toList


toKnownTable :: (IsList as, a ~ Item as, Ord k) => (k -> v -> a) -> TableCube k v -> as
toKnownTable combiner = L.fromList . map (uncurry combiner) . M.assocs


reify :: DataCube a => (Ord k, IsList ks, k ~ Item ks) => ks -> a k v -> TableCube k v
reify ks cube =
  M.fromList
    $ catMaybes
    [
      (k, ) <$> cube `evaluate` k
    |
      k <- L.toList ks
    ]
