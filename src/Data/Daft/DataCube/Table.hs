{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}


module Data.Daft.DataCube.Table (
-- * Types
  TableCube
-- * Conversion
, fromTable
, fromTableM
, reify
) where


import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (***))
import Control.Monad (guard)
import Data.Daft.DataCube (DataCube(..), Gregator(..), Joiner(..), Rekeyer(..))
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)

import qualified Data.Map.Strict as M (assocs, filterWithKey, findMin, findMax, fromList, fromListWith, keys, keysSet, lookup, mapKeysWith, mapWithKey, member, null, split, size)
import qualified Data.Set as S (toList)


type TableCube = Map


instance DataCube TableCube where

  type Key TableCube k = Ord k

  cmap = fmap

  cempty = mempty

  cappend = mappend

  evaluate = flip M.lookup

  evaluable = flip M.member

  knownKeys = S.toList . M.keysSet

  knownSize = M.size

  knownEmpty = M.null

  selectWithKey = M.filterWithKey

  selectRange Nothing Nothing     = id
  selectRange (Just k0) Nothing   =                    snd . M.split k0
  selectRange Nothing (Just k1)   = fst . M.split k1
  selectRange (Just k0) (Just k1) = fst . M.split k1 . snd . M.split k0

  toKnownTable combiner = map (uncurry combiner) . M.assocs

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

  projectKnownKeys = (. M.keys) . fmap

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


fromTable :: Ord k => (a -> k) -> (a -> v) -> [a] -> TableCube k v
fromTable keyer valuer = M.fromList . fmap (keyer &&& valuer)


fromTableM :: (Monad m, Ord k) => (a -> m k) -> (a -> m v) -> [a] -> m (TableCube k v)
fromTableM keyer valuer =
  fmap M.fromList
    . mapM (liftA2 (,) . keyer <*> valuer)


reify :: (DataCube a, Key a k, Ord k) => [k] -> a k v -> TableCube k v
reify ks cube =
  M.fromList
    $ catMaybes
    [
      (k, ) <$> cube `evaluate` k
    |
      k <- ks
    ]
