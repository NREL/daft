-----------------------------------------------------------------------------
--
-- Module      :  Data.Daft.MapReduce.Parallel
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Various map-reduce functions, with parallelism.
--
-----------------------------------------------------------------------------


{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes     #-}


module Data.Daft.MapReduce.Parallel (
-- * Grouping
  groupReduce
, groupReduceFlatten
, groupReduceByKey
, groupReduceFlattenByKey
, groupExtract
-- * Map-Reduce
, mapReduce
, mapReduceFlatten
, mapReduceFinalize
-- * Aggregation
, aggregate
, aggregateByKey
, aggregateWithKey
) where


import Control.Parallel.Strategies (Strategy, parMap)
import Data.List.Util (sortedGroups)

import qualified Data.Daft.MapReduce.Internal as I


-- | Reduce values by group.
groupReduce :: Ord k
            => (forall z . Strategy z) -- ^ The evaluation strategy.
            -> (a -> k)                -- ^ Function for extracting keys for grouping.
            -> ([a] -> b)              -- ^ Function for reducing values.
            -> [a]                     -- ^ The values.
            -> [b]                     -- ^ The reduced values.
{- FIXME: Why can't this be written in pointfree style?
groupReduce = I.groupReduce . mapReduce
-}
groupReduce strategy = I.groupReduce $ mapReduce strategy


-- | Reduce values by group, and flatten the result.
groupReduceFlatten :: Ord k
                   => (forall z . Strategy z) -- ^ The evaluation strategy.
                   -> (a -> k)                -- ^ Function for extracting keys for grouping.
                   -> ([a] -> [b])            -- ^ Function for reducing values.
                   -> [a]                     -- ^ The values.
                   -> [b]                     -- ^ The reduced values.
groupReduceFlatten strategy = I.groupReduceFlatten $ mapReduce strategy


-- | Reduce values by group, where the reducer receives the key.
groupReduceByKey :: Ord k
                 => (forall z . Strategy z) -- ^ The evaluation strategy.
                 -> (a -> k)                -- ^ Function for extracting keys for grouping.
                 -> (k -> [a] -> b)         -- ^ Function for reducing values.
                 -> [a]                     -- ^ The values.
                 -> [b]                     -- ^ The reduced values.
groupReduceByKey strategy = I.groupReduceByKey $ mapReduce strategy


-- | Reduce values by group, and flatten the result, where the reducer receives the key.
groupReduceFlattenByKey :: Ord k
                        => (forall z . Strategy z) -- ^ The evaluation strategy.
                        -> (a -> k)                -- ^ Function for extracting keys for grouping.
                        -> (k -> [a] -> [b])       -- ^ Function for reducing values.
                        -> [a]                     -- ^ The values.
                        -> [b]                     -- ^ The reduced values.
groupReduceFlattenByKey strategy = I.groupReduceFlattenByKey $ mapReduce strategy


-- | Order and extract values.
groupExtract :: Ord k
             => (forall z . Strategy z) -- ^ The evaluation strategy.
             -> (a -> k)                -- ^ Function for extracting keys for grouping.
             -> (a -> b)                -- ^ Function for extracting values.
             -> [a]                     -- ^ The values.
             -> [b]                     -- ^ The extract.
groupExtract strategy = I.groupExtract $ mapReduce strategy


-- | Reduce values by keying them.
mapReduce :: Ord k
          => (forall z . Strategy z) -- ^ The evaluation strategy.
          -> (a -> (k, v))           -- ^ Function for mapping to keys and values.
          -> (k -> [v] -> b)         -- ^ Function for reducing values.
          -> [a]                     -- ^ The values.
          -> [b]                     -- ^ The reduced values.
mapReduce strategy mapper reducer =
  parMap strategy (uncurry reducer)
    . sortedGroups
    . parMap strategy mapper


-- | Reduce values by keying them, and flatten the result.
mapReduceFlatten :: Ord k
                 => (forall z . Strategy z) -- ^ The evaluation strategy.
                 -> (a -> (k, v))           -- ^ Function for mapping to keys and values.
                 -> (k -> [v] -> [b])       -- ^ Function for reducing values.
                 -> [a]                     -- ^ The values.
                 -> [b]                     -- ^ The reduced values.
mapReduceFlatten strategy = I.mapReduceFlatten $ mapReduce strategy


-- | Reduce values by keying them, transforming the result.
mapReduceFinalize :: Ord k
                  => (forall z . Strategy z) -- ^ The evaluation strategy.
                  -> (a -> (k, v))           -- ^ Function for mapping to keys and values.
                  -> (k -> [v] -> v)         -- ^ Function for reducing values.
                  -> (k -> v -> b)           -- ^ Function for transforming the reduced values.
                  -> [a]                     -- ^ The values.
                  -> [b]                     -- ^ The reduced values.
mapReduceFinalize strategy = I.mapReduceFinalize $ mapReduce strategy


-- | Aggregate values.
aggregate :: (forall z . Strategy z) -- ^ The evaluation strategy.
          -> (a -> v)                -- ^ Function to extract values.
          -> ([v] -> b)              -- ^ Function to aggregate a list of values.
          -> [a]                     -- ^ The values.
          -> b                       -- ^ The aggregate.
aggregate strategy = I.aggregate $ mapReduce strategy


-- | Aggregate values by key.
aggregateByKey :: Ord k
               => (forall z . Strategy z) -- ^ The evaluation strategy.
               -> (a -> k)                -- ^ Function for extracting keys.
               -> (a -> v)                -- ^ Function for extracting values.
               -> ([v] -> b)              -- ^ Function to aggregate a list of values.
               -> [a]                     -- ^ The values.
               -> [b]                     -- ^ The aggregates.
aggregateByKey strategy = I.aggregateByKey $ mapReduce strategy


-- | Aggregate values by key.
aggregateWithKey :: Ord k
                 => (forall z . Strategy z) -- ^ The evaluation strategy.
                 -> (a -> k)                -- ^ Function for extracting keys.
                 -> (a -> v)                -- ^ Function for extracting values.
                 -> (k -> [v] -> b)         -- ^ Function to aggregate a list of values.
                 -> [a]                     -- ^ The values.
                 -> [b]                     -- ^ The aggregates.
aggregateWithKey strategy = I.aggregateWithKey $ mapReduce strategy
