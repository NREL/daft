-----------------------------------------------------------------------------
--
-- Module      :  Data.Daft.MapReduce
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Various map-reduce functions.
--
-----------------------------------------------------------------------------


{-# LANGUAGE Safe #-}


module Data.Daft.MapReduce (
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


import Data.List.Util (sortedGroups)

import qualified Data.Daft.MapReduce.Internal as I


-- | Reduce values by group.
groupReduce :: Ord k
            => (a -> k)   -- ^ Function for extracting keys for grouping.
            -> ([a] -> b) -- ^ Function for reducing values.
            -> [a]        -- ^ The values.
            -> [b]        -- ^ The reduced values.
groupReduce = I.groupReduce mapReduce


-- | Reduce values by group, and flatten the result.
groupReduceFlatten :: Ord k
                   => (a -> k)     -- ^ Function for extracting keys for grouping.
                   -> ([a] -> [b]) -- ^ Function for reducing values.
                   -> [a]          -- ^ The values.
                   -> [b]          -- ^ The reduced values.
groupReduceFlatten = I.groupReduceFlatten mapReduce


-- | Reduce values by group, where the reducer receives the key.
groupReduceByKey :: Ord k
                 => (a -> k)        -- ^ Function for extracting keys for grouping.
                 -> (k -> [a] -> b) -- ^ Function for reducing values.
                 -> [a]             -- ^ The values.
                 -> [b]             -- ^ The reduced values.
groupReduceByKey = I.groupReduceByKey mapReduce


-- | Reduce values by group, and flatten the result, where the reducer receives the key.
groupReduceFlattenByKey :: Ord k
                        => (a -> k)          -- ^ Function for extracting keys for grouping.
                        -> (k -> [a] -> [b]) -- ^ Function for reducing values.
                        -> [a]               -- ^ The values.
                        -> [b]               -- ^ The reduced values.
groupReduceFlattenByKey = I.groupReduceFlattenByKey mapReduce


-- | Order and extract values.
groupExtract :: Ord k
             => (a -> k) -- ^ Function for extracting keys for grouping.
             -> (a -> b) -- ^ Function for extracting values.
             -> [a]      -- ^ The values.
             -> [b]      -- ^ The extract.
groupExtract = I.groupExtract mapReduce


-- | Reduce values by keying them.
mapReduce :: Ord k
          => (a -> (k, v))   -- ^ Function for mapping to keys and values.
          -> (k -> [v] -> b) -- ^ Function for reducing values.
          -> [a]             -- ^ The values.
          -> [b]             -- ^ The reduced values.
mapReduce mapper reducer =
  map (uncurry reducer)
    . sortedGroups
    . map mapper


-- | Reduce values by keying them, and flatten the result.
mapReduceFlatten :: Ord k
                 => (a -> (k, v))     -- ^ Function for mapping to keys and values.
                 -> (k -> [v] -> [b]) -- ^ Function for reducing values.
                 -> [a]               -- ^ The values.
                 -> [b]               -- ^ The reduced values.
mapReduceFlatten = I.mapReduceFlatten mapReduce


-- | Reduce values by keying them, transforming the result.
mapReduceFinalize :: Ord k
                  => (a -> (k, v))   -- ^ Function for mapping to keys and values.
                  -> (k -> [v] -> v) -- ^ Function for reducing values.
                  -> (k -> v -> b)   -- ^ Function for transforming the reduced values.
                  -> [a]             -- ^ The values.
                  -> [b]             -- ^ The reduced values.
mapReduceFinalize = I.mapReduceFinalize mapReduce


-- | Aggregate values.
aggregate :: (a -> v)   -- ^ Function to extract values.
          -> ([v] -> b) -- ^ Function to aggregate a list of values.
          -> [a]        -- ^ The values.
          -> b          -- ^ The aggregate.
aggregate = I.aggregate mapReduce
{- FIXME: The following probably performs better?
  flip (.) . map
-}


-- | Aggregate values by key.
aggregateByKey :: Ord k
               => (a -> k)   -- ^ Function for extracting keys.
               -> (a -> v)   -- ^ Function for extracting values.
               -> ([v] -> b) -- ^ Function to aggregate a list of values.
               -> [a]        -- ^ The values.
               -> [b]        -- ^ The aggregates.
aggregateByKey = I.aggregateByKey mapReduce


-- | Aggregate values by key.
aggregateWithKey :: Ord k
                 => (a -> k)        -- ^ Function for extracting keys.
                 -> (a -> v)        -- ^ Function for extracting values.
                 -> (k -> [v] -> b) -- ^ Function to aggregate a list of values.
                 -> [a]             -- ^ The values.
                 -> [b]             -- ^ The aggregates.
aggregateWithKey = I.aggregateWithKey mapReduce
