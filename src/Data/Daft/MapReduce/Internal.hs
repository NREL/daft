-----------------------------------------------------------------------------
--
-- Module      :  Data.Daft.MapReduce.Internal
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Generic implementation of various map-reduce functions.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE Safe             #-}


module Data.Daft.MapReduce.Internal (
-- * Types
  MapReduce
-- * Grouping
, groupReduce
, groupReduceFlatten
, groupReduceByKey
, groupReduceFlattenByKey
, groupExtract
-- * Map-Reduce
, mapReduceFlatten
, mapReduceFinalize
-- * Aggregation
, aggregate
, aggregateByKey
, aggregateWithKey
) where


import Control.Applicative (liftA2)
import Control.Arrow ((&&&))


-- | Functiom for mapping and then reducing.
type MapReduce a k v b =  Ord k
               => (a -> (k, v))   -- ^ Function for mapping to keys and values.
               -> (k -> [v] -> b) -- ^ Function for reducing values.
               -> [a]             -- ^ The values.
               -> [b]             -- ^ The reduced values.


-- | Reduce values by group.
groupReduce :: Ord k
            => MapReduce a k a b 
            -> (a -> k)          -- ^ Function for extracting keys for grouping.
            -> ([a] -> b)        -- ^ Function for reducing values.
            -> [a]               -- ^ The values.
            -> [b]               -- ^ The reduced values.
groupReduce mapReduce =
  (. const)
    . mapReduce
    . (&&& id)


-- | Reduce values by group, and flatten the result.
groupReduceFlatten :: Ord k
                   => MapReduce a k a [b]
                   -> (a -> k)            -- ^ Function for extracting keys for grouping.
                   -> ([a] -> [b])        -- ^ Function for reducing values.
                   -> [a]                 -- ^ The values.
                   -> [b]                 -- ^ The reduced values.
groupReduceFlatten mapReduce =
  ((concat .) .)
    . groupReduce mapReduce


-- | Reduce values by group, where the reducer receives the key.
groupReduceByKey :: Ord k
                 => MapReduce a k a b
                 -> (a -> k)          -- ^ Function for extracting keys for grouping.
                 -> (k -> [a] -> b)   -- ^ Function for reducing values.
                 -> [a]               -- ^ The values.
                 -> [b]               -- ^ The reduced values.
groupReduceByKey mapReduce =
  mapReduce
    . (&&& id)


-- | Reduce values by group, and flatten the result, where the reducer receives the key.
groupReduceFlattenByKey :: Ord k
                        => MapReduce a k a [b]
                        -> (a -> k)            -- ^ Function for extracting keys for grouping.
                        -> (k -> [a] -> [b])   -- ^ Function for reducing values.
                        -> [a]                 -- ^ The values.
                        -> [b]                 -- ^ The reduced values.
groupReduceFlattenByKey mapReduce =
  ((concat .) . )
    . groupReduceByKey mapReduce


-- | Order and extract values.
groupExtract :: Ord k
             => MapReduce a k a [b]
             -> (a -> k)            -- ^ Function for extracting keys for grouping.
             -> (a -> b)            -- ^ Function for extracting values.
             -> [a]                 -- ^ The values.
             -> [b]                 -- ^ The extract.
groupExtract mapReduce =
  (. map)
    . groupReduceFlatten mapReduce


-- | Reduce values by keying them, and flatten the result.
mapReduceFlatten :: Ord k
                 => MapReduce a k v [b]
                 -> (a -> (k, v))       -- ^ Function for mapping to keys and values.
                 -> (k -> [v] -> [b])   -- ^ Function for reducing values.
                 -> [a]                 -- ^ The values.
                 -> [b]                 -- ^ The reduced values.
mapReduceFlatten mapReduce =
  ((concat .) .)
    . mapReduce


-- | Reduce values by keying them, transforming the result.
mapReduceFinalize :: Ord k
                  => MapReduce a k v b
                  -> (a -> (k, v))     -- ^ Function for mapping to keys and values.
                  -> (k -> [v] -> v)   -- ^ Function for reducing values.
                  -> (k -> v -> b)     -- ^ Function for transforming the reduced values.
                  -> [a]               -- ^ The values.
                  -> [b]               -- ^ The reduced values.
mapReduceFinalize mapReduce mapper reducer finalizer =
  mapReduce mapper
    $ liftA2 (.) finalizer reducer


-- | Aggregate values.
aggregate :: MapReduce a () v b
          -> (a -> v)           -- ^ Function to extract values.
          -> ([v] -> b)         -- ^ Function to aggregate a list of values.
          -> [a]                -- ^ The values.
          -> b                  -- ^ The aggregate.
aggregate mapReduce =
  ((head .) .) . aggregateByKey mapReduce (const ())


-- | Aggregate values by key.
aggregateByKey :: Ord k
               => MapReduce a k v b
               -> (a -> k)          -- ^ Function for extracting keys.
               -> (a -> v)          -- ^ Function for extracting values.
               -> ([v] -> b)        -- ^ Function to aggregate a list of values.
               -> [a]               -- ^ The values.
               -> [b]               -- ^ The aggregates.
aggregateByKey mapReduce keyer extractor =
  aggregateWithKey mapReduce keyer extractor . const


-- | Aggregate values by key.
aggregateWithKey :: Ord k
                 => MapReduce a k v b
                 -> (a -> k)          -- ^ Function for extracting keys.
                 -> (a -> v)          -- ^ Function for extracting values.
                 -> (k -> [v] -> b)   -- ^ Function to aggregate a list of values.
                 -> [a]               -- ^ The values.
                 -> [b]               -- ^ The aggregates.
aggregateWithKey mapReduce keyer extractor =
  mapReduce (keyer &&& extractor)
