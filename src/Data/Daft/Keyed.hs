{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe            #-}


module Data.Daft.Keyed (
  Keyed(..)
, asPair
, groupByKey
, aggregate
, aggregateByKey
) where


import Data.Function (on)
import Data.List (groupBy, sortOn)


data Keyed k v =
  Keyed
  {
    key   :: k
  , value :: v
  }
    deriving (Eq, Ord, Read, Show)


asPair :: Keyed k v -> (k, v)
asPair Keyed{..} = (key, value)


groupByKey :: (Eq k, Ord k) => [Keyed k v] -> [Keyed k [v]]
groupByKey =
  map (Keyed <$> key . head <*> map value)
    . groupBy ((==) `on` key)
    . sortOn key
      

aggregate :: ([v] -> v') -> Keyed k [v] -> Keyed k v'
aggregate f (Keyed k vs) = Keyed k $ f vs


aggregateByKey :: (Eq k, Ord k) => ([v] -> v') -> [Keyed k v] -> [Keyed k v']
aggregateByKey f = map (aggregate f) . groupByKey
