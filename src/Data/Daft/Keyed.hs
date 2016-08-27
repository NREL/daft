{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe            #-}


module Data.Daft.Keyed {-# DEPRECATED "This module will be removed in a future release." #-} (
  Keyed(..)
, makeKeyed
, asPair
, groupByKey
, aggregate
, aggregateByKey
) where


import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (groupBy, sortOn)


data Keyed k v =
  Keyed
  {
    key   :: k
  , value :: v
  }
    deriving (Eq, Ord, Read, Show)


makeKeyed :: (a -> k) -> (a -> v) -> a -> Keyed k v
makeKeyed fk fv = uncurry Keyed . (fk &&& fv)


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
