{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}


module Data.Daft.DataCube (
-- * Types
  DataCube(..)
, TableCube
, ExistentialCube(..)
, Join
, Joinable(..)
, FunctionCube
, Rekeyer(..)
, Gregator(..)
, Joiner(..)
-- * Conversion
, fromTable
, fromTableM
, fromFunction
, toKnownTable
, reify
-- * Aggregation
, fromKnownKeys
-- * Joins
, semijoin
, antijoin
) where


import Control.Applicative ((<|>), liftA2)
import Control.Arrow ((&&&), (***))
import Control.DeepSeq (NFData(..))
import Control.Monad (guard)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import GHC.Exts (IsList(Item))

import qualified Data.Map.Strict as M ((!), assocs, filterWithKey, findMin, findMax, fromList, fromListWith, keys, keysSet, lookup, mapKeysWith, mapWithKey, member, null, split, size)
import qualified Data.Set as S (empty, null, size)
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

  joinSelf :: (Ord k1, Ord k2, Ord k3) => Joiner k1 k2 k3 -> (v1 -> v2 -> v3) -> cube k1 v1 -> cube k2 v2 -> cube k3 v3


joinAny :: (Ord k1, Ord k2, Ord k3, DataCube cube, DataCube cube') => Joiner k1 k2 k3 -> (v1 -> v2 -> v3) -> cube k1 v1 -> cube' k2 v2 -> FunctionCube k3 v3
joinAny Joiner{..} combiner cube1 cube2 =
  FunctionCube $ \k3 ->
    do
      v1 <- cube1 `evaluate` castLeft k3
      v2 <- cube2 `evaluate` castRight k3
      return $ v1 `combiner` v2


type family Join c1 c2 :: * -> * -> * where
  Join a a = a
  Join a b = FunctionCube


data JoinSelf
data JoinAny

type family JoinStyle (c1 :: * -> * -> *) (c2 :: * -> * -> *) where
  JoinStyle a a = JoinSelf
  JoinStyle a b = JoinAny


class Joinable c1 c2 where
  join :: (Ord k1, Ord k2, Ord k3) => Joiner k1 k2 k3 -> (v1 -> v2 -> v3) -> c1 k1 v1 -> c2 k2 v2 -> (Join c1 c2) k3 v3

instance (JoinStyle c1 c2 ~ flag, Joinable' flag c1 c2 (Join c1 c2)) => Joinable c1 c2 where
  join = join' (Proxy :: Proxy flag)


class Joinable' flag c1 c2 c3 where
  join' :: (Ord k1, Ord k2, Ord k3) => Proxy flag -> Joiner k1 k2 k3 -> (v1 -> v2 -> v3) -> c1 k1 v1 -> c2 k2 v2 -> c3 k3 v3

instance DataCube c1 => Joinable' JoinSelf c1 c1 c1 where
  join' _ = joinSelf

instance (DataCube c1, DataCube c2) => Joinable' JoinAny c1 c2 FunctionCube where
  join' _ = joinAny


data ExistentialCube ks vs = forall cube . DataCube cube => ExistentialCube (cube ks vs)

instance DataCube ExistentialCube where
  evaluate (ExistentialCube c) = evaluate c
  selectWithKey f (ExistentialCube c) = ExistentialCube $ selectWithKey f c
  selectRange k1 k2 (ExistentialCube c) = ExistentialCube $ selectRange k1 k2 c
  knownKeys (ExistentialCube c) = knownKeys c
  selectKnownMinimum (ExistentialCube c) = selectKnownMinimum c
  selectKnownMaximum (ExistentialCube c) = selectKnownMaximum c
  projectWithKey f (ExistentialCube c) = ExistentialCube $ projectWithKey f c
  projectKnownKeys f (ExistentialCube c) = projectKnownKeys f c
  rekey f (ExistentialCube c) = ExistentialCube $ rekey f c
  aggregateWithKey f g (ExistentialCube c) = ExistentialCube $ aggregateWithKey f g c
  disaggregateWithKey f g (ExistentialCube c) = ExistentialCube $ disaggregateWithKey f g c
  joinSelf f g (ExistentialCube c1) (ExistentialCube c2) = ExistentialCube $ joinAny f g c1 c2 -- FIXME: Danger--optimizations are lost here because it uses `joinAny` instead of `join`.  Maybe we could use a view pattern here?


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


newtype FunctionCube k v = FunctionCube {function :: k -> Maybe v}


instance DataCube FunctionCube where

  evaluate = function

  knownKeys = const S.empty

  knownSize = const 0

  knownEmpty = const False

  selectWithKey selector FunctionCube{..} =
    FunctionCube $ \k ->
      do
        v <- function k
        guard
          $ selector k v
        return v

  selectRange Nothing Nothing f = f
  selectRange (Just k0) Nothing FunctionCube{..} =
    FunctionCube $ \k ->
      do
        guard
          $ k0 < k
        function k
  selectRange Nothing (Just k1) FunctionCube{..} =
    FunctionCube $ \k ->
      do
        guard
          $ k < k1
        function k
  selectRange (Just k0) (Just k1) FunctionCube{..} =
    FunctionCube $ \k ->
      do
        guard
          $ k0 < k && k < k1
        function k

  selectKnownMinimum FunctionCube{..} = Nothing

  selectKnownMaximum FunctionCube{..} = Nothing

  projectWithKey projector FunctionCube{..} = FunctionCube $ liftA2 fmap projector function

  projectKnownKeys _         FunctionCube{..} = L.fromList []

  rekey Rekeyer{..} FunctionCube{..} = FunctionCube $ function . unrekeyer

  aggregateWithKey Gregator{..} reducer FunctionCube{..} =
    FunctionCube $ \k' ->
      return
        . reducer k'
        . mapMaybe function
        $ disaggregator k'

  disaggregateWithKey Gregator{..} expander FunctionCube{..} = 
    FunctionCube $ \k' ->
      fmap (expander k')
        . function
        $ aggregator k'

  joinSelf = joinAny


instance Functor (FunctionCube k) where
  fmap f (FunctionCube g) = FunctionCube $ fmap f . g


instance Ord k => Applicative (FunctionCube k) where
  pure = FunctionCube . const . return
  FunctionCube g <*> FunctionCube h =
    FunctionCube $ \k ->
      do
        f <- g k
        v <- h k
        return $ f v


instance Ord k => Monoid (FunctionCube k v) where
  mempty = FunctionCube $ const Nothing
  mappend (FunctionCube f) (FunctionCube g) = FunctionCube $ \k -> f k <|> g k


instance (NFData k, NFData v) => NFData (FunctionCube k v) where
  rnf (FunctionCube f) = rnf f
 

fromTable :: (IsList as, a ~ Item as, Ord k) => (a -> k) -> (a -> v) -> as -> TableCube k v
fromTable keyer valuer = M.fromList . fmap (keyer &&& valuer) . L.toList


fromTableM :: (Monad m, IsList as, a ~ Item as, Ord k) => (a -> m k) -> (a -> m v) -> as -> m (TableCube k v)
fromTableM keyer valuer =
  fmap M.fromList
    . mapM (liftA2 (,) . keyer <*> valuer)
    . L.toList


fromFunction :: (k -> Maybe v) -> FunctionCube k v
fromFunction = FunctionCube


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


fromKnownKeys :: (IsList ks, k ~ Item ks, Ord k, Ord k') => (k -> k') -> ks -> Gregator k k'
fromKnownKeys aggregator ks =
  let
    disaggregator =
      (M.!)
        $ M.fromListWith (++)
        $ (aggregator &&& return)
        <$> L.toList ks
  in
    Gregator{..}


data Joiner kLeft kRight k =
  Joiner
  {
    joiner    :: kLeft -> kRight -> Maybe k
  , castLeft  :: k -> kLeft
  , castRight :: k -> kRight
  }


semijoin :: (Ord k2, DataCube cube1, DataCube cube2) => Joiner k1 k2 k1 -> cube1 k1 v1 -> cube2 k2 v2 -> cube1 k1 v1
semijoin Joiner{..} = flip (selectKeys . (. castRight) . evaluable)


antijoin :: (Ord k2, DataCube cube1, DataCube cube2) => Joiner k1 k2 k1 -> cube1 k1 v1 -> cube2 k2 v2 -> cube1 k1 v1
antijoin Joiner{..}= flip (selectKeys . (. castRight) . (not .) . evaluable)
