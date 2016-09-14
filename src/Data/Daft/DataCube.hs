{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TypeFamilies    #-}


module Data.Daft.DataCube (
-- * Types
  DataCube -- FIXME: Would it be better not to keep this as abstract?
, Rekeyer(..)
, Gregator(..)
, Joiner(..)
-- * Conversion
, fromTable
, fromFunction
, toTable
, toKnownTable
, knownKeys
, withKnown
, knownSize
, knownEmpty
, reify
-- * Evaluation
, evaluate
, evaluable
-- * Selection
, select
, selectWithKey
, selectKeys
, selectRange
, selectKnownMinimum
, selectKnownMaximum
-- * Projection
, project
, projectWithKey
, projectKnownKeys
, rekey
-- * Aggregation
, fromKnownKeys
, aggregate
, aggregateWithKey
, disaggregate
, disaggregateWithKey
-- * Joins
, join
, semijoin
, antijoin
) where


import Control.Applicative ((<|>), liftA2)
import Control.Arrow ((&&&), (***))
import Control.DeepSeq (NFData(..))
import Control.Monad (guard)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Set (Set)
import GHC.Exts (IsList(Item))

import qualified Data.Map.Strict as M ((!), assocs, empty, filterWithKey, findMin, findMax, fromList, fromListWith, keys, keysSet, lookup, mapKeysWith, mapWithKey, member, mergeWithKey, null, split, size, union)
import qualified Data.Set as S (empty)
import qualified GHC.Exts as L (IsList(..))


data DataCube k v =
    TableCube
    {
      table :: Map k v
    }
  | FunctionCube
    {
      function :: k -> Maybe v
    }

instance Functor (DataCube k) where
  fmap f TableCube{..}    = TableCube    $ fmap f table
  fmap f FunctionCube{..} = FunctionCube $ fmap f . function

instance Ord k => Applicative (DataCube k) where
  pure = FunctionCube . const . return
  TableCube table1 <*> TableCube table2 =
    TableCube
      $ M.mergeWithKey (const (Just .)) (const M.empty) (const M.empty) table1 table2
  cube1 <*> cube2 =
    FunctionCube $ \k ->
      do
        f <- cube1 `evaluate` k
        v <- cube2 `evaluate` k
        return $ f v

instance Ord k => Monoid (DataCube k v) where
  mempty = TableCube mempty
  mappend (TableCube table1) (TableCube table2) = TableCube $ M.union table1 table2
  mappend cube1 cube2 = FunctionCube $ \k -> evaluate cube1 k <|> evaluate cube2 k

instance (NFData k, NFData v) => NFData (DataCube k v) where
  rnf TableCube{..}    = rnf table
  rnf FunctionCube{..} = rnf function
 

fromTable :: (IsList as, a ~ Item as, Ord k) => (a -> k) -> (a -> v) -> as -> DataCube k v
fromTable keyer valuer = TableCube . M.fromList . fmap (keyer &&& valuer) . L.toList


fromFunction :: (k -> Maybe v) -> DataCube k v
fromFunction = FunctionCube


toTable :: (IsList ks, k ~ Item ks, IsList as, a ~ Item as, Ord k) => (k -> v -> a) -> ks -> DataCube k v -> as
toTable combiner ks cube = L.fromList . mapMaybe (evaluate $ projectWithKey combiner cube) $ L.toList ks


toKnownTable :: (IsList as, a ~ Item as, Ord k) => (k -> v -> a) -> DataCube k v -> as
toKnownTable combiner TableCube{..}  = L.fromList . map (uncurry combiner) $ M.assocs table
toKnownTable _        FunctionCube{} = L.fromList []


knownKeys :: DataCube k v -> Set k
knownKeys TableCube{..}  = M.keysSet table
knownKeys FunctionCube{} = S.empty


withKnown :: DataCube k v -> (Set k -> DataCube k v -> a) -> a
withKnown cube f = f (knownKeys cube) cube


knownSize :: DataCube k v -> Int
knownSize TableCube{..}  = M.size table
knownSize FunctionCube{} = 0


knownEmpty :: DataCube k v -> Bool
knownEmpty TableCube{..}  = M.null table
knownEmpty FunctionCube{} = False


reify :: (Ord k, IsList ks, k ~ Item ks) => ks -> DataCube k v -> DataCube k v
reify ks cube =
  TableCube
    . M.fromList
    $ catMaybes
    [
      (k, ) <$> cube `evaluate` k
    |
      k <- L.toList ks
    ]


evaluate :: Ord k => DataCube k v -> k -> Maybe v
evaluate TableCube{..}    = flip M.lookup table
evaluate FunctionCube{..} = function


evaluable :: Ord k => DataCube k v -> k -> Bool
evaluable TableCube{..}    = flip M.member table
evaluable FunctionCube{..} = isJust . function


select :: (v -> Bool) -> DataCube k v -> DataCube k v
select = selectWithKey . const


selectWithKey :: (k -> v -> Bool) -> DataCube k v -> DataCube k v
selectWithKey selector TableCube{..} =
  TableCube
    $ M.filterWithKey selector table
selectWithKey selector FunctionCube{..} =
  FunctionCube $ \k ->
    do
      v <- function k
      guard
        $ selector k v
      return v


selectKeys :: (k -> Bool) -> DataCube k v -> DataCube k v
selectKeys = selectWithKey . (const .)


selectRange :: Ord k => Maybe k -> Maybe k -> DataCube k v -> DataCube k v
selectRange Nothing Nothing x = x
selectRange (Just k0) Nothing TableCube{..} =
  TableCube
    . snd
    $ M.split k0 table
selectRange Nothing (Just k1) TableCube{..} =
  TableCube
    . fst
    $ M.split k1 table
selectRange (Just k0) (Just k1) TableCube{..} =
  TableCube
    . fst
    . M.split k1
    . snd
    $ M.split k0 table
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


selectKnownMinimum :: Ord k => DataCube k v -> Maybe (k, v)
selectKnownMinimum TableCube{..} =
  do
    guard . not
      $ M.null table
    return
      $ M.findMin table
selectKnownMinimum FunctionCube{..} = Nothing


selectKnownMaximum :: Ord k => DataCube k v -> Maybe (k, v)
selectKnownMaximum TableCube{..} =
  do
    guard . not
      $ M.null table
    return
      $ M.findMax table
selectKnownMaximum FunctionCube{..} = Nothing


project :: (v -> v') -> DataCube k v -> DataCube k v'
project = projectWithKey . const


projectWithKey :: (k -> v -> v') -> DataCube k v -> DataCube k v'
projectWithKey projector TableCube{..} = TableCube $ M.mapWithKey projector table
projectWithKey projector FunctionCube{..} = FunctionCube $ liftA2 fmap projector function


projectKnownKeys :: (IsList ks', k' ~ Item ks') => (k -> k') -> DataCube k v -> ks'
projectKnownKeys projector TableCube{..}    = L.fromList . fmap projector $ M.keys table
projectKnownKeys _         FunctionCube{..} = L.fromList []


data Rekeyer a b =
  Rekeyer
  {
    rekeyer   :: a -> b
  , unrekeyer :: b -> a
  }


rekey :: Ord k' => Rekeyer k k' -> DataCube k v -> DataCube k' v
rekey Rekeyer{..} TableCube{..}    = TableCube $ M.mapKeysWith (const id) rekeyer table
rekey Rekeyer{..} FunctionCube{..} = FunctionCube $ function . unrekeyer


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


aggregate :: Ord k' => Gregator k k' -> ([v] -> v') -> DataCube k v -> DataCube k' v'
aggregate = (. const) . aggregateWithKey


aggregateWithKey :: Ord k' => Gregator k k' -> (k' -> [v] -> v') -> DataCube k v -> DataCube k' v'
aggregateWithKey Gregator{..} reducer TableCube{..} =
  TableCube
    . M.mapWithKey reducer
    . M.fromListWith (++)
    . fmap (aggregator *** return)
    $ M.assocs table
aggregateWithKey Gregator{..} reducer FunctionCube{..} =
  FunctionCube $ \k' ->
    return
      . reducer k'
      . mapMaybe function
      $ disaggregator k'


disaggregate :: Ord k' => Gregator k' k -> (v -> v') -> DataCube k v -> DataCube k' v'
disaggregate = (. const) . disaggregateWithKey


disaggregateWithKey :: Ord k' => Gregator k' k -> (k' -> v -> v') -> DataCube k v -> DataCube k' v'
disaggregateWithKey Gregator{..} expander TableCube{..} =
  TableCube
    . M.fromList
    . concatMap (\(k, v) -> map (\k' -> (k', expander k' v)) $ disaggregator k)
    $ M.assocs table
disaggregateWithKey Gregator{..} expander FunctionCube{..} = 
  FunctionCube $ \k' ->
    fmap (expander k')
      . function
      $ aggregator k'


data Joiner kLeft kRight k =
  Joiner
  {
    joiner    :: kLeft -> kRight -> Maybe k
  , castLeft  :: k -> kLeft
  , castRight :: k -> kRight
  }


join :: (Ord k1, Ord k2, Ord k3) => Joiner k1 k2 k3 -> (v1 -> v2 -> v3) -> DataCube k1 v1 -> DataCube k2 v2 -> DataCube k3 v3
join Joiner{..} combiner (TableCube table1) (TableCube table2) =
  TableCube
    . M.fromList
    $ catMaybes
    [
      (, v1 `combiner` v2) <$> k1 `joiner` k2
    |
      (k1, v1) <- M.assocs table1
    , (k2, v2) <- M.assocs table2
    ]
join Joiner{..} combiner cube1 cube2 =
  FunctionCube $ \k3 ->
    do
      v1 <- cube1 `evaluate` castLeft k3
      v2 <- cube2 `evaluate` castRight k3
      return $ v1 `combiner` v2


semijoin :: Ord k2 => Joiner k1 k2 k1 -> DataCube k1 v1 -> DataCube k2 v2 -> DataCube k1 v1
semijoin Joiner{..} = flip (selectKeys . (. castRight) . evaluable)


antijoin :: Ord k2 => Joiner k1 k2 k1 -> DataCube k1 v1 -> DataCube k2 v2 -> DataCube k1 v1
antijoin Joiner{..}= flip (selectKeys . (. castRight) . (not .) . evaluable)
