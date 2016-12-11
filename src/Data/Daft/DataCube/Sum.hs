module Data.Daft.DataCube.Sum (
-- * Types
  SumCube(..)
, asTableCube
, asFunctionCube
) where


import Control.Applicative ((<|>))
import Control.DeepSeq (NFData(..))
import Data.Daft.DataCube (DataCube(..))
import Data.Daft.DataCube.Function (FunctionCube(..), joinAny)
import Data.Daft.DataCube.Table (TableCube)
import Data.Map.Strict (empty, mergeWithKey, union)


-- FIXME: Is there a way to make this type extensible, maybe using polymorphic variants?

data SumCube k v =
    TableSumCube    (TableCube    k v)
  | FunctionSumCube (FunctionCube k v)

instance Functor (SumCube k) where
  fmap f (TableSumCube    cube) = TableSumCube    $ fmap f cube
  fmap f (FunctionSumCube cube) = FunctionSumCube $ fmap f cube

instance Ord k => Applicative (SumCube k) where

  pure = FunctionSumCube . FunctionCube . const . return

  TableSumCube table1 <*> TableSumCube table2 =
    TableSumCube
      $ mergeWithKey (const (Just .)) (const empty) (const empty) table1 table2

  cube1 <*> cube2 =
    FunctionSumCube . FunctionCube $ \k ->
      do
        f <- cube1 `evaluate` k
        v <- cube2 `evaluate` k
        return $ f v

instance Ord k => Monoid (SumCube k v) where

  mempty = TableSumCube mempty

  mappend (TableSumCube table1) (TableSumCube table2) = TableSumCube $ union table1 table2

  mappend cube1 cube2 = FunctionSumCube . FunctionCube $ \k -> evaluate cube1 k <|> evaluate cube2 k

instance (NFData k, NFData v) => NFData (SumCube k v) where

  rnf (TableSumCube    cube) = rnf cube

  rnf (FunctionSumCube cube) = rnf cube

instance DataCube SumCube where

  evaluate (TableSumCube    c) = evaluate c
  evaluate (FunctionSumCube c) = evaluate c

  selectWithKey f (TableSumCube    c) = TableSumCube    $ selectWithKey f c
  selectWithKey f (FunctionSumCube c) = FunctionSumCube $ selectWithKey f c

  selectRange k1 k2 (TableSumCube    c) = TableSumCube    $ selectRange k1 k2 c
  selectRange k1 k2 (FunctionSumCube c) = FunctionSumCube $ selectRange k1 k2 c

  knownKeys (TableSumCube    c) = knownKeys c
  knownKeys (FunctionSumCube c) = knownKeys c

  selectKnownMinimum (TableSumCube    c) = selectKnownMinimum c
  selectKnownMinimum (FunctionSumCube c) = selectKnownMinimum c

  selectKnownMaximum (TableSumCube    c) = selectKnownMaximum c
  selectKnownMaximum (FunctionSumCube c) = selectKnownMaximum c

  projectWithKey f (TableSumCube    c) = TableSumCube    $ projectWithKey f c
  projectWithKey f (FunctionSumCube c) = FunctionSumCube $ projectWithKey f c

  projectKnownKeys f (TableSumCube    c) = projectKnownKeys f c
  projectKnownKeys f (FunctionSumCube c) = projectKnownKeys f c

  rekey f (TableSumCube    c) = TableSumCube    $ rekey f c
  rekey f (FunctionSumCube c) = FunctionSumCube $ rekey f c

  aggregateWithKey f g (TableSumCube    c) = TableSumCube    $ aggregateWithKey f g c
  aggregateWithKey f g (FunctionSumCube c) = FunctionSumCube $ aggregateWithKey f g c

  disaggregateWithKey f g (TableSumCube    c) = TableSumCube    $ disaggregateWithKey f g c
  disaggregateWithKey f g (FunctionSumCube c) = FunctionSumCube $ disaggregateWithKey f g c

  joinSelf f g (TableSumCube    c1) (TableSumCube    c2) = TableSumCube    $ joinSelf f g c1 c2
  joinSelf f g (TableSumCube    c1) (FunctionSumCube c2) = FunctionSumCube $ joinAny  f g c1 c2
  joinSelf f g (FunctionSumCube c1) (TableSumCube    c2) = FunctionSumCube $ joinAny  f g c1 c2
  joinSelf f g (FunctionSumCube c1) (FunctionSumCube c2) = FunctionSumCube $ joinSelf f g c1 c2


asTableCube :: SumCube k v -> TableCube k v
asTableCube (TableSumCube    c) = c
asTableCube (FunctionSumCube _) = empty


asFunctionCube :: Ord k => SumCube k v -> FunctionCube k v
asFunctionCube (TableSumCube    c) = FunctionCube $ evaluate c
asFunctionCube (FunctionSumCube c) = c
