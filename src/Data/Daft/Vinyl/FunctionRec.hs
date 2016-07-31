{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}


module Data.Daft.Vinyl.FunctionRec (
  FunctionRec(..)
, makeTabulatedFunction
, unmakeTabulatedFunction
, evaluate
, evaluate'
, tabulate
, naturalJoin
, crossJoin
) where


import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import Data.Daft.Keyed (makeKeyed)
import Data.Daft.Lookup (LookupTable, asLookupTable, assocs)
import Data.Daft.Vinyl.TypeLevel (RDistinct, RIntersection, RUnion(runion))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆), rcast)

import qualified Data.Daft.Vinyl.Join as J (crossJoin, naturalJoin)
import qualified Data.Map as M (empty, lookup, mergeWithKey, union)


data FunctionRec k v =
    TabulatedFunction
    {
      tabulation :: LookupTable k v
    }
  | SupportedFunction
    {
      function :: k -> Maybe v
    }

instance Functor (FunctionRec k) where
  fmap f (TabulatedFunction t1) = TabulatedFunction $ fmap f t1
  fmap f (SupportedFunction f1) = SupportedFunction $ fmap f . f1

instance Ord k => Applicative (FunctionRec k) where
  pure = SupportedFunction . const . return
  TabulatedFunction t1 <*> TabulatedFunction t2 =
    TabulatedFunction
      $ M.mergeWithKey (const (Just .)) (const M.empty) (const M.empty) t1 t2
  fr1 <*> fr2 =
    SupportedFunction $ \k ->
      do
       f <- fr1 `evaluate` k
       v <- fr2 `evaluate` k
       return $ f v

instance Ord k => Monoid (FunctionRec k v) where
  mempty = TabulatedFunction mempty
  mappend (TabulatedFunction t1) (TabulatedFunction t2) = TabulatedFunction $ M.union t1 t2
  mappend tf1 tf2 = SupportedFunction $ \k -> evaluate tf1 k <|> evaluate tf2 k


makeTabulatedFunction :: (Ord (FieldRec ks), ks ⊆ rs, vs ⊆ rs) => [FieldRec rs] -> FunctionRec (FieldRec ks) (FieldRec vs)
makeTabulatedFunction = TabulatedFunction . asLookupTable . fmap (makeKeyed rcast rcast)


unmakeTabulatedFunction :: (Ord (FieldRec ks), RUnion ks vs rs) => [FieldRec ks] -> FunctionRec (FieldRec ks) (FieldRec vs) -> [FieldRec rs]
unmakeTabulatedFunction = ((fmap (uncurry runion) . assocs) .) . tabulate


evaluate :: Ord k => FunctionRec k v -> k -> Maybe v
evaluate TabulatedFunction{..} = flip M.lookup tabulation
evaluate SupportedFunction{..} = function


evaluate' :: Ord k => FunctionRec k v -> k -> v
evaluate' = (fromMaybe (error "Data.Daft.Vinyl.FunctionRec: function not defined for key") .) . evaluate


tabulate :: Ord k => [k] -> FunctionRec k v -> LookupTable k v
tabulate ks f = asLookupTable $ mapMaybe (liftM2 fmap (,) (evaluate f)) ks


-- FIXME: Try to unify natural and cross joins into a single function.


naturalJoin :: forall kj k1 k2 k3 v1 v2 v3 proxy
            .  (Eq (FieldRec kj), kj ⊆ k1, kj ⊆ k2, k1 ⊆ k3, k2 ⊆ k3, RUnion k1 k2 k3, RIntersection k1 k2 kj, RUnion v1 v2 v3, RDistinct v1 v2, Ord (FieldRec k1), Ord (FieldRec k2), Ord (FieldRec k3))
            => proxy kj -> FunctionRec (FieldRec k1) (FieldRec v1) -> FunctionRec (FieldRec k2) (FieldRec v2) -> FunctionRec (FieldRec k3) (FieldRec v3)
-- FIXME: We could simplify the constaints if the type system could make inteferences about the relationships between unions, intersections, and subsets.
naturalJoin p (TabulatedFunction t1) (TabulatedFunction t2) =
  -- FIXME: This is an O(n^2) algorithm!
  TabulatedFunction
    . asLookupTable
    $ catMaybes
    [
      (, v1 `runion` v2) <$> J.naturalJoin p k1 k2
    |
      (k1, v1) <- assocs t1
    , (k2, v2) <- assocs t2
    ]
naturalJoin _ fr1 fr2 =
  SupportedFunction $ \k3 ->
    do
      v1 <- fr1 `evaluate` rcast k3
      v2 <- fr2 `evaluate` rcast k3
      return $ v1 `runion` v2


crossJoin :: forall k1 k2 k3 v1 v2 v3
          .  (k1 ⊆ k3, k2 ⊆ k3, RUnion k1 k2 k3, RDistinct k1 k2, RUnion v1 v2 v3, RDistinct v1 v2, Ord (FieldRec k1), Ord (FieldRec k2), Ord (FieldRec k3))
          => FunctionRec (FieldRec k1) (FieldRec v1) -> FunctionRec (FieldRec k2) (FieldRec v2) -> FunctionRec (FieldRec k3) (FieldRec v3)
-- FIXME: We could simplify the constaints if the type system could make inteferences about the relationships between unions, intersections, and subsets.
crossJoin (TabulatedFunction t1) (TabulatedFunction t2) =
  TabulatedFunction
    $  asLookupTable
    [
      (k1 `J.crossJoin` k2, v1 `runion` v2)
    |
      (k1, v1) <- assocs t1
    , (k2, v2) <- assocs t2
    ]
crossJoin fr1 fr2 =
  SupportedFunction $ \k3 ->
    do
      v1 <- fr1 `evaluate` rcast k3
      v2 <- fr2 `evaluate` rcast k3
      return $ v1 `runion` v2
