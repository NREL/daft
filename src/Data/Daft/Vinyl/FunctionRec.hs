{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Data.Daft.Vinyl.FunctionRec (
  FunctionRec(..)
, naturalJoin
, crossJoin
) where


import Data.Daft.Lookup
import Data.Daft.Vinyl.TypeLevel (RDistinct, RIntersection, RUnion(runion))
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Vinyl.Core (Rec)
import Data.Vinyl.Lens (type (⊆), rcast)

import qualified Data.Daft.Vinyl.Join as J (crossJoin, naturalJoin)
import qualified Data.Set as S (elems, fromList)


data FunctionRec k v =
    Tabulation
    {
      tabulation :: LookupTable k v
    }
  | SupportedFunction
    {
      support  :: Set k
    , function :: k -> v
    }


naturalJoin :: forall f kj k1 k2 k3 v1 v2 v3 proxy
            .  (Eq (Rec f kj), kj ⊆ k1, kj ⊆ k2, k1 ⊆ k3, k2 ⊆ k3, RUnion k1 k2 k3, RIntersection k1 k2 kj, RUnion v1 v2 v3, RDistinct v1 v2, Ord (Rec f k3))
            => proxy kj -> FunctionRec (Rec f k1) (Rec f v1) -> FunctionRec (Rec f k2) (Rec f v2) -> FunctionRec (Rec f k3) (Rec f v3)
-- FIXME: We could simplify the constaints if the type system could make inteferences about the relationships between unions, intersections, and subsets.
-- FIXME: This is an O(n^2) algorithm!
naturalJoin p (Tabulation t1) (Tabulation t2) =
  Tabulation
    . asLookupTable
    $ catMaybes
    [
      (, v1 `runion` v2) <$> J.naturalJoin p k1 k2
    |
      (k1, v1) <- assocs t1
    , (k2, v2) <- assocs t2
    ]
naturalJoin p (Tabulation t1) (SupportedFunction s2 f2) =
  Tabulation
    . asLookupTable 
    $ catMaybes
    [
        (, v1 `runion` f2 k2) <$> J.naturalJoin p k1 k2
    |
      (k1, v1) <- assocs t1
    , k2 <- S.elems s2
    ]
naturalJoin p (SupportedFunction s1 f1) (Tabulation t2) =
  Tabulation
    . asLookupTable 
    $ catMaybes
    [
        (, f1 k1 `runion` v2) <$> J.naturalJoin p k1 k2
    |
      k1 <- S.elems s1
    , (k2, v2) <- assocs t2
    ]
naturalJoin p (SupportedFunction s1 f1) (SupportedFunction s2 f2) =
  let
    support =
      S.fromList
      $ catMaybes
      [
        J.naturalJoin p k1 k2
      |
        k1 <- S.elems s1
      , k2 <- S.elems s2
      ]
    function k3 =
      let
        k1 = rcast k3 :: Rec f k1
        k2 = rcast k3 :: Rec f k2
      in
        f1 k1 `runion` f2 k2
  in
    SupportedFunction{..}


crossJoin :: forall f k1 k2 k3 v1 v2 v3
          .  (k1 ⊆ k3, k2 ⊆ k3, RUnion k1 k2 k3, RDistinct k1 k2, RUnion v1 v2 v3, RDistinct v1 v2, Ord (Rec f k3))
          => FunctionRec (Rec f k1) (Rec f v1) -> FunctionRec (Rec f k2) (Rec f v2) -> FunctionRec (Rec f k3) (Rec f v3)
crossJoin (Tabulation t1) (Tabulation t2) =
  Tabulation
    $  asLookupTable
    [
      (k1 `J.crossJoin` k2, v1 `runion` v2)
    |
      (k1, v1) <- assocs t1
    , (k2, v2) <- assocs t2
    ]
crossJoin (Tabulation t1) (SupportedFunction s2 f2) =
  Tabulation
    $ asLookupTable
    [
      (k1 `J.crossJoin` k2, v1 `runion` f2 k2)
    |
      (k1, v1) <- assocs t1
    , k2 <- S.elems s2
    ]
crossJoin (SupportedFunction s1 f1) (Tabulation t2) =
  Tabulation
    $ asLookupTable
    [
      (k1 `J.crossJoin` k2, f1 k1 `runion` v2)
    |
      k1 <- S.elems s1
    , (k2, v2) <- assocs t2
    ]
crossJoin (SupportedFunction s1 f1) (SupportedFunction s2 f2) =
  let
    support =
      S.fromList
      [
        J.crossJoin k1 k2
      |
        k1 <- S.elems s1
      , k2 <- S.elems s2
      ]
    function k3 =
      let
        k1 = rcast k3 :: Rec f k1
        k2 = rcast k3 :: Rec f k2
      in
        f1 k1 `runion` f2 k2
  in
    SupportedFunction{..}
