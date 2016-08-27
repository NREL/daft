{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Data.Daft.Vinyl.FieldCube (
  FieldCube
, fromRecords
, toRecords
, (!)
, σ
, π
, κ
, (⋈)
, (⋉)
, (▷)
, FieldGregator
, module Data.Daft.DataCube
) where


import Data.Daft.DataCube (DataCube)
import Data.Daft.TypeLevel (Intersection)
import Data.Daft.Vinyl.TypeLevel (RDistinct, RJoin(rjoin), RUnion(runion))
import Data.Maybe (fromMaybe)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆), rcast)

import qualified Data.Daft.DataCube as C


type FieldCube ks vs = DataCube (FieldRec ks) (FieldRec vs)


fromRecords :: (ks ⊆ as, vs ⊆ as, Ord (FieldRec ks)) => [FieldRec as] -> FieldCube ks vs
fromRecords = C.fromTable rcast rcast


toRecords :: (Ord (FieldRec ks), RUnion ks vs as) => [FieldRec ks] -> FieldCube ks vs -> [FieldRec as]
toRecords = C.toTable runion


(!) :: Ord (FieldRec ks) => FieldCube ks vs -> FieldRec ks -> FieldRec vs
(!) = (fromMaybe (error "FieldCube: key not found.") .) . C.evaluate


σ :: (FieldRec ks -> FieldRec vs -> Bool) -> FieldCube ks vs -> FieldCube ks vs
σ = C.selectWithKey


π :: (FieldRec ks -> FieldRec vs -> FieldRec ws) -> FieldCube ks vs -> FieldCube ks ws
π = C.projectWithKey


type FieldGregator as bs = C.Gregator (FieldRec as) (FieldRec bs)


κ :: Ord (FieldRec ks') => FieldGregator ks ks' -> (FieldRec ks' -> [FieldRec vs] -> FieldRec vs') -> FieldCube ks vs -> FieldCube ks' vs'
κ = C.aggregateWithKey


(⋈) :: (Eq (FieldRec (Intersection k1 k2)), Intersection k1 k2 ⊆ k1, Intersection k1 k2 ⊆ k2, k1 ⊆ k3, k2 ⊆ k3, RUnion k1 k2 k3, RUnion v1 v2 v3, RDistinct v1 v2, Ord (FieldRec k1), Ord (FieldRec k2), Ord (FieldRec k3))
    => FieldCube k1 v1 -> FieldCube k2 v2 -> FieldCube k3 v3
(⋈) = C.join (C.Joiner rjoin rcast rcast) runion


(⋉) :: (Ord (FieldRec k2), k2 ⊆ k1)
    => FieldCube k1 v1 -> FieldCube k2 v2 -> FieldCube k1 v1
(⋉) = C.semijoin $ C.Joiner undefined undefined rcast


(▷) :: (Ord (FieldRec k2), k2 ⊆ k1)
    => FieldCube k1 v1 -> FieldCube k2 v2 -> FieldCube k1 v1
(▷) = C.antijoin $ C.Joiner undefined undefined rcast
