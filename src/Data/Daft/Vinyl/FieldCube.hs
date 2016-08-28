{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}


module Data.Daft.Vinyl.FieldCube (
-- * Types
  type (↝)
, FieldCube
, FieldGregator
-- * Conversion
, fromRecords
, toRecords
-- * Evaluation
, (!)
-- * Selection, projection, and aggregation
, σ
, π
, κ
-- * Joins
, (⋈)
, (⋉)
, (▷)
{- TODO: Implement these:
-- * Input/output
, readFieldCube
, readFieldCubeFile
, readFiledCubeSource
, showFieldCube
, writeFieldCubeFile
, writeFieldCubeSource
-}
) where


import Data.Daft.DataCube (DataCube)
import Data.Daft.TypeLevel (Intersection, Union)
import Data.Daft.Vinyl.TypeLevel (RDistinct, RJoin(rjoin), RUnion(runion))
import Data.Maybe (fromMaybe)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆), rcast)

import qualified Data.Daft.DataCube as C (Gregator, Joiner(Joiner), aggregateWithKey, antijoin, evaluate, fromTable, join, projectWithKey, selectWithKey, semijoin, toTable)


type ks ↝ vs = FieldCube ks vs


type FieldCube ks vs = DataCube (FieldRec ks) (FieldRec vs)


fromRecords :: (ks ⊆ as, vs ⊆ as, Ord (FieldRec ks)) => [FieldRec as] -> FieldCube ks vs
fromRecords = C.fromTable rcast rcast


toRecords :: (Ord (FieldRec ks), RUnion ks vs as, Union ks vs ~ as) => [FieldRec ks] -> FieldCube ks vs -> [FieldRec as]
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


(⋈) :: (Eq (FieldRec (Intersection kLeft kRight)), Intersection kLeft kRight ⊆ kLeft, Intersection kLeft kRight ⊆ kRight, kLeft ⊆ k, kRight ⊆ k, RUnion kLeft kRight k, RUnion vLeft vRight v, RDistinct vLeft vRight, Ord (FieldRec kLeft), Ord (FieldRec kRight), Ord (FieldRec k)) -- FIXME: This can be simplified somewhat.
    => FieldCube kLeft vLeft -> FieldCube kRight vRight -> FieldCube k v
(⋈) = C.join (C.Joiner rjoin rcast rcast) runion


(⋉) :: (Ord (FieldRec kRight), kRight ⊆ kLeft)
    => FieldCube kLeft vLeft -> FieldCube kRight vRight -> FieldCube kLeft vLeft
(⋉) = C.semijoin $ C.Joiner undefined undefined rcast


(▷) :: (Ord (FieldRec kRight), kRight ⊆ kLeft)
    => FieldCube kLeft vLeft -> FieldCube kRight vRight -> FieldCube kLeft vLeft
(▷) = C.antijoin $ C.Joiner undefined undefined rcast
