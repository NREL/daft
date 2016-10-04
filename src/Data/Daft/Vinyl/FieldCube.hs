{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}


module Data.Daft.Vinyl.FieldCube (
-- * Types
  type (+↝)
, type (-↝)
, FieldCube
, FieldGregator
-- * Conversion
, fromRecords
, toRecords
, toKnownRecords
, τ
-- * Evaluation
, (!)
-- * Selection, projection, and aggregation
, σ
, π
, κ
, δ
, ω
, ρ
-- * Joins
, (⋈)
, (⋉)
, (▷)
, (×)
) where


import Control.Applicative (liftA2)
import Data.Daft.DataCube (DataCube, FunctionCube, Joinable, TableCube)
import Data.Daft.TypeLevel (Intersection)
import Data.Daft.Vinyl.TypeLevel (RDistinct, RJoin(rjoin), RUnion(runion))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆), rcast)
import Data.Vinyl.TypeLevel (type (++))

import qualified Data.Daft.DataCube as C -- (Gregator(..), Joiner(Joiner), aggregateWithKey, antijoin, disaggregateWithKey, evaluate, fromTable, join, knownKeys, projectWithKey, reify, selectWithKey, semijoin, toKnownTable, toTable)
import qualified Data.Set as S (fromDistinctAscList, map, toAscList)


type ks +↝ vs = TableCube (FieldRec ks) (FieldRec vs)


type ks -↝ vs = FunctionCube (FieldRec ks) (FieldRec vs)


type FieldCube cube ks vs = cube (FieldRec ks) (FieldRec vs)


fromRecords :: (ks ⊆ as, vs ⊆ as, RUnion ks vs as, Ord (FieldRec ks)) => [FieldRec as] -> ks +↝ vs
fromRecords = C.fromTable rcast rcast


toRecords :: (Ord (FieldRec ks), RUnion ks vs as, DataCube cube) => [FieldRec ks] -> FieldCube cube ks vs -> [FieldRec as]
toRecords = C.toTable runion


toKnownRecords :: (Ord (FieldRec ks), RUnion ks vs as) => ks +↝ vs -> [FieldRec as]
toKnownRecords = C.toKnownTable runion


τ :: (b ⊆ a) => FieldRec a -> FieldRec b
τ = rcast


(!) :: (Ord (FieldRec ks), DataCube cube) => FieldCube cube ks vs -> FieldRec ks -> FieldRec vs
(!) = (fromMaybe (error "FieldCube: key not found.") .) . C.evaluate


σ :: DataCube cube => (FieldRec ks -> FieldRec vs -> Bool) -> FieldCube cube ks vs -> FieldCube cube ks vs
σ = C.selectWithKey


π :: DataCube cube => (FieldRec ks -> FieldRec vs -> FieldRec ws) -> FieldCube cube ks vs -> FieldCube cube ks ws
π = C.projectWithKey


ρ :: DataCube cube => Ord (FieldRec ks) => Set (FieldRec ks) -> FieldCube cube ks vs -> ks +↝ vs
ρ = C.reify


type FieldGregator as bs = C.Gregator (FieldRec as) (FieldRec bs)


κ :: (ks0 ⊆ ks, ks ⊆ (ks' ++ ks0), ks' ⊆ ks, Ord (FieldRec ks), Ord (FieldRec ks'), DataCube cube) => Set (FieldRec ks0) -> (FieldRec ks' -> [FieldRec vs] -> FieldRec vs') -> FieldCube cube ks vs -> FieldCube cube ks' vs' -- FIXME: Instead of subset, use sum.
κ keys =
  C.aggregateWithKey
    C.Gregator
    {
      C.aggregator    = rcast
    , C.disaggregator = flip map (S.toAscList keys) . (rcast .) . (<+>)
    }


δ :: (ks0 ⊆ ks', ks' ⊆ (ks ++ ks0), ks ⊆ ks', Ord (FieldRec ks), Ord (FieldRec ks'), DataCube cube) => Set (FieldRec ks0) -> (FieldRec ks' -> FieldRec vs -> FieldRec vs') -> FieldCube cube ks vs -> FieldCube cube ks' vs' -- FIXME: Instead of subset, use sum.
δ keys =
  C.disaggregateWithKey
    C.Gregator
    {
      C.aggregator    = rcast
    , C.disaggregator = flip map (S.toAscList keys) . (rcast .) . (<+>)
    }


ω :: (ks' ⊆ ks, Ord (FieldRec ks'), DataCube cube) => FieldCube cube ks vs -> Set (FieldRec ks')
ω = S.map rcast . C.knownKeys


(⋈) :: (Eq (FieldRec (Intersection kLeft kRight)), Intersection kLeft kRight ⊆ kLeft, Intersection kLeft kRight ⊆ kRight, kLeft ⊆ k, kRight ⊆ k, RUnion kLeft kRight k, RUnion vLeft vRight v, RDistinct vLeft vRight, Ord (FieldRec kLeft), Ord (FieldRec kRight), Ord (FieldRec k), DataCube cubeLeft, DataCube cubeRight, DataCube cube, Joinable cubeLeft cubeRight cube) -- FIXME: This can be simplified somewhat.
    => FieldCube cubeLeft kLeft vLeft -> FieldCube cubeRight kRight vRight -> FieldCube cube k v
(⋈) = C.join (C.Joiner rjoin rcast rcast) runion
infixl 6 ⋈


(⋉) :: (Ord (FieldRec kRight), kRight ⊆ kLeft, DataCube cube)
    => FieldCube cube kLeft vLeft -> FieldCube cube kRight vRight -> FieldCube cube kLeft vLeft
(⋉) = C.semijoin $ C.Joiner undefined undefined rcast
infixl 6 ⋉


(▷) :: (Ord (FieldRec kRight), kRight ⊆ kLeft, DataCube cube)
    => FieldCube cube kLeft vLeft -> FieldCube cube kRight vRight -> FieldCube cube kLeft vLeft
(▷) = C.antijoin $ C.Joiner undefined undefined rcast
infixl 6 ▷


(×) :: Set (FieldRec ks) -> Set (FieldRec ks') -> Set (FieldRec (ks ++ ks'))
(×) s1 s2 = S.fromDistinctAscList $ liftA2 (<+>) (S.toAscList s1) (S.toAscList s2)
infixl 8 ×
