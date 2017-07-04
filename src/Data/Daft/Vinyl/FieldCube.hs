{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}


{-# OPTIONS_GHC -fno-warn-deprecations #-}


module Data.Daft.Vinyl.FieldCube (
-- * Types
  type (↝)
, type (+↝)
, type (*↝)
, type (-↝)
, FieldCube
, FieldGregator
-- * Conversion
, fromRecords
, toRecords
, toKnownRecords
, τ
, ε
, θ
, φ
-- * Evaluation
, (!)
-- * Selection, projection, and aggregation
, σ
, π
, κ
, δ
, ω
, υ
, ρ
-- * Joins
, (⋈)
, (⋉)
, (▷)
, (×)
) where


import Control.Applicative (liftA2)
import Data.Daft.DataCube (DataCube(..))
import Data.Daft.DataCube.Existential (ExistentialCube(..))
import Data.Daft.DataCube.Function (FunctionCube)
import Data.Daft.DataCube.Join (Join, Joinable)
import Data.Daft.DataCube.Sum (SumCube(..))
import Data.Daft.DataCube.Table (TableCube)
import Data.Daft.TypeLevel (Intersection)
import Data.Daft.Vinyl.TypeLevel (RDistinct, RJoin(rjoin), RUnion(runion))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Set (Set)
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆), rcast)
import Data.Vinyl.TypeLevel (type (++))

import qualified Data.Daft.DataCube as C (DataCube(..), Gregator(..), Joiner(..))
import qualified Data.Daft.DataCube.Join as C (antijoin, join, semijoin)
import qualified Data.Daft.DataCube.Table as C (reify, fromTable)
import qualified Data.Set as S (fromDistinctAscList, toAscList)


ε :: (Typeable cube, DataCube cube, Key cube ~ Ord, Keys cube ~ Set) => FieldCube cube ks vs -> ks ↝ vs -- FIXME: Could the 'Ord' constraint be dropped?
ε = ExistentialCube


{-# DEPRECATED θ, φ, (+↝) "Use (↝) and ε instead." #-}


θ :: ks *↝ vs -> ks +↝ vs
θ = TableSumCube


φ :: ks -↝ vs -> ks +↝ vs
φ = FunctionSumCube


type ks ↝ vs = FieldCube ExistentialCube ks vs


type ks +↝ vs = FieldCube SumCube ks vs


type ks *↝ vs = FieldCube TableCube ks vs


type ks -↝ vs = FieldCube FunctionCube ks vs


type FieldCube cube ks vs = cube (FieldRec ks) (FieldRec vs)


fromRecords :: (ks ⊆ as, vs ⊆ as, RUnion ks vs as, Ord (FieldRec ks)) => [FieldRec as] -> ks *↝ vs
fromRecords = C.fromTable rcast rcast


toRecords :: (Key cube (FieldRec ks), RUnion ks vs as, DataCube cube) => Keys cube (FieldRec ks) -> FieldCube cube ks vs -> [FieldRec as]
toRecords = C.toTable runion


toKnownRecords :: (Key cube (FieldRec ks), RUnion ks vs as, DataCube cube) => FieldCube cube ks vs -> [FieldRec as]
toKnownRecords = C.toKnownTable runion


τ :: (b ⊆ a) => FieldRec a -> FieldRec b
τ = rcast


(!) :: (Key cube (FieldRec ks), DataCube cube) => FieldCube cube ks vs -> FieldRec ks -> FieldRec vs
(!) = (fromMaybe (error "FieldCube: key not found.") .) . C.evaluate


σ :: DataCube cube => (FieldRec ks -> FieldRec vs -> Bool) -> FieldCube cube ks vs -> FieldCube cube ks vs
σ = C.selectWithKey


π :: DataCube cube => (FieldRec ks -> FieldRec vs -> FieldRec ws) -> FieldCube cube ks vs -> FieldCube cube ks ws
π = C.projectWithKey


ρ :: (DataCube cube, Key cube (FieldRec ks), Ord (FieldRec ks)) => Set (FieldRec ks) -> FieldCube cube ks vs -> ks *↝ vs
ρ = C.reify


type FieldGregator as bs = C.Gregator (FieldRec as) (FieldRec bs)


κ :: (ks0 ⊆ ks, ks ⊆ (ks' ++ ks0), ks' ⊆ ks, Key cube (FieldRec ks), Key cube (FieldRec ks'), DataCube cube) => Set (FieldRec ks0) -> (FieldRec ks' -> [FieldRec vs] -> FieldRec vs') -> FieldCube cube ks vs -> FieldCube cube ks' vs' -- FIXME: Instead of subset, use sum.
κ keys =
  C.aggregateWithKey
    C.Gregator
    {
      C.aggregator    = rcast
    , C.disaggregator = flip map (S.toAscList keys) . (rcast .) . (<+>)
    }


δ :: (ks0 ⊆ ks', ks' ⊆ (ks ++ ks0), ks ⊆ ks', Key cube (FieldRec ks), Key cube (FieldRec ks'), DataCube cube) => Set (FieldRec ks0) -> (FieldRec ks' -> FieldRec vs -> FieldRec vs') -> FieldCube cube ks vs -> FieldCube cube ks' vs' -- FIXME: Instead of subset, use sum.
δ keys =
  C.disaggregateWithKey
    C.Gregator
    {
      C.aggregator    = rcast
    , C.disaggregator = flip map (S.toAscList keys) . (rcast .) . (<+>)
    }


ω :: (ks' ⊆ ks, Key cube (FieldRec ks'), DataCube cube) => FieldCube cube ks vs -> Keys cube (FieldRec ks')
ω = C.projectKnownKeys rcast


υ :: (Key cube k', DataCube cube) => (FieldRec ks -> k') -> FieldCube cube ks vs -> Keys cube k'
υ = C.projectKnownKeys


(⋈) :: (Eq (FieldRec (Intersection kLeft kRight)), Intersection kLeft kRight ⊆ kLeft, Intersection kLeft kRight ⊆ kRight, kLeft ⊆ k, kRight ⊆ k, RUnion kLeft kRight k, RUnion vLeft vRight v, RDistinct vLeft vRight, Key cubeLeft (FieldRec kLeft), Key cubeRight (FieldRec kRight), Key (Join cubeLeft cubeRight) (FieldRec k), DataCube cubeLeft, DataCube cubeRight, Joinable cubeLeft cubeRight) -- FIXME: This can be simplified somewhat.
    => FieldCube cubeLeft kLeft vLeft -> FieldCube cubeRight kRight vRight -> FieldCube (Join cubeLeft cubeRight) k v
(⋈) = C.join (C.Joiner rjoin rcast rcast) runion
infixl 6 ⋈


(⋉) :: (Key cube (FieldRec kRight), kRight ⊆ kLeft, DataCube cube)
    => FieldCube cube kLeft vLeft -> FieldCube cube kRight vRight -> FieldCube cube kLeft vLeft
(⋉) = C.semijoin $ C.Joiner undefined undefined rcast
infixl 6 ⋉


(▷) :: (Key cube (FieldRec kRight), kRight ⊆ kLeft, DataCube cube)
    => FieldCube cube kLeft vLeft -> FieldCube cube kRight vRight -> FieldCube cube kLeft vLeft
(▷) = C.antijoin $ C.Joiner undefined undefined rcast
infixl 6 ▷


(×) :: Set (FieldRec ks) -> Set (FieldRec ks') -> Set (FieldRec (ks ++ ks'))
(×) s1 s2 = S.fromDistinctAscList $ liftA2 (<+>) (S.toAscList s1) (S.toAscList s2)
infixl 8 ×
