{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Data.Daft.Vinyl.Derived (
  (<:)
, using1
, using2
, replacing1
, replacing2
, keyed
, unkeyed
, aggregating1
) where


import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (***))
import Data.Daft.Keyed (Keyed(..))
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec, (=:))
import Data.Vinyl.Lens (type (∈), type (⊆), rcast, rget, rreplace)
import Data.Vinyl.TypeLevel (type (++))
import GHC.TypeLits (KnownSymbol)

import qualified Data.Vinyl.Derived as V (getField)


-- Extract a field's data from a record.
(<:) :: ('(s, t) ∈ rs) => sing '(s, t) -> FieldRec rs -> t
(<:) p r = V.getField (rget p r)


-- TODO: It may be possible to unify the various arities in a type class "Applying" with functions "using" and "replacing", if the functional dependencies for the class can be specified.  This might look something like the following:
--
-- class Using sta stb ta tb f f' | sta -> ta, stb -> tb, sta -> f, sta -> f' where
--   using :: (ta -> tb) -> (sta, stb) -> f -> f'
--
-- class Replacing sta ta f | sta -> ta, sta -> f where
--   replacing :: (ta -> ta) -> sta -> f -> f
--


-- Apply a function to extract one field of a record.
using1 :: (KnownSymbol s1, KnownSymbol s2, '(s1, t1) ∈ fields) => (t1 -> t2) -> (sing '(s1, t1), sing '(s2, t2)) -> FieldRec fields -> FieldRec '[ '(s2, t2) ]
using1 fun (f1, f2) =
  (f2 =:) . fun . (f1 <:)


-- Apply a function to extract two fields of a record.
using2 :: (KnownSymbol s1, KnownSymbol s2, KnownSymbol s3, KnownSymbol s4, '(s1, t1) ∈ fields, '(s2, t2) ∈ fields) => ((t1, t2) -> (t3, t4)) -> ((sing '(s1, t1), sing '(s2, t2)), (sing '(s3, t3), sing '(s4, t4))) -> FieldRec fields -> FieldRec '[ '(s3, t3), '(s4, t4) ]
using2 fun ((f1, f2), (f3, f4)) =
  uncurry (<+>) . ((f3 =:) *** (f4 =:)) . fun . ((f1 <:) &&& (f2 <:))


-- Apply a function to update one field of a record.
replacing1 :: (KnownSymbol s1, '(s1, t1) ∈ fields) => (t1 -> t1) -> sing '(s1, t1) -> FieldRec fields -> FieldRec fields
replacing1 fun f1 =
  uncurry rreplace . (using1 fun (f1, f1) &&& id)


-- Apply a function to update two fields of a record.
replacing2 :: (KnownSymbol s1, KnownSymbol s2, '(s1, t1) ∈ fields, '(s2, t2) ∈ fields) => ((t1, t2) -> (t1, t2)) -> (sing '(s1, t1), sing '(s2, t2)) -> FieldRec fields -> FieldRec fields
replacing2 fun f12 =
  uncurry rreplace . (using2 fun (f12, f12) &&& id)


-- Separate a key and value from a record.
keyed :: (ks ⊆ rs, vs ⊆ rs) => FieldRec rs -> Keyed (FieldRec ks) (FieldRec vs)
keyed = liftA2 Keyed rcast rcast


-- Merge a key and value into a record
unkeyed :: Keyed (FieldRec ks) (FieldRec vs) -> FieldRec (ks ++ vs)
unkeyed (Keyed k v) = k <+> v

-- Aggregate several recods into a single record.
aggregating1 :: (KnownSymbol s1, KnownSymbol s2, '(s1, t1) ∈ fields) => ([t1] -> t2) -> (sing '(s1, t1), sing '(s2, t2)) -> [FieldRec fields] -> FieldRec '[ '(s2, t2) ]
aggregating1 fun (f1, f2) =
  (f2 =:) . fun . map (f1 <:)
