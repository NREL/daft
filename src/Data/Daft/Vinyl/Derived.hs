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
, labels
, readFieldRec
, readFieldRec'
, readFieldRecs
) where


import Control.Arrow ((&&&), (***))
import Control.Monad (liftM2)
import Data.Daft.String (readEither)
import Data.Default (Default(..))
import Data.List (elemIndex, nub)
import Data.Maybe (fromJust, isNothing)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl.Core (Dict(..), Rec(..), (<+>), recordToList, reifyConstraint, rmap)
import Data.Vinyl.Derived (ElField(..), FieldRec, (=:))
import Data.Vinyl.Functor (Compose(Compose), Const(Const))
import Data.Vinyl.Lens (type (∈), rget, rreplace)
import Data.Vinyl.TypeLevel (RecAll)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Vinyl.Derived as V (getField)


-- Auxilliary class for extracting labels from fields.
class Labelled a where
  label :: a -> String

instance KnownSymbol s => Labelled (ElField '(s, t)) where
  label _ = symbolVal (Proxy :: Proxy s)


-- Extract the labels from a record.
labels :: RecAll ElField fields Labelled => FieldRec fields -> [String]
labels =
  recordToList
    . rmap (\(Compose (Dict x)) -> Const $ label x)
    . reifyConstraint (Proxy :: Proxy Labelled)


-- Auxilliary class for reading a record.
class ReadFieldRec (rs :: [(Symbol, *)]) where
  readFieldRec :: [String] -> Either String (FieldRec rs)

instance ReadFieldRec '[] where
  readFieldRec [] = Right RNil
  readFieldRec _  = Left "too many data items for record" 

instance (KnownSymbol s, Read t, ReadFieldRec (rs :: [(Symbol, *)])) => ReadFieldRec ('(s, t) ': rs) where
  readFieldRec (x : xs) = liftM2 (:&) (Field <$> readEither x) (readFieldRec xs)
  readFieldRec []       = Left "too few data items for record"


-- Auxilliary class for creating default records.
class Default' a where
  def' :: a

instance Default' (FieldRec '[]) where
  def' = RNil

instance (KnownSymbol s, Default t, Default' (FieldRec (rs :: [(Symbol, *)]))) => Default' (FieldRec ('(s, t) ': rs)) where
  def' = Field def :& def'


-- Find the permutation of one list relative to another.
elemPermutation :: Eq a => [a] -> [a] -> Maybe [Int]
elemPermutation = mapM . flip elemIndex


-- Read a record by matching a header to the type signature.
readFieldRec' :: (Default' (FieldRec fields), RecAll ElField fields Labelled, ReadFieldRec fields) => [String] -> [String] -> Either String (FieldRec fields)
readFieldRec' headerRow dataRow = head <$> readFieldRecs (headerRow : [dataRow])


-- Read records by matching a header to the type signature.
readFieldRecs :: (Default' (FieldRec fields), RecAll ElField fields Labelled, ReadFieldRec fields) => [[String]] -> Either String [FieldRec fields]
readFieldRecs [] = Left "a header must be specified"
readFieldRecs (headerRow : dataRows)
  | isNothing permutation       = Left "header names do not match field names"
  | n /= length signature       = Left "header and field names differ in length"
  | n /= length (nub headerRow) = Left "header contains duplicate names"
  | otherwise                   = if True then mapM (readFieldRec =<<) dataRows' else Right [u]
    where 
      n = length headerRow
      u = def'
      signature = labels u
      permutation = elemPermutation headerRow signature
      permuteRow :: [String] -> Either String [String]
      permuteRow dataRow
        | n /= length dataRow = Left $ "incorrect number of data items in " ++ show dataRow
        | otherwise           = Right . map (dataRow !!) $ fromJust permutation
      dataRows' = map permuteRow dataRows


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
