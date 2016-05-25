{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Data.Daft.Vinyl.Derived.ReadWrite (
  labels
, readFieldRec
, readFieldRec'
, readFieldRecs
, showFieldRec
, showFieldRecs
, displayFieldRecs
) where


import Control.Monad (liftM2)
import Data.Daft.String (readEither)
import Data.Default (Default(..))
import Data.List (elemIndex, intercalate, nub)
import Data.Maybe (fromJust, isNothing)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl.Core (Dict(..), Rec(..), recordToList, reifyConstraint, rmap)
import Data.Vinyl.Derived (ElField(..), FieldRec)
import Data.Vinyl.Functor (Compose(Compose), Const(Const))
import Data.Vinyl.TypeLevel (RecAll)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


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
  -- Why cannot we write the following using 'foldr'?
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


-- Auxilliary class for showing a record.
class ShowFieldRec (rs :: [(Symbol, *)]) where
  showFieldRec :: FieldRec rs -> [String]

instance ShowFieldRec '[] where
  showFieldRec RNil = []

instance (KnownSymbol s, Show t, ShowFieldRec (rs :: [(Symbol, *)])) => ShowFieldRec ('(s, t) ': rs) where
  showFieldRec (Field x :& xs) = show x : showFieldRec xs


-- Show records with a header from the type signature.
showFieldRecs :: (Default' (FieldRec fields), RecAll ElField fields Labelled, ShowFieldRec fields) => [FieldRec fields] -> [[String]]
showFieldRecs dataRows =
  (signature :)
    . tail
    $ showFieldRec
    <$> (u : dataRows)
    where
      u = def'
      signature = labels u


displayFieldRecs :: (Default' (FieldRec fields), RecAll ElField fields Labelled, ShowFieldRec fields) => [FieldRec fields] -> String
displayFieldRecs = unlines . map (intercalate "\t") . showFieldRecs
