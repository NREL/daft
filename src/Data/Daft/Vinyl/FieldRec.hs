{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Data.Daft.Vinyl.FieldRec (
  fieldMap'
, (<:)
, labels
, readFieldRec
, readFieldRecCheckingLabels
, readFieldRecs
, showFieldRec
, showFieldRecs
, readFieldRecFile
, readFieldRecSource
, writeFieldRecFile
, writeFieldRecSource
, naturalJoin
, crossJoin
) where


import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Control.Monad.Except (MonadError, MonadIO, throwError)
import Control.Monad.Except.Util (tryIO)
import Data.Daft.Source (DataSource(..))
import Data.Daft.TypeLevel (Intersection)
import Data.Daft.Vinyl.TypeLevel (RDistinct, RJoin(rjoin), RUnion(runion))
import Data.Default (Default(..))
import Data.List (intercalate, nub)
import Data.List.Split (splitOn)
import Data.List.Util (elemPermutation)
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString(..))
import Data.String.ToString (ToString(..))
import Data.String.Util (readExcept)
import Data.Vinyl.Core (Dict(..), Rec(..), recordToList, reifyConstraint, rmap)
import Data.Vinyl.Derived (ElField(..), FieldRec)
import Data.Vinyl.Functor (Compose(Compose), Const(Const))
import Data.Vinyl.Lens (type (∈), type (⊆), rget)
import Data.Vinyl.TypeLevel (RecAll)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Vinyl.Derived as V (getField)


-- Map a field to a new symbol and type.
fieldMap' :: KnownSymbol t => (a -> b) -> ElField '(s, a) -> ElField '(t, b)
fieldMap' f (Field x) = Field (f x)


-- Extract a field's data from a record.
(<:) :: ('(s, t) ∈ rs) => sing '(s, t) -> FieldRec rs -> t
(<:) p r = V.getField (rget p r)


-- Extract the labels from a record.
labels :: (IsString s, RecAll ElField fields InternalLabeled) => FieldRec fields -> [s]
labels =
  recordToList
    . rmap (\(Compose (Dict x)) -> Const . fromString $ label x)
    . reifyConstraint (Proxy :: Proxy InternalLabeled)


-- Internal class for extracting labels from fields.
class InternalLabeled a where
  label :: a -> String

instance KnownSymbol s => InternalLabeled (ElField '(s, t)) where
  label _ = symbolVal (Proxy :: Proxy s)


-- Internal class for reading a record.
class InternalReadFieldRec (rs :: [(Symbol, *)]) where
  readFieldRec :: (ToString s, IsString e, MonadError e m) => [s] -> m (FieldRec rs)

instance InternalReadFieldRec '[] where
  readFieldRec [] = return RNil
  readFieldRec _  = throwError "too many data items for record" 

instance (KnownSymbol s, Read t, InternalReadFieldRec (rs :: [(Symbol, *)])) => InternalReadFieldRec ('(s, t) ': rs) where
  -- Why cannot we write the following using 'foldr'?
  readFieldRec (x : xs) = liftM2 (:&) (Field <$> readExcept x) (readFieldRec xs)
  readFieldRec []       = throwError "too few data items for record"


-- Internal class for creating default records.
class InternalDefault a where
  def' :: a

instance InternalDefault (FieldRec '[]) where
  def' = RNil

instance (KnownSymbol s, Default t, InternalDefault (FieldRec (rs :: [(Symbol, *)]))) => InternalDefault (FieldRec ('(s, t) ': rs)) where
  def' = Field def :& def'


-- Read a record by matching a header to the type signature.
readFieldRecCheckingLabels :: (Eq s, IsString s, ToString s, IsString e, MonadError e m, InternalDefault (FieldRec fields), RecAll ElField fields InternalLabeled, InternalReadFieldRec fields) => [s] -> [s] -> m (FieldRec fields)
readFieldRecCheckingLabels headerRow dataRow = head <$> readFieldRecs (headerRow : [dataRow])


-- Read records by matching a header to the type signature.
readFieldRecs :: (Eq s, IsString s, ToString s, IsString e, MonadError e m, InternalDefault (FieldRec fields), RecAll ElField fields InternalLabeled, InternalReadFieldRec fields) => [[s]] -> m [FieldRec fields]
readFieldRecs [] = throwError "a header must be specified"
readFieldRecs (headerRow : dataRows)
  | isNothing permutation       = throwError "header names do not match field names"
  | n /= length signature       = throwError "header and field names differ in length"
  | n /= length (nub headerRow) = throwError "header contains duplicate names"
  | otherwise                   = if True then mapM (readFieldRec =<<) dataRows' else return [u]
    where 
      n = length headerRow
      u = def'
      signature = labels u
      permutation = elemPermutation headerRow signature
      permuteRow dataRow
        | n /= length dataRow = throwError . fromString $ "incorrect number of data items in " ++ show (map toString dataRow)
        | otherwise           = return . map (dataRow !!) $ fromJust permutation
      dataRows' = map permuteRow dataRows


-- Auxilliary class for showing a record.
class InternalShowFieldRec (rs :: [(Symbol, *)]) where
  showFieldRec :: FieldRec rs -> [String]

instance InternalShowFieldRec '[] where
  showFieldRec RNil = []

instance (KnownSymbol s, Show t, InternalShowFieldRec (rs :: [(Symbol, *)])) => InternalShowFieldRec ('(s, t) ': rs) where
  showFieldRec (Field x :& xs) = show x : showFieldRec xs


-- Show records with a header from the type signature.
showFieldRecs :: (InternalDefault (FieldRec fields), RecAll ElField fields InternalLabeled, InternalShowFieldRec fields) => [FieldRec fields] -> [[String]]
showFieldRecs dataRows =
  (signature :)
    . tail
    $ showFieldRec
    <$> (u : dataRows)
    where
      u = def'
      signature = labels u


readFieldRecFile :: (IsString e, MonadError e m, MonadIO m, InternalDefault (FieldRec fields), RecAll ElField fields InternalLabeled, InternalReadFieldRec fields) => FilePath -> m [FieldRec fields]
readFieldRecFile =
  ((readFieldRecs . fmap (splitOn "\t") . lines) =<<)
    . tryIO . readFile


readFieldRecSource :: (IsString e, MonadError e m, MonadIO m, InternalDefault (FieldRec fields), RecAll ElField fields InternalLabeled, InternalReadFieldRec fields) => DataSource a -> m [FieldRec fields]
readFieldRecSource FileData{..}    = readFieldRecFile filePath
readFieldRecSource TextData{..}    = readFieldRecs . fmap (splitOn "\t") $ lines parsableText
readFieldRecSource BuiltinData{..} = throwError "Cannot read records from built-in data source."
readFieldRecSource NoData          = return []


writeFieldRecFile :: (IsString e, MonadError e m, MonadIO m, InternalDefault (FieldRec fields), RecAll ElField fields InternalLabeled, InternalShowFieldRec fields) => FilePath -> [FieldRec fields] -> m ()
writeFieldRecFile =
  (tryIO .)
    . (. (unlines . fmap (intercalate "\t") . showFieldRecs))
    . writeFile


writeFieldRecSource :: (IsString e, MonadError e m, MonadIO m, InternalDefault (FieldRec fields), RecAll ElField fields InternalLabeled, InternalShowFieldRec fields) => DataSource a -> [FieldRec fields] -> m (Maybe String)
writeFieldRecSource FileData{..} =
  (>> return Nothing)
    . writeFieldRecFile filePath
writeFieldRecSource TextData{..} =
  return
    . Just
    . unlines
    . fmap (intercalate "\t")
    . showFieldRecs
writeFieldRecSource BuiltinData{..} =
  const
    $ throwError "Cannot write records to built-in data source."
writeFieldRecSource NoData =
  const
    $ return Nothing


naturalJoin :: forall as bs cs
            .  (Eq (FieldRec (Intersection as bs)), Intersection as bs ⊆ as, Intersection as bs ⊆ bs, RUnion as bs cs)
            => [FieldRec as] -> [FieldRec bs] -> [FieldRec cs]
-- FIXME: We could simplify the constaints if the type system could make inteferences about the relationships between unions, intersections, and subsets.
naturalJoin = (catMaybes .) . liftA2 rjoin -- FIXME: This is an O(n^2) algorithm!  Would it be worthwhile to use 'Data.MultiMap' to organize intermediate computations?


crossJoin :: (RUnion as bs cs, RDistinct as bs)
          => [FieldRec as] -> [FieldRec bs] -> [FieldRec cs]
crossJoin = liftA2 runion
