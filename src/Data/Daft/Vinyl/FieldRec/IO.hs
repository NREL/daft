{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Data.Daft.Vinyl.FieldRec.IO (
  readFieldRec
, readFieldRecCheckingLabels
, readFieldRecs
, readFieldRecFile
, readFieldRecSource
, showFieldRec
, showFieldRecs
, writeFieldRecFile
, writeFieldRecSource
, ReadFieldRec
, ShowFieldRec
) where


import Control.Monad (liftM2)
import Control.Monad.Except (MonadError, MonadIO, throwError)
import Control.Monad.Except.Util (tryIO)
import Data.Daft.Source (DataSource(..))
import Data.Daft.Vinyl.FieldRec (Labeled(..))
import Data.List (nub)
import Data.List.Util (elemPermutation)
import Data.List.Util.Listable (fromTabbeds, toTabbeds)
import Data.Maybe (fromJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import Data.String.ToString (ToString(..))
import Data.String.Util (readExcept)
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (ElField(..), FieldRec, )
import GHC.TypeLits (KnownSymbol, Symbol)


--  Auxilliary class for reading a record.
class ReadFieldRec (rs :: [(Symbol, *)]) where
  readFieldRec :: (ToString s, IsString e, MonadError e m) => [s] -> m (FieldRec rs)

instance ReadFieldRec '[] where
  readFieldRec [] = return RNil
  readFieldRec _  = throwError "too many data items for record" 

instance (KnownSymbol s, Read t, ReadFieldRec (rs :: [(Symbol, *)])) => ReadFieldRec ('(s, t) ': rs) where
  -- Why cannot we write the following using 'foldr'?
  readFieldRec (x : xs) = liftM2 (:&) (Field <$> readExcept x) (readFieldRec xs)
  readFieldRec []       = throwError "too few data items for record"


-- Read a record by matching a header to the type signature.
readFieldRecCheckingLabels :: (Eq s, IsString s, ToString s, IsString e, MonadError e m, Labeled (FieldRec fields), ReadFieldRec fields) => [s] -> [s] -> m (FieldRec fields)
readFieldRecCheckingLabels headerRow dataRow = head <$> readFieldRecs (headerRow : [dataRow])


-- Read records by matching a header to the type signature.
readFieldRecs :: forall s e m fields . (Eq s, IsString s, ToString s, IsString e, MonadError e m, Labeled (FieldRec fields), ReadFieldRec fields) => [[s]] -> m [FieldRec fields]
readFieldRecs [] = throwError "a header must be specified"
readFieldRecs (headerRow : dataRows)
  | isNothing permutation       = throwError "header names do not match field names"
  | n /= length signature       = throwError "header and field names differ in length"
  | n /= length (nub headerRow) = throwError "header contains duplicate names"
  | otherwise                   = mapM (readFieldRec =<<) dataRows'
    where 
      n = length headerRow
      signature = labels (Proxy :: Proxy (FieldRec fields))
      permutation = elemPermutation headerRow signature
      permuteRow dataRow
        | n /= length dataRow = throwError . fromString $ "incorrect number of data items in " ++ show (map toString dataRow)
        | otherwise           = return . map (dataRow !!) $ fromJust permutation
      dataRows' = map permuteRow dataRows


-- Auxilliary class for showing a record.
class ShowFieldRec (rs :: [(Symbol, *)]) where
  showFieldRec :: FieldRec rs -> [String]

instance ShowFieldRec '[] where
  showFieldRec RNil = []

instance (KnownSymbol s, Show t, ShowFieldRec (rs :: [(Symbol, *)])) => ShowFieldRec ('(s, t) ': rs) where
  showFieldRec (Field x :& xs) = show x : showFieldRec xs


-- Show records with a header from the type signature.
showFieldRecs :: forall fields . (Labeled (FieldRec fields), ShowFieldRec fields) => [FieldRec fields] -> [[String]]
showFieldRecs dataRows =
  (signature :)
    $ showFieldRec
    <$> dataRows
    where
      signature = labels (Proxy :: Proxy (FieldRec fields))


readFieldRecFile :: (IsString e, MonadError e m, MonadIO m, Labeled (FieldRec fields), ReadFieldRec fields) => FilePath -> m [FieldRec fields]
readFieldRecFile =
  ((readFieldRecs . fromTabbeds) =<<)
    . tryIO . readFile


readFieldRecSource :: (IsString e, MonadError e m, MonadIO m, Labeled (FieldRec fields), ReadFieldRec fields) => DataSource a -> m [FieldRec fields]
readFieldRecSource FileData{..}    = readFieldRecFile filePath
readFieldRecSource TextData{..}    = readFieldRecs $ fromTabbeds parsableText
readFieldRecSource BuiltinData{..} = throwError "Cannot read records from built-in data source."
readFieldRecSource NoData          = return []


writeFieldRecFile :: (IsString e, MonadError e m, MonadIO m, Labeled (FieldRec fields), ShowFieldRec fields) => FilePath -> [FieldRec fields] -> m ()
writeFieldRecFile =
  (tryIO .)
    . (. (toTabbeds . showFieldRecs))
    . writeFile


writeFieldRecSource :: (IsString e, MonadError e m, MonadIO m, Labeled (FieldRec fields), ShowFieldRec fields) => DataSource a -> [FieldRec fields] -> m (Maybe String)
writeFieldRecSource FileData{..} =
  (>> return Nothing)
    . writeFieldRecFile filePath
writeFieldRecSource TextData{..} =
  return
    . Just
    . toTabbeds
    . showFieldRecs
writeFieldRecSource BuiltinData{..} =
  const
    $ throwError "Cannot write records to built-in data source."
writeFieldRecSource NoData =
  const
    $ return Nothing
