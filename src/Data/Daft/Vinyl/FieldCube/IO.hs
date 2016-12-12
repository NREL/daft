{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Data.Daft.Vinyl.FieldCube.IO (
-- * Input
  readFieldCube
, readFieldCubeFile
, readFieldCubeSource
, readFieldCubeSource'
-- * Output
, showFieldCube
, writeFieldCubeFile
, writeFieldCubeSource
) where


import Control.Monad.Except (MonadError, MonadIO, throwError)
import Data.Daft.DataCube (DataCube)
import Data.Daft.Source (DataSource(..))
import Data.Daft.TypeLevel (Union)
import Data.Daft.Vinyl.FieldCube (FieldCube, type (*↝))
import Data.Daft.Vinyl.FieldRec (Labeled)
import Data.Daft.Vinyl.FieldRec.Instances ()
import Data.Daft.Vinyl.FieldRec.IO (ReadFieldRec, ShowFieldRec, readFieldRecs, readFieldRecFile, readFieldRecSource, showFieldRecs, writeFieldRecFile, writeFieldRecSource)
import Data.Default.Util (Unknown(..))
import Data.List.Util.Listable  (fromTabbeds)
import Data.Map.Strict (empty)
import Data.String (IsString(..))
import Data.String.ToString (ToString(..))
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆), rcast)
import Data.Vinyl.TypeLevel (type (++))

import qualified Data.Daft.DataCube as C (DataCube(..))
import qualified Data.Daft.DataCube.Table as C (fromTable)


readFieldCube :: forall ks vs s e m . (ks ⊆ Union ks vs, vs ⊆ Union ks vs, Ord (FieldRec ks), Eq s, IsString s, ToString s, IsString e, MonadError e m, Labeled (FieldRec (Union ks vs)), ReadFieldRec (Union ks vs)) => [[s]] -> m (ks *↝ vs)
readFieldCube =
  fmap (C.fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs))
    . readFieldRecs


readFieldCubeFile :: forall ks vs e m . (ks ⊆ Union ks vs, vs ⊆ Union ks vs, Ord (FieldRec ks), IsString e, MonadError e m, MonadIO m, Labeled (FieldRec (Union ks vs)), ReadFieldRec (Union ks vs)) => FilePath -> m (ks *↝ vs)
readFieldCubeFile =
  fmap (C.fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs)) 
    . readFieldRecFile


readFieldCubeSource :: forall ks vs e m a . (ks ⊆ Union ks vs, vs ⊆ Union ks vs, Ord (FieldRec ks), IsString e, MonadError e m, MonadIO m, Labeled (FieldRec (Union ks vs)), ReadFieldRec (Union ks vs)) => DataSource a -> m (ks *↝ vs)
readFieldCubeSource =
  fmap (C.fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs)) 
    . readFieldRecSource


readFieldCubeSource' :: forall ks vs e m a . (ks ⊆ Union ks vs, vs ⊆ Union ks vs, Ord (FieldRec ks), Unknown (FieldRec vs), IsString e, MonadError e m, MonadIO m, Labeled (FieldRec (Union ks vs)), ReadFieldRec (Union ks vs)) => DataSource a -> m (ks *↝ vs)
readFieldCubeSource' FileData{..}    = C.fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs) <$> readFieldRecFile filePath
readFieldCubeSource' TextData{..}    = fmap (C.fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs)) . readFieldRecs $ fromTabbeds parsableText
readFieldCubeSource' BuiltinData{..} = throwError "Cannot read records from built-in data source."
readFieldCubeSource' NoData          = return empty


showFieldCube :: forall c ks vs . (DataCube c, Ord (FieldRec ks), Labeled (FieldRec (ks ++ vs)), ShowFieldRec (ks ++ vs)) => (FieldCube c) ks vs -> [[String]]
showFieldCube = showFieldRecs . C.toKnownTable ((<+>) :: FieldRec ks -> FieldRec vs -> FieldRec (ks ++ vs))


writeFieldCubeFile :: forall c ks vs e m . (DataCube c, Ord (FieldRec ks), Labeled (FieldRec (ks ++ vs)), ShowFieldRec (ks ++ vs), IsString e, MonadError e m, MonadIO m) => FilePath -> (FieldCube c) ks vs -> m ()
writeFieldCubeFile = (. C.toKnownTable ((<+>) :: FieldRec ks -> FieldRec vs -> FieldRec (ks ++ vs))) . writeFieldRecFile


writeFieldCubeSource :: forall c ks vs e m a . (DataCube c, Ord (FieldRec ks), Labeled (FieldRec (ks ++ vs)), ShowFieldRec (ks ++ vs), IsString e, MonadError e m, MonadIO m) => DataSource a -> (FieldCube c) ks vs -> m (Maybe String)
writeFieldCubeSource = (. C.toKnownTable ((<+>) :: FieldRec ks -> FieldRec vs -> FieldRec (ks ++ vs))) . writeFieldRecSource
