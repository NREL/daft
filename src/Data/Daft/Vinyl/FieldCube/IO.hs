{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Data.Daft.Vinyl.FieldCube.IO (
-- * Input
  readFieldCube
, readFieldCubeFile
, readFieldCubeSource
-- * Output
, showFieldCube
, writeFieldCubeFile
, writeFieldCubeSource
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Daft.Source (DataSource(..))
import Data.Daft.TypeLevel (Union)
import Data.Daft.Vinyl.FieldCube (FieldCube)
import Data.Daft.Vinyl.FieldRec (InternalLabeled)
import Data.Daft.Vinyl.FieldRec.IO (InternalDefault, InternalReadFieldRec, InternalShowFieldRec, readFieldRecs, readFieldRecFile, readFieldRecSource, showFieldRecs, writeFieldRecFile, writeFieldRecSource)
import Data.String (IsString(..))
import Data.String.ToString (ToString(..))
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (ElField(..), FieldRec, )
import Data.Vinyl.Lens (type (⊆), rcast)
import Data.Vinyl.TypeLevel (RecAll, type (++))

import qualified Data.Daft.DataCube as C (fromTable, toKnownTable)


readFieldCube :: forall ks vs s e m . (ks ⊆ Union ks vs, vs ⊆ Union ks vs, Ord (FieldRec ks), Eq s, IsString s, ToString s, IsString e, MonadError e m, InternalDefault (FieldRec (Union ks vs)), RecAll ElField (Union ks vs) InternalLabeled, InternalReadFieldRec (Union ks vs)) => [[s]] -> m (FieldCube ks vs)
readFieldCube =
  fmap (C.fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs))
    . readFieldRecs


readFieldCubeFile :: forall ks vs e m . (ks ⊆ Union ks vs, vs ⊆ Union ks vs, Ord (FieldRec ks), IsString e, MonadError e m, MonadIO m, InternalDefault (FieldRec (Union ks vs)), RecAll ElField (Union ks vs) InternalLabeled, InternalReadFieldRec (Union ks vs)) => FilePath -> m (FieldCube ks vs)
readFieldCubeFile =
  fmap (C.fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs)) 
    . readFieldRecFile


readFieldCubeSource :: forall ks vs e m a . (ks ⊆ Union ks vs, vs ⊆ Union ks vs, Ord (FieldRec ks), IsString e, MonadError e m, MonadIO m, InternalDefault (FieldRec (Union ks vs)), RecAll ElField (Union ks vs) InternalLabeled, InternalReadFieldRec (Union ks vs)) => DataSource a -> m (FieldCube ks vs)
readFieldCubeSource =
  fmap (C.fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs)) 
    . readFieldRecSource


showFieldCube :: forall ks vs . (Ord (FieldRec ks), InternalDefault (FieldRec (ks ++ vs)), RecAll ElField (ks ++ vs) InternalLabeled, InternalShowFieldRec (ks ++ vs)) => FieldCube ks vs -> [[String]]
showFieldCube = showFieldRecs . C.toKnownTable ((<+>) :: FieldRec ks -> FieldRec vs -> FieldRec (ks ++ vs))


writeFieldCubeFile :: forall ks vs e m . (Ord (FieldRec ks), InternalDefault (FieldRec (ks ++ vs)), RecAll ElField (ks ++ vs) InternalLabeled, InternalShowFieldRec (ks ++ vs), IsString e, MonadError e m, MonadIO m) => FilePath -> FieldCube ks vs -> m ()
writeFieldCubeFile = (. C.toKnownTable ((<+>) :: FieldRec ks -> FieldRec vs -> FieldRec (ks ++ vs))) . writeFieldRecFile


writeFieldCubeSource :: forall ks vs e m a . (Ord (FieldRec ks), InternalDefault (FieldRec (ks ++ vs)), RecAll ElField (ks ++ vs) InternalLabeled, InternalShowFieldRec (ks ++ vs), IsString e, MonadError e m, MonadIO m) => DataSource a -> FieldCube ks vs -> m (Maybe String)
writeFieldCubeSource = (. C.toKnownTable ((<+>) :: FieldRec ks -> FieldRec vs -> FieldRec (ks ++ vs))) . writeFieldRecSource
