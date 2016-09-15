{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Data.Daft.Vinyl.FieldCube.MongoDB (
  insertMany
, insertMany_
, insertAll
, insertAll_
, select
, selectKey
, rest
) where


import Control.Monad.Except (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bson (Field(..), Value)
import Data.Bson.Generic (FromBSON(..), ToBSON(..))
import Data.Daft.Vinyl.FieldCube (FieldCube)
import Data.Daft.Vinyl.FieldCube.Bson (keysAsIds, idsAsKeys)
import Data.Daft.Vinyl.FieldRec.Bson ()
import Data.Text (append)
import Data.Vinyl.Derived (FieldRec)
import Database.MongoDB (Action, Collection, Cursor, Select)

import qualified Database.MongoDB.Query as M


insertMany :: (MonadIO m, Ord (FieldRec ks), ToBSON (FieldRec ks), ToBSON (FieldRec vs)) => Collection -> FieldCube ks vs -> Action m [Value]
insertMany = (. keysAsIds) . M.insertMany


insertMany_ :: (MonadIO m, Ord (FieldRec ks), ToBSON (FieldRec ks), ToBSON (FieldRec vs)) => Collection -> FieldCube ks vs -> Action m ()
insertMany_ = (. keysAsIds) . M.insertMany_


insertAll :: (MonadIO m, Ord (FieldRec ks), ToBSON (FieldRec ks), ToBSON (FieldRec vs)) => Collection -> FieldCube ks vs -> Action m [Value]
insertAll = (. keysAsIds) . M.insertAll


insertAll_ :: (MonadIO m, Ord (FieldRec ks), ToBSON (FieldRec ks), ToBSON (FieldRec vs)) => Collection -> FieldCube ks vs -> Action m ()
insertAll_ = (. keysAsIds) . M.insertAll_


select :: (ToBSON (FieldRec rs), Select aQueryOrSelection) => FieldRec rs -> Collection -> aQueryOrSelection
select = M.select . toBSON


selectKey :: (Select aQueryOrSelection, ToBSON (FieldRec rs)) => FieldRec rs -> Collection -> aQueryOrSelection
selectKey = M.select . fmap prependId . toBSON


prependId :: Field -> Field
prependId (k := v) = "_id." `append` k := v


rest :: (MonadIO m, MonadBaseControl IO m, Ord (FieldRec ks), FromBSON (FieldRec ks), FromBSON (FieldRec vs)) => Cursor -> Action m (Maybe (FieldCube ks vs))
rest = fmap idsAsKeys . M.rest
