{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Data.Daft.Vinyl.FieldCube.MongoDB (
  insertMany
, insertMany_
, insertAll
, insertAll_
, saveAll
, select
, selectKey
, rest
) where


import Control.Monad.Except (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bson (Field(..), Value)
import Data.Bson.Generic (FromBSON(..), ToBSON(..))
import Data.Daft.DataCube (DataCube(Key))
import Data.Daft.Vinyl.FieldCube (FieldCube, type (*↝))
import Data.Daft.Vinyl.FieldCube.Bson (keysAsIds, idsAsKeys)
import Data.Daft.Vinyl.FieldRec.Bson ()
import Data.Text (append)
import Data.Vinyl.Derived (FieldRec)
import Database.MongoDB (Action, Collection, Cursor, Select)

import qualified Database.MongoDB.Query as M


insertMany :: (MonadIO m, Key c (FieldRec ks), DataCube c, Ord (FieldRec ks), ToBSON (FieldRec ks), ToBSON (FieldRec vs)) => Collection -> FieldCube c ks vs -> Action m [Value]
insertMany = (. keysAsIds) . M.insertMany


insertMany_ :: (MonadIO m, Key c (FieldRec ks), DataCube c, Ord (FieldRec ks), ToBSON (FieldRec ks), ToBSON (FieldRec vs)) => Collection -> FieldCube c ks vs -> Action m ()
insertMany_ = (. keysAsIds) . M.insertMany_


insertAll :: (MonadIO m, Key c (FieldRec ks), DataCube c, Ord (FieldRec ks), ToBSON (FieldRec ks), ToBSON (FieldRec vs)) => Collection -> FieldCube c ks vs -> Action m [Value]
insertAll = (. keysAsIds) . M.insertAll


insertAll_ :: (MonadIO m, Key c (FieldRec ks), DataCube c, Ord (FieldRec ks), ToBSON (FieldRec ks), ToBSON (FieldRec vs)) => Collection -> FieldCube c ks vs -> Action m ()
insertAll_ = (. keysAsIds) . M.insertAll_


saveAll :: (MonadIO m, Key c (FieldRec ks), DataCube c, Ord (FieldRec ks), ToBSON (FieldRec ks), ToBSON (FieldRec vs)) => Collection -> FieldCube c ks vs -> Action m ()
saveAll collection =  mapM_ (M.save collection) . keysAsIds


select :: (ToBSON (FieldRec rs), Select aQueryOrSelection) => FieldRec rs -> Collection -> aQueryOrSelection
select = M.select . toBSON


selectKey :: (Select aQueryOrSelection, ToBSON (FieldRec rs)) => FieldRec rs -> Collection -> aQueryOrSelection
selectKey = M.select . fmap prependId . toBSON


prependId :: Field -> Field
prependId (k := v) = "_id." `append` k := v


rest :: (MonadIO m, MonadBaseControl IO m, Ord (FieldRec ks), FromBSON (FieldRec ks), FromBSON (FieldRec vs)) => Cursor -> Action m (Maybe (ks *↝ vs))
rest = fmap idsAsKeys . M.rest
