{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Data.Daft.Vinyl.FieldRec.MongoDB (
  insert
, insert_
, insertMany
, insertMany_
, insertAll
, insertAll_
, select
, selectId
, findOne
, next
, rest
) where


import Control.Monad.Except (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bson (Value)
import Data.Bson.Generic (FromBSON(..), ToBSON(..))
import Data.Maybe (mapMaybe)
import Data.Daft.Vinyl.FieldRec.Bson ()
import Data.Vinyl.Derived (FieldRec)
import Database.MongoDB (Action, Collection, Cursor, Query, Select, (=:))

import qualified Database.MongoDB.Query as M


insert :: (MonadIO m, ToBSON (FieldRec rs)) => Collection -> FieldRec rs -> Action m Value
insert = (. toBSON) . M.insert


insert_ :: (MonadIO m, ToBSON (FieldRec rs)) => Collection -> FieldRec rs -> Action m ()
insert_ = (. toBSON) . M.insert_


insertMany :: (MonadIO m, ToBSON (FieldRec rs)) => Collection -> [FieldRec rs] -> Action m [Value]
insertMany = (. fmap toBSON) . M.insertMany


insertMany_ :: (MonadIO m, ToBSON (FieldRec rs)) => Collection -> [FieldRec rs] -> Action m ()
insertMany_ = (. fmap toBSON) . M.insertMany_


insertAll :: (MonadIO m, ToBSON (FieldRec rs)) => Collection -> [FieldRec rs] -> Action m [Value]
insertAll = (. fmap toBSON) . M.insertAll


insertAll_ :: (MonadIO m, ToBSON (FieldRec rs)) => Collection -> [FieldRec rs] -> Action m ()
insertAll_ = (. fmap toBSON) . M.insertAll_


select :: (ToBSON (FieldRec rs), Select aQueryOrSelection) => FieldRec rs -> Collection -> aQueryOrSelection
select = M.select . toBSON


selectId :: Select aQueryOrSelection => Value -> Collection -> aQueryOrSelection
selectId = M.select . (: []) . ("_id" =:)


findOne :: (MonadIO m, FromBSON (FieldRec rs)) => Query -> Action m (Maybe (FieldRec rs))
findOne = fmap (>>= fromBSON) . M.findOne


next :: (MonadIO m, MonadBaseControl IO m, FromBSON (FieldRec rs)) => Cursor -> Action m (Maybe (FieldRec rs))
next = fmap (>>= fromBSON) . M.next


rest :: (MonadIO m, MonadBaseControl IO m, FromBSON (FieldRec rs)) => Cursor -> Action m [FieldRec rs]
rest = fmap (mapMaybe fromBSON) . M.rest
