{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Data.Daft.Vinyl.FieldCube.Bson (
  keysAsIds
, idsAsKeys
, toArray
, fromArray
) where


import Data.Bson (Document, (=:))
import Data.Bson.Generic (FromBSON(..), ToBSON(..))
import Data.Daft.DataCube (fromTableM, toKnownTable)
import Data.Daft.Vinyl.FieldCube (FieldCube)
import Data.Daft.Vinyl.FieldRec.Bson ()
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.TypeLevel (type (++))

import qualified Data.Bson as B (lookup)


keysAsIds :: (Ord (FieldRec ks), ToBSON (FieldRec ks), ToBSON (FieldRec vs)) => FieldCube ks vs -> [Document]
keysAsIds = toKnownTable $ (. toBSON) . (:) . ("_id" =:) . toBSON


toArray :: (Ord (FieldRec ks), ToBSON (FieldRec (ks ++ vs))) => FieldCube ks vs -> [Document]
toArray = toKnownTable $ (toBSON .) . (<+>)


idsAsKeys :: forall ks vs . (Ord (FieldRec ks), FromBSON (FieldRec ks), FromBSON (FieldRec vs)) => [Document] -> Maybe (FieldCube ks vs)
idsAsKeys = fromTableM ((fromBSON =<<) . B.lookup "_id") fromBSON
    

fromArray :: forall ks vs . (Ord (FieldRec ks), FromBSON (FieldRec ks), FromBSON (FieldRec vs)) => [Document] -> Maybe (FieldCube ks vs)
fromArray = fromTableM fromBSON fromBSON
