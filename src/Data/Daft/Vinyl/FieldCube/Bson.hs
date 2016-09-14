{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}


module Data.Daft.Vinyl.FieldCube.Bson (
  toArray
, fromArray
) where


import Data.Bson (Document)
import Data.Bson.Generic (FromBSON(..), ToBSON(..))
import Data.Daft.DataCube (fromTable, toKnownTable)
import Data.Daft.TypeLevel (Union)
import Data.Daft.Vinyl.FieldCube (FieldCube)
import Data.Daft.Vinyl.FieldRec.Bson ()
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆), rcast)
import Data.Vinyl.TypeLevel (type (++))


toArray :: (Ord (FieldRec ks), ToBSON (FieldRec (ks ++ vs))) => FieldCube ks vs -> [Document]
toArray =
  map toBSON
    . toKnownTable (<+>)


fromArray :: forall ks vs . (Ord (FieldRec ks), FromBSON (FieldRec (Union ks vs)), ks ⊆ Union ks vs, vs ⊆ Union ks vs) => [Document] -> Maybe (FieldCube ks vs)
fromArray =
  fmap (fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs))
    . mapM fromBSON
