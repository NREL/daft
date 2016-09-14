{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}


module Data.Daft.Vinyl.FieldCube.Aeson (
  toArray
, fromArray
) where


import Data.Aeson.Types (FromJSON(..), Parser, ToJSON(..), Value)
import Data.Daft.DataCube (fromTable, toKnownTable)
import Data.Daft.TypeLevel (Union)
import Data.Daft.Vinyl.FieldCube (FieldCube)
import Data.Daft.Vinyl.FieldRec.Aeson ()
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆), rcast)
import Data.Vinyl.TypeLevel (type (++))


toArray :: (Ord (FieldRec ks), ToJSON (FieldRec (ks ++ vs))) => FieldCube ks vs -> [Value]
toArray =
  map toJSON
    . toKnownTable (<+>)


fromArray :: forall ks vs . (Ord (FieldRec ks), FromJSON (FieldRec (Union ks vs)), ks ⊆ Union ks vs, vs ⊆ Union ks vs) => [Value] -> Parser (FieldCube ks vs)
fromArray =
  fmap (fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs))
    . mapM parseJSON
