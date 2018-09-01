{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Data.Daft.Vinyl.FieldRec.Bson (
) where


import Data.Bson (Val(..), Field(..))
import Data.Bson.Generic (FromBSON(..), ToBSON(..))
import Data.Proxy (Proxy(..))
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (ElField(..), FieldRec)
import Data.Text (pack)
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Data.Bson as B (lookup)


instance ToBSON (FieldRec '[]) where
  toBSON RNil = []

instance (KnownSymbol s, Val t, ToBSON (FieldRec rs)) => ToBSON (FieldRec ('(s, t) ': rs)) where
  toBSON (Field x :& xs) = (lab := val x) : toBSON xs
    where
      lab = pack $ symbolVal (Proxy :: Proxy s)


instance FromBSON (FieldRec '[]) where
  fromBSON _ = return RNil

instance (KnownSymbol s, Val t, FromBSON (FieldRec rs)) => FromBSON (FieldRec ('(s, t) ': rs)) where
  fromBSON doc =
    do
      let
        lab = pack $ symbolVal (Proxy :: Proxy s)
      x <- lab `B.lookup` doc :: Maybe t
      (Field x :&) <$> fromBSON doc -- FIXME: Do we need to strip out the field just found?
