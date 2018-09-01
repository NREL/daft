{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Data.Daft.Vinyl.FieldRec.Aeson (
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..), Value(Object), (.:), emptyObject, withObject)
import Data.HashMap.Strict (insert)
import Data.Proxy (Proxy(..))
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (ElField(..), FieldRec)
import Data.Text (pack)
import GHC.TypeLits (KnownSymbol, symbolVal)


instance ToJSON (FieldRec '[]) where
  toJSON RNil = emptyObject

instance (KnownSymbol s, ToJSON t, ToJSON (FieldRec rs)) => ToJSON (FieldRec ('(s, t) ': rs)) where
  toJSON (Field x :& xs) = Object $ insert lab (toJSON x) doc
    where
      lab = pack $ symbolVal (Proxy :: Proxy s)
      Object doc = toJSON xs


instance FromJSON (FieldRec '[]) where
  parseJSON _ = return RNil

instance (KnownSymbol s, FromJSON t, FromJSON (FieldRec rs)) => FromJSON (FieldRec ('(s, t) ': rs)) where
  parseJSON doc =
    flip (withObject "FieldRec") doc $ \o ->
      do
        let
          lab = pack $ symbolVal (Proxy :: Proxy s)
        x <- o .: lab
        (Field x :&) <$> parseJSON doc -- FIXME: Do we need to strip out the field just found?
