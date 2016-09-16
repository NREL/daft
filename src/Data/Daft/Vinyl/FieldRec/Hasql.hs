{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}


module Data.Daft.Vinyl.FieldRec.Hasql (
  params
, row
, selectList
) where


import Data.ByteString.Char8 (pack)
import Data.Daft.Vinyl.FieldRec ((<:), labels, InternalLabeled)
import Data.Daft.Vinyl.FieldRec.IO (InternalDefault(..))
import Data.Default (Default(..))
import Data.Functor.Contravariant (contramap)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Data.Text (pack, unpack)
import Data.Vinyl.Core (Rec(..), rappend)
import Data.Vinyl.Derived (ElField(..), FieldRec)
import Data.Vinyl.Lens (type (∈))
import Data.Vinyl.TypeLevel (RecAll)
import GHC.TypeLits (KnownSymbol)

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Query as Q


class Paramsable a where
  params :: E.Params a

instance ParamsableWithProxy (FieldRec rs) (FieldRec rs) => Paramsable (FieldRec rs) where
  params = paramsWithProxy (Proxy :: Proxy (FieldRec rs))


class ParamsableWithProxy a b where
  paramsWithProxy :: proxy a -> E.Params b

instance ParamsableWithProxy (FieldRec '[]) (FieldRec rs) where
  paramsWithProxy _ = contramap (const ()) E.unit

instance (Default (E.Value t), '(s, t) ∈ rs, ParamsableWithProxy (FieldRec rs') (FieldRec rs)) => ParamsableWithProxy (FieldRec ('(s, t) ': rs')) (FieldRec rs) where
  paramsWithProxy _ = contramap ((Proxy :: Proxy '(s, t)) <:) (E.value def) <> paramsWithProxy (Proxy :: Proxy (FieldRec rs'))


instance Default (E.Value String) where
  def = contramap Data.Text.pack E.text


class Rowable a where
  row :: D.Row a

instance Rowable (FieldRec '[]) where
  row = pure RNil

instance (Default (D.Value t), KnownSymbol s, Rowable (FieldRec rs)) => Rowable (FieldRec ('(s, t) ': rs)) where
  row = rappend <$> (D.value $ (:& RNil) . Field <$> def) <*> row


instance Default (D.Value String) where
  def = unpack <$> D.text


selectList :: forall ps rs . (ParamsableWithProxy (FieldRec ps) (FieldRec ps), Rowable (FieldRec rs), RecAll ElField ps InternalLabeled, RecAll ElField rs InternalLabeled, InternalDefault (FieldRec ps), InternalDefault (FieldRec rs)) => String -> Q.Query (FieldRec ps) [FieldRec rs]
selectList table =
  let
    quote x = "\"" ++ x ++ "\""
    inLabels = labels (def' :: FieldRec ps)
    outLabels = labels (def' :: FieldRec rs)
    sql = -- FIXME: This needs to escape special SQL characters.
      "select "
        ++ intercalate "," (quote <$> outLabels)
        ++ " from "
        ++ table
        ++ (
             if null inLabels
               then ""
               else (" where " ++)
                      . intercalate " and "
                      $ zipWith (\l n -> l ++ "=$" ++ show n) (quote <$> inLabels) [(1 :: Int)..]
           )
    encoder = params
    decoder = D.rowsList row
  in 
    Q.statement (Data.ByteString.Char8.pack sql) encoder decoder True
