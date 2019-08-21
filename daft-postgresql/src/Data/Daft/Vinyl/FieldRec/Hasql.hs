{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}


module Data.Daft.Vinyl.FieldRec.Hasql (
  Paramsable(..)
, Rowable(..)
, selectList
, insertRow
-- Internal type classes
, ParamsableWithProxy
) where


import Data.Daft.Vinyl.FieldRec (Labeled(..), (<:))
import Data.Default (Default(..))
import Data.Functor.Contravariant (contramap)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Data.Vinyl.Core (Rec(..), rappend)
import Data.Vinyl.Derived (ElField(..), FieldRec)
import Data.Vinyl.Lens (type (∈))
import GHC.TypeLits (KnownSymbol)
import Hasql.Statement (Statement(..))

import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.Text as T (pack, unpack)
import qualified Hasql.Decoders as D (Row, Value, column, int8, rowsAffected, rowList, text)
import qualified Hasql.Encoders as E (Params, Value, int8, param, text, unit)


class Paramsable a where
  params :: E.Params a

instance ParamsableWithProxy (FieldRec rs) (FieldRec rs) => Paramsable (FieldRec rs) where
  params = paramsWithProxy (Proxy :: Proxy (FieldRec rs))


class ParamsableWithProxy a b where
  paramsWithProxy :: proxy a -> E.Params b

instance ParamsableWithProxy (FieldRec '[]) (FieldRec rs) where
  paramsWithProxy _ = contramap (const ()) E.unit

instance (Default (E.Value t), '(s, t) ∈ rs, ParamsableWithProxy (FieldRec rs') (FieldRec rs)) => ParamsableWithProxy (FieldRec ('(s, t) ': rs')) (FieldRec rs) where
  paramsWithProxy _ = contramap ((Proxy :: Proxy '(s, t)) <:) (E.param def) <> paramsWithProxy (Proxy :: Proxy (FieldRec rs'))


instance Default (E.Value Int) where
  def = contramap toEnum E.int8

instance Default (E.Value String) where
  def = contramap T.pack E.text


class Rowable a where
  row :: D.Row a

instance Rowable (FieldRec '[]) where
  row = pure RNil

instance (Default (D.Value t), KnownSymbol s, Rowable (FieldRec rs)) => Rowable (FieldRec ('(s, t) ': rs)) where
  row = rappend <$> D.column ((:& RNil) . Field <$> def) <*> row


instance Default (D.Value Int) where
  def = fromEnum <$> D.int8

instance Default (D.Value String) where
  def = T.unpack <$> D.text


selectList :: forall ps rs . (Paramsable (FieldRec ps), Rowable (FieldRec rs), Labeled (FieldRec ps), Labeled (FieldRec rs)) => String -> Statement (FieldRec ps) [FieldRec rs]
selectList table =
  let
    inLabels  = labels (Proxy :: Proxy (FieldRec ps))
    outLabels = labels (Proxy :: Proxy (FieldRec rs))
    sql = -- FIXME: This needs to escape special SQL characters.
      "select "
        ++ intercalate ", " (quote <$> outLabels)
        ++ " from "
        ++ table
        ++ (
             if null inLabels
               then ""
               else (" where " ++)
                      . intercalate " and "
                      $ zipWith ((. ((" = $" ++) . show)) . (++)) (quote <$> inLabels) [(1 :: Int)..]
           )
    encoder = params
    decoder = D.rowList row
  in 
    Statement (B.pack sql) encoder decoder True


insertRow :: forall ps . (Paramsable (FieldRec ps), Labeled (FieldRec ps)) => String -> Statement (FieldRec ps) Int
insertRow table =
  let
    inLabels  = labels (Proxy :: Proxy (FieldRec ps))
    sql =
      "insert into "
        ++ table
        ++ " ("
        ++ intercalate ", " (quote <$> inLabels)
        ++ ") values("
        ++ intercalate ", " (zipWith (const $ ("$" ++) . show) inLabels [(1 :: Int)..])
        ++ ")"
    encoder = params
    decoder = fromEnum <$> D.rowsAffected
  in
    Statement (B.pack sql) encoder decoder True


quote :: String -> String
quote = ("\"" ++) . (++ "\"")
