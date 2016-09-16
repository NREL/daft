{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Data.Daft.Vinyl.FieldRec.Instances (
) where


import Data.Default (Default(..))
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (ElField(..), FieldRec, )
import GHC.TypeLits (KnownSymbol, Symbol)


instance Default (FieldRec '[]) where
  def = RNil

instance (KnownSymbol s, Default t, Default (FieldRec (rs :: [(Symbol, *)]))) => Default (FieldRec ('(s, t) ': rs)) where
  def = Field def :& def
