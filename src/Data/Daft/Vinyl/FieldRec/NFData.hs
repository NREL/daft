{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Data.Daft.Vinyl.FieldRec.NFData (
) where


import Control.DeepSeq (NFData(..))
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (ElField(..), FieldRec)


instance NFData (FieldRec '[]) where
  rnf _ = ()

instance (NFData t, NFData (FieldRec rs)) => NFData (FieldRec ('(s, t) ': rs)) where
  rnf (Field x :& xs) = rnf x `seq` rnf xs
