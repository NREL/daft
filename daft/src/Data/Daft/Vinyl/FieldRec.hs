{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Data.Daft.Vinyl.FieldRec (
  fieldMap'
, (<+>)
, (=:)
, (<:)
, labels
, join
-- * Internals
, Labeled
) where


import Control.Applicative (liftA2)
import Data.Daft.TypeLevel (Intersection)
import Data.Daft.Vinyl.TypeLevel (RJoin(rjoin), RUnion)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString(..))
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (ElField(..), FieldRec, )
import Data.Vinyl.Lens (type (∈), type (⊆), rget)
import Data.Vinyl.TypeLevel (type (++))
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Data.Vinyl.Core as V ((<+>))
import qualified Data.Vinyl.Derived as V ((=:), getField)


-- Map a field to a new symbol and type.
fieldMap' :: KnownSymbol t => (a -> b) -> ElField '(s, a) -> ElField '(t, b)
fieldMap' f (Field x) = Field (f x)


-- Combine records.
(<+>) :: Rec f as -> Rec f bs -> Rec f (as ++ bs)
(<+>) = (V.<+>)
infixl 1 <+>

-- Set a field's data in a record.
(=:) :: KnownSymbol s => proxy '(s,a) -> a -> FieldRec '[ '(s,a) ]
(=:) = (V.=:)
infixl 2 =:


-- Extract a field's data from a record.
(<:) :: ('(s, t) ∈ rs) => sing '(s, t) -> FieldRec rs -> t
(<:) p r = V.getField (rget p r)
infixl 9 <:


-- Extract the labels from a record.
class Labeled a where
  labels :: IsString s => proxy a -> [s]

instance Labeled (FieldRec '[]) where
  labels _ = []

instance (KnownSymbol s, Labeled (FieldRec rs)) => Labeled (FieldRec ('(s, t) ': rs)) where
  labels _ = fromString (symbolVal (Proxy :: Proxy s)) : labels (Proxy :: Proxy (FieldRec rs))


join :: forall as bs cs
     .  (Eq (FieldRec (Intersection as bs)), Intersection as bs ⊆ as, Intersection as bs ⊆ bs, RUnion as bs cs)
     => [FieldRec as] -> [FieldRec bs] -> [FieldRec cs]
-- FIXME: We could simplify the constaints if the type system could make inteferences about the relationships between unions, intersections, and subsets.
join = (catMaybes .) . liftA2 rjoin -- FIXME: This is an O(n^2) algorithm!  Would it be worthwhile to use 'Data.MultiMap' to organize intermediate computations?
