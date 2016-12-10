{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}


module Data.Daft.Vinyl.TypeLevel (
  RElem(..)
, RNonElem(..)
, RUnique(..)
, RDistinct(..)
, RNub(..)
, RUnion(..)
, RIntersection(..)
, RDifference(..)
, RJoin(..)
, NameNotIn
, UniqueNames
)  where 


import Control.Monad (guard)
import Data.Daft.TypeLevel (Difference, Elem, Equal, Intersection, Nub, Reverse, Union, Unique)
import Data.Vinyl.Core (Rec, (<+>))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆), rcast)
import Data.Vinyl.TypeLevel (type (++))


class RElem a bs where
  relem :: f a -> Rec f bs -> ()
  relem = const $ const ()

instance Elem r rs ~ 'True => RElem r rs


class RNonElem a bs where
  rnonElem :: f a -> Rec f bs -> ()
  rnonElem = const $ const ()

instance  Elem r rs ~ 'False => RNonElem r rs


class RUnique as where
  runique :: Rec f as -> ()
  runique = const ()

instance Unique as ~ 'True => RUnique as


class RDistinct as bs where
  rdistinct :: Rec f as -> Rec f bs -> ()
  rdistinct = const $ const ()

instance Intersection as bs ~ '[] => RDistinct as bs


class RNub as bs where
  rnub :: Rec f as -> Rec f bs

instance (Reverse (Nub (Reverse as)) ~ bs, bs ⊆ as) => RNub as bs where
  rnub = rcast


class RUnion as bs cs where
  runion :: Rec f as -> Rec f bs -> Rec f cs

instance (Union (Reverse bs) as ~ cs, cs ⊆ (as ++ bs)) => RUnion as bs cs where
  runion xs ys = rcast $ xs <+> ys


class RIntersection as bs cs where
 rintersection :: Rec f as -> Rec f bs -> Rec f cs

instance (Intersection as bs ~ cs, cs ⊆ as) => RIntersection as bs cs where
  rintersection xs _ = rcast xs


class RDifference as bs cs where
 rdifference :: Rec f as -> Rec f bs -> Rec f cs

instance (Difference as bs ~ cs, cs ⊆ bs) => RDifference as bs cs where
  rdifference _ = rcast


class RJoin as bs cs where
  rjoin :: FieldRec as -> FieldRec bs -> Maybe (FieldRec cs)
-- FIXME: GHC will not accept 'Rec f' instead of 'FieldRec' because it cannot infer enough about 'f'.

instance (Eq (FieldRec (Intersection as bs)), Intersection as bs ⊆ as, Intersection as bs ⊆ bs, RUnion as bs cs) => RJoin as bs cs where
  rjoin xs ys =
    do
      guard
        $ rcast xs == (rcast ys :: FieldRec (Intersection as bs))
      return
        $ runion xs ys


class NameNotIn n rs

instance NameNotIn '(s, t) '[]

instance (NameNotIn '(s, t) rs, Equal s w ~ 'False) => NameNotIn '(s, t) ('(w, u) ': rs)


class UniqueNames rs

instance (NameNotIn '(s, t) rs, UniqueNames rs) => UniqueNames ('(s, t) ': rs)
