{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}


module Data.Daft.TypeLevel (
  Equal
, Elem
, Unique
, Equiv
, Nub
, Union
, Intersection
, Difference
)  where 


import Data.Type.Bool (If, Not, type (&&))
import Data.Type.List (Difference, Find, Remove, Union)


type family Equal (a :: k) (b :: k) :: Bool where
  Equal a a = 'True
  Equal a b = 'False


type family Elem a bs :: Bool where
  Elem a '[]       = 'False
  Elem a (a ': bs) = 'True
  Elem a (b ': bs) = Elem a bs


type family Unique as :: Bool where
  Unique '[] = 'True
  Unique (a ': as) = Not (Elem a as) && Unique as


type family Equiv as bs where
  Equiv '[]       '[] = 'True
  Equiv '[]       bs  = 'False
  Equiv (a ': as) bs  = If (Elem a bs) (Equiv as (Remove a bs)) 'False


type family Nub as where
  Nub '[]       = '[]
  Nub (a ': as) = If (Elem a as) as (a ': Nub as)


type family Intersection xs ys where
  Intersection '[]       ys        = '[] 
  Intersection (x ': xs) (x ': ys) = x ': Intersection xs ys 
  Intersection (x ': xs) (y ': ys) = If (Find x ys) (x ': Intersection xs (y ': ys)) (Intersection xs (y ': ys))
  -- FIXME: The following equation is missing from type-list-0.5.0.0!
  Intersection xs        '[]       = '[]
