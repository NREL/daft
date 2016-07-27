{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}


module Data.Daft.Vinyl.TypeLevel (
  Equal
, Elem
, RElem(..)
, RNonElem(..)
, Unique
, RUnique(..)
, RDistinct(..)
, Equiv
, RNub(..)
, Union
, RUnion(..)
, Intersection
, RIntersection(..)
, Difference
, RDifference(..)
, NameNotIn
, UniqueNames
)  where 


import Data.Type.Bool (If, Not, type (&&))
import Data.Type.List (Difference, Find, Remove, Union)
import Data.Vinyl.Core (Rec, (<+>))
import Data.Vinyl.Lens (RSubset, rcast)
import Data.Vinyl.TypeLevel (RImage, type (++))


type family Equal (a :: k) (b :: k) :: Bool where
  Equal a a = 'True
  Equal a b = 'False


type family Elem a bs :: Bool where
  Elem a '[]       = 'False
  Elem a (a ': bs) = 'True
  Elem a (b ': bs) = Elem a bs

class RElem a bs where
  relem :: f a -> Rec f bs -> ()
  relem = const $ const ()

instance Elem r rs ~ 'True => RElem r rs

-- The following correctly type check.

testElem1 :: f A -> Rec f '[A] -> ()
testElem1 = relem

-- The following correctly do not type check.
{-
testElem2 :: f B -> Rec f '[] -> ()
testElem2 = relem

testElem3 :: f B -> Rec f '[A] -> ()
testElem3 = relem
-}


class RNonElem a bs where
  rnonElem :: f a -> Rec f bs -> ()
  rnonElem = const $ const ()

instance  Elem r rs ~ 'False => RNonElem r rs

-- The following correctly type check.

testNonElem1 :: f A -> Rec f '[] -> ()
testNonElem1 = rnonElem

testNonElem2 :: f A -> Rec f '[B] -> ()
testNonElem2 = rnonElem

-- The following correctly do not type check.
{-
testNonElem3 :: f A -> Rec f '[A] -> ()
testNonElem3 = rnonElem
-}


type family Unique as :: Bool where
  Unique '[] = 'True
  Unique (a ': as) = Not (Elem a as) && Unique as

class RUnique as where
  runique :: Rec f as -> ()
  runique = const ()

instance Unique as ~ 'True => RUnique as

-- The following correctly type check.

testUnique1 :: Rec f '[] -> ()
testUnique1 = runique

testUnique2 :: Rec f '[A] -> ()
testUnique2 = runique

testUnique3 :: Rec f '[A,B] -> ()
testUnique3 = runique

-- The following correctly do not type check.
{-
testUnique4 :: Rec f '[A,A] -> ()
testUnique4 = runique
-}


class RDistinct as bs where
  rdistinct :: Rec f as -> Rec f bs -> ()
  rdistinct = const $ const ()

instance Intersection as bs ~ '[] => RDistinct as bs

-- The following correctly type check.

testDistinct1 :: Rec f '[] -> Rec f '[A] -> ()
testDistinct1 = rdistinct

testDistinct2 :: Rec f '[A] -> Rec f '[] -> ()
testDistinct2 = rdistinct

testDistinct3 :: Rec f '[B] -> Rec f '[A] -> ()
testDistinct3 = rdistinct

testDistinct4 :: Rec f '[A, B] -> Rec f '[C] -> ()
testDistinct4 = rdistinct

-- The following correctly do not type check.
{-
testDistinct6 :: Rec f '[A] -> Rec f '[A] -> ()
testDistinct6 = rdistinct

testDistinct7 :: Rec f '[A, B] -> Rec f '[B, C, A] -> ()
testDistinct7 = rdistinct

testDistinct8 :: Rec f '[A, B] -> Rec f '[C, A] -> ()
testDistinct8 = rdistinct
-}


type family Equiv as bs where
  Equiv '[]       '[] = 'True
  Equiv '[]       bs  = 'False
  Equiv (a ': as) bs  = If (Elem a bs) (Equiv as (Remove a bs)) 'False


type family Nub as where
  Nub '[]       = '[]
  Nub (a ': as) = If (Elem a as) as (a ': Nub as)

class RNub as bs where
  rnub :: Rec f as -> Rec f bs

instance (Equiv (Nub as) bs ~ 'True, RSubset bs as (RImage bs as)) => RNub as bs where
  rnub = rcast

-- The following correctly type check.

testNub1 :: Rec f '[] -> Rec f '[]
testNub1 = rnub

testNub2 :: Rec f '[A] -> Rec f '[A]
testNub2 = rnub

testNub3 :: Rec f '[A, B] -> Rec f '[A, B]
testNub3 = rnub

testNub4 :: Rec f '[A, A, B] -> Rec f '[A, B]
testNub4 = rnub

testNub5 :: Rec f '[A, A, B] -> Rec f '[A, B]
testNub5 = rnub

testNub6 :: Rec f '[A, B, A] -> Rec f '[B, A]
testNub6 = rnub

testNub7 :: Rec f '[A, B, A] -> Rec f '[A, B]
testNub7 = rnub

-- The following correctly do not type check.
{-
testNub8 :: Rec f '[] -> Rec f '[A]
testNub8 = rnub

testNub9 :: Rec f '[B] -> Rec f '[A]
testNub9 = rnub

testNub10 :: Rec f '[B, A, B] -> Rec f '[B]
testNub10 = rnub
-}


class RUnion as bs cs where
  runion :: Rec f as -> Rec f bs -> Rec f cs

instance (Equiv (Union as bs) cs ~ 'True, RSubset cs (as ++ bs) (RImage cs (as ++ bs))) => RUnion as bs cs where
  runion xs ys = rcast $ xs <+> ys

-- The following correctly type check.

testUnion1 :: Rec f '[] -> Rec f '[] -> Rec f '[]
testUnion1 = runion

testUnion2 :: Rec f '[A] -> Rec f '[] -> Rec f '[A]
testUnion2 = runion

testUnion3 :: Rec f '[] -> Rec f '[A] -> Rec f '[A]
testUnion3 = runion

testUnion4 :: Rec f '[A] -> Rec f '[A] -> Rec f '[A]
testUnion4 = runion

testUnion5 :: Rec f '[A] -> Rec f '[B] -> Rec f '[A, B]
testUnion5 = runion

testUnion6 :: Rec f '[A] -> Rec f '[B] -> Rec f '[B, A]
testUnion6 = runion

testUnion7 :: Rec f '[A, B] -> Rec f '[B] -> Rec f '[B, A]
testUnion7 = runion

testUnion8 :: Rec f '[A, B] -> Rec f '[C] -> Rec f '[B, A, C]
testUnion8 = runion

-- The following correctly do not type check.
{-
testUnion9 :: Rec f '[A, B] -> Rec f '[C] -> Rec f '[A, B]
testUnion9 = runion

testUnion10 :: Rec f '[A, B] -> Rec f '[A, C] -> Rec f '[A, B]
testUnion10 = runion

testUnion11 :: Rec f '[B] -> Rec f '[A, C] -> Rec f '[A, B]
testUnion11 = runion

testUnion12 :: Rec f '[A, B] -> Rec f '[B] -> Rec f '[A, B, B]
testUnion12 = runion
-}


type family Intersection xs ys where
  Intersection '[]       ys        = '[] 
  Intersection (x ': xs) (x ': ys) = x ': Intersection xs ys 
  Intersection (x ': xs) (y ': ys) = If (Find x ys) (x ': Intersection xs (y ': ys)) (Intersection xs (y ': ys))
  -- FIXME: The following equation is missing from type-list-0.5.0.0!
  Intersection xs        '[]       = '[]

class RIntersection as bs cs where
 rintersection :: Rec f as -> Rec f bs -> Rec f cs

instance (Equiv (Intersection as bs) cs ~ 'True, RSubset cs as (RImage cs as)) => RIntersection as bs cs where
  rintersection xs _ = rcast xs

-- The following correctly type check.

testIntersection1 :: Rec f '[] -> Rec f '[] -> Rec f '[]
testIntersection1 = rintersection

testIntersection2 :: Rec f '[A] -> Rec f '[] -> Rec f '[]
testIntersection2 = rintersection

testIntersection3 :: Rec f '[] -> Rec f '[A] -> Rec f '[]
testIntersection3 = rintersection

testIntersection4 :: Rec f '[A] -> Rec f '[A] -> Rec f '[A]
testIntersection4 = rintersection

testIntersection5 :: Rec f '[A] -> Rec f '[B] -> Rec f '[]
testIntersection5 = rintersection

testIntersection6 :: Rec f '[A, C] -> Rec f '[B, C] -> Rec f '[C]
testIntersection6 = rintersection

testIntersection7 :: Rec f '[A, B] -> Rec f '[B, A] -> Rec f '[B, A]
testIntersection7 = rintersection

testIntersection8 :: Rec f '[A, B] -> Rec f '[C, B, A] -> Rec f '[B, A]
testIntersection8 = rintersection

-- The following correctly do not type check.
{-
testIntersection9 :: Rec f '[A, B] -> Rec f '[C] -> Rec f '[A, B]
testIntersection9 = rintersection

testIntersection10 :: Rec f '[A, B] -> Rec f '[A, C] -> Rec f '[A, B]
testIntersection10 = rintersection

testIntersection11 :: Rec f '[B] -> Rec f '[A, C] -> Rec f '[A, B]
testIntersection11 = rintersection

testIntersection12 :: Rec f '[A, B] -> Rec f '[B] -> Rec f '[A, B, B]
testIntersection12 = rintersection
-}


class RDifference as bs cs where
 rdifference :: Rec f as -> Rec f bs -> Rec f cs

instance (Equiv(Difference as bs) cs ~ 'True, RSubset cs bs (RImage cs bs)) => RDifference as bs cs where
  rdifference _ = rcast

-- The following correctly type check.

testDifference1 :: Rec f '[] -> Rec f '[] -> Rec f '[]
testDifference1 = rdifference

testDifference2 :: Rec f '[A] -> Rec f '[] -> Rec f '[]
testDifference2 = rdifference

testDifference3 :: Rec f '[] -> Rec f '[A] -> Rec f '[A]
testDifference3 = rdifference

testDifference4 :: Rec f '[A] -> Rec f '[A] -> Rec f '[]
testDifference4 = rdifference

testDifference5 :: Rec f '[A] -> Rec f '[B, A] -> Rec f '[B]
testDifference5 = rdifference

-- The following correctly do not type check.
{-
testDifference6 :: Rec f '[B] -> Rec f '[] -> Rec f '[B]
testDifference6 = rdifference

testDifference7 :: Rec f '[B] -> Rec f '[C] -> Rec f '[B]
testDifference7 = rdifference
-}


type A = '("A", Double)
type B = '("B", Double)
type C = '("C", Double)


class NameNotIn n rs

instance NameNotIn '(s, t) '[]

instance (NameNotIn '(s, t) rs, Equal s w ~ 'False) => NameNotIn '(s, t) ('(w, u) ': rs)


class UniqueNames rs

instance (NameNotIn '(s, t) rs, UniqueNames rs) => UniqueNames ('(s, t) ': rs)
