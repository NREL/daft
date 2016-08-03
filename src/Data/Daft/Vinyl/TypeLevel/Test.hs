{-# LANGUAGE DataKinds                 #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}


module Data.Daft.Vinyl.TypeLevel.Test (
)  where 


import Data.Daft.Vinyl.TypeLevel (RElem(..) , RNonElem(..) , RUnique(..) , RDistinct(..) , RNub(..) , RUnion(..) , RIntersection(..) , RDifference(..))
import Data.Vinyl.Core (Rec)


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
