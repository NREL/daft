{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}


module Data.Daft.Vinyl.Join (
  naturalJoin
, crossJoin
) where 


import Control.Monad (guard)
import Data.Daft.Vinyl.TypeLevel (RDistinct, RIntersection(..), RUnion(..))
import Data.Proxy (Proxy)
import Data.Vinyl.Core (Rec)
import Data.Vinyl.Lens (type (⊆), rcast)


naturalJoin :: forall ks as bs cs f proxy
            .  (Eq (Rec f ks), ks ⊆ as, ks ⊆ bs, RUnion as bs cs, RIntersection as bs ks)
            => proxy ks -> Rec f as -> Rec f bs -> Maybe (Rec f cs)
-- FIXME: I think it is not possible to write a natural join without having a proxy argument that gives the hint as to the time of the intersection.  One hope is that this needed constraint could be forced to be reified by a function similar in spirit to 'reifyConstraint'.
naturalJoin _ x y =
  do
    guard
      $ rcast x == (rcast y :: Rec f ks)
    return
      $ runion x y

-- FIXME: The following should type check, but they do not!

--testNaturalJoin1 :: Eq (Rec f '[]) => Proxy '[] -> Rec f '[] -> Rec f '[] -> Maybe (Rec f '[])
--testNaturalJoin1 = naturalJoin

--testNaturalJoin2 :: Eq (Rec f '[]) => Proxy '[] -> Rec f '[A] -> Rec f '[B] -> Maybe (Rec f '[A, B])
--testNaturalJoin2 = naturalJoin

-- The following correctly type check.

testNaturalJoin4 :: Eq (Rec f '[A]) => Proxy '[A] -> Rec f '[A] -> Rec f '[A] -> Maybe (Rec f '[A])
testNaturalJoin4 = naturalJoin

testNaturalJoin5 :: Eq (Rec f '[A]) => Proxy '[A] -> Rec f '[A, B] -> Rec f '[C, A] -> Maybe (Rec f '[A, B, C])
testNaturalJoin5 = naturalJoin

testNaturalJoin6 :: Eq (Rec f '[A, B]) => Proxy '[A, B] -> Rec f '[A, B] -> Rec f '[B, C, A] -> Maybe (Rec f '[A, B, C])
testNaturalJoin6 = naturalJoin

-- The following correctly do not type check.

--testNaturalJoin6 :: Eq (Rec f '[A]) => Proxy '[A] -> Rec f '[A, B] -> Rec f '[A, B] -> Maybe (Rec f '[A, B])
--testNaturalJoin6 = naturalJoin

--testNaturalJoin8 :: Eq (Rec f '[A]) => Proxy '[A] -> Rec f '[A] -> Rec f '[A] -> Maybe (Rec f '[A, A])
--testNaturalJoin8 = naturalJoin

--testNaturalJoin9 :: Eq (Rec f '[A, B]) => Proxy '[A, B] -> Rec f '[A, B] -> Rec f '[C, A] -> Maybe (Rec f '[A, B, C])
--testNaturalJoin9 = naturalJoin


crossJoin :: (RUnion as bs cs, RDistinct as bs)
          => Rec f as -> Rec f bs -> Rec f cs
crossJoin = runion

-- The following correctly type check.

testCrossJoin1 :: Rec f '[] -> Rec f '[] -> Rec f '[]
testCrossJoin1 = crossJoin

testCrossJoin2 :: Rec f '[A] -> Rec f '[] -> Rec f '[A]
testCrossJoin2 = crossJoin

testCrossJoin3 :: Rec f '[A] -> Rec f '[B] -> Rec f '[A, B]
testCrossJoin3 = crossJoin

-- The following correctly do not type check.

--testCrossJoin4 :: Rec f '[A] -> Rec f '[A] -> Rec f '[A]
--testCrossJoin4 = crossJoin

--testCrossJoin5 :: Rec f '[B, A] -> Rec f '[A, C] -> Rec f '[A, B, C]
--testCrossJoin5 = crossJoin


type A = '("A", Double)
type B = '("B", Double)
type C = '("C", Double)
