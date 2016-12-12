{-# LANGUAGE ExistentialQuantification #-}


module Data.Daft.DataCube.Existential (
-- * Types
  ExistentialCube(..)
) where


import Data.Daft.DataCube (DataCube(..))
import Data.Daft.DataCube.Function (joinAny)
import Data.Typeable (Typeable, cast, typeOf)


data ExistentialCube ks vs = forall cube . (Typeable cube, DataCube cube) => ExistentialCube (cube ks vs)


instance DataCube ExistentialCube where

  evaluate (ExistentialCube c) = evaluate c

  selectWithKey f (ExistentialCube c) = ExistentialCube $ selectWithKey f c

  selectRange k1 k2 (ExistentialCube c) = ExistentialCube $ selectRange k1 k2 c

  knownKeys (ExistentialCube c) = knownKeys c

  selectKnownMinimum (ExistentialCube c) = selectKnownMinimum c

  selectKnownMaximum (ExistentialCube c) = selectKnownMaximum c

  projectWithKey f (ExistentialCube c) = ExistentialCube $ projectWithKey f c

  projectKnownKeys f (ExistentialCube c) = projectKnownKeys f c

  rekey f (ExistentialCube c) = ExistentialCube $ rekey f c

  aggregateWithKey f g (ExistentialCube c) = ExistentialCube $ aggregateWithKey f g c

  disaggregateWithKey f g (ExistentialCube c) = ExistentialCube $ disaggregateWithKey f g c

{-
  joinSelf f g (ExistentialCube c1) (ExistentialCube c2) =
    ExistentialCube
      $ joinAny f g c1 c2 -- FIXME: In order to maintain optimizations, we need 'join' here instead of 'joinAny'.
                          -- It seems that 'Data.Typeable.cast' and 'Data.Typeable.eqT' could be used to do this.
-}
  joinSelf f g (ExistentialCube c1) (ExistentialCube c2) =
    ExistentialCube
      $ if typeOf c1 == typeOf c2 -- FIXME: This won't work, since we want to test the type at * -> * -> *, not just *.
          then let
                 Just c1' = cast c1
                 Just c2' = cast c2
               in
                 joinSelf f g c1' c2'
          else joinAny f g c1 c2
