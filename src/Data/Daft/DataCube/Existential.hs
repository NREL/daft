{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}


module Data.Daft.DataCube.Existential (
-- * Types
  ExistentialCube(..)
) where


import Data.Daft.DataCube (DataCube(..))
import Data.Daft.DataCube.Function (joinAny)


data ExistentialCube ks vs = forall cube . DataCube cube => ExistentialCube (cube ks vs)

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
  joinSelf f g (ExistentialCube c1) (ExistentialCube c2) = ExistentialCube $ joinAny f g c1 c2 -- FIXME: Danger--optimizations are lost here because it uses `joinAny` instead of `join`.  Maybe we could use a view pattern here?
