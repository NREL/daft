{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}


module Data.Daft.DataCube.Existential (
-- * Types
  ExistentialCube(..)
) where


import Control.Applicative ((<|>))
import Data.Daft.DataCube (DataCube(..))
import Data.Daft.DataCube.Function (FunctionCube(..), joinAny)
import Data.Daft.DataCube.Table (TableCube)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Typeable (Typeable, gcast2, typeOf2)


data ExistentialCube k v = forall cube . (Typeable cube, DataCube cube, Key cube ~ Ord, Keys cube ~ Set) => ExistentialCube (cube k v) -- FIXME: TInstead of requiring 'Ord', could the constraint be a parameter?


instance DataCube ExistentialCube k where

--type Key k

  type Key ExistentialCube = Ord

  type Keys ExistentialCube = Set

  cmap = fmap

  cempty = mempty

  cappend = mappend

  evaluate (ExistentialCube c) = evaluate c

  selectWithKey f (ExistentialCube c) = ExistentialCube $ selectWithKey f c

  selectRange k1 k2 (ExistentialCube c) = ExistentialCube $ selectRange k1 k2 c

  knownKeys (ExistentialCube c) = knownKeys c

  knownSize (ExistentialCube c) = knownSize c

  knownEmpty (ExistentialCube c) = knownEmpty c

  selectKnownMinimum (ExistentialCube c) = selectKnownMinimum c

  selectKnownMaximum (ExistentialCube c) = selectKnownMaximum c

  projectWithKey f (ExistentialCube c) = ExistentialCube $ projectWithKey f c

  projectKnownKeys f (ExistentialCube c) = projectKnownKeys f c

  rekey f (ExistentialCube c) = ExistentialCube $ rekey f c

  aggregateWithKey f g (ExistentialCube c) = ExistentialCube $ aggregateWithKey f g c

  disaggregateWithKey f g (ExistentialCube c) = ExistentialCube $ disaggregateWithKey f g c

  joinSelf f g (ExistentialCube c1) (ExistentialCube c2) =
    if typeOf2 c1 == typeOf2 c2
      then ExistentialCube $ joinSelf f g c1 $ fromJust $ cast2 c2
      else ExistentialCube $ joinAny  f g c1                    c2


cast2 :: forall c c' k v . (Typeable c, Typeable c') => c k v -> Maybe (c' k v)
cast2 = fmap runIdentity . gcast2 . Identity


instance Functor (ExistentialCube k) where

  fmap f (ExistentialCube c) = ExistentialCube $ cmap f c

instance Key ExistentialCube k => Monoid (ExistentialCube k v) where

  mempty = ExistentialCube (cempty :: TableCube k v)

  mappend (ExistentialCube c1) (ExistentialCube c2) =
    if typeOf2 c1 == typeOf2 c2
      then ExistentialCube $ cappend c1 $ fromJust $ cast2 c2
      else ExistentialCube $ FunctionCube $ \k -> evaluate c1 k <|> evaluate c2 k
