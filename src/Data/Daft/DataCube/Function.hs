{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}


module Data.Daft.DataCube.Function (
-- * Types
  FunctionCube(..)
-- * Conversion
, fromFunction
-- * Joins
, joinAny
) where


import Control.Applicative ((<|>), liftA2)
import Control.DeepSeq (NFData(..))
import Control.Monad (guard)
import Data.Daft.DataCube (DataCube(..), Gregator(..), Joiner(..), Rekeyer(..))
import Data.Maybe (mapMaybe)


newtype FunctionCube k v = FunctionCube {function :: k -> Maybe v}


class Unconstrained k

instance Unconstrained k


instance DataCube FunctionCube where

  type Key FunctionCube k = Unconstrained k

  cmap = fmap

  cempty = mempty

  cappend = mappend

  evaluate = function

  knownKeys = const []

  knownSize = const 0

  knownEmpty = const False

  selectWithKey selector FunctionCube{..} =
    FunctionCube $ \k ->
      do
        v <- function k
        guard
          $ selector k v
        return v

  selectRange Nothing Nothing f = f
  selectRange (Just k0) Nothing FunctionCube{..} =
    FunctionCube $ \k ->
      do
        guard
          $ k0 < k
        function k
  selectRange Nothing (Just k1) FunctionCube{..} =
    FunctionCube $ \k ->
      do
        guard
          $ k < k1
        function k
  selectRange (Just k0) (Just k1) FunctionCube{..} =
    FunctionCube $ \k ->
      do
        guard
          $ k0 < k && k < k1
        function k

  selectKnownMinimum FunctionCube{..} = Nothing

  selectKnownMaximum FunctionCube{..} = Nothing

  projectWithKey projector FunctionCube{..} = FunctionCube $ liftA2 fmap projector function

  projectKnownKeys _         FunctionCube{..} = []

  rekey Rekeyer{..} FunctionCube{..} = FunctionCube $ function . unrekeyer

  aggregateWithKey Gregator{..} reducer FunctionCube{..} =
    FunctionCube $ \k' ->
      return
        . reducer k'
        . mapMaybe function
        $ disaggregator k'

  disaggregateWithKey Gregator{..} expander FunctionCube{..} = 
    FunctionCube $ \k' ->
      fmap (expander k')
        . function
        $ aggregator k'

  joinSelf = joinAny


instance Functor (FunctionCube k) where
  fmap f (FunctionCube g) = FunctionCube $ fmap f . g


instance Applicative (FunctionCube k) where
  pure = FunctionCube . const . return
  FunctionCube g <*> FunctionCube h =
    FunctionCube $ \k ->
      do
        f <- g k
        v <- h k
        return $ f v


instance Monoid (FunctionCube k v) where
  mempty = FunctionCube $ const Nothing
  mappend (FunctionCube f) (FunctionCube g) = FunctionCube $ \k -> f k <|> g k


instance (NFData k, NFData v) => NFData (FunctionCube k v) where
  rnf (FunctionCube f) = rnf f
 

fromFunction :: (k -> Maybe v) -> FunctionCube k v
fromFunction = FunctionCube


joinAny :: (Key cube k1, Key cube' k2, Key FunctionCube k3, DataCube cube, DataCube cube') => Joiner k1 k2 k3 -> (v1 -> v2 -> v3) -> cube k1 v1 -> cube' k2 v2 -> FunctionCube k3 v3
joinAny Joiner{..} combiner cube1 cube2 =
  FunctionCube $ \k3 ->
    do
      v1 <- cube1 `evaluate` castLeft k3
      v2 <- cube2 `evaluate` castRight k3
      return $ v1 `combiner` v2
