{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


module Data.Daft.Cache (
  Cache(..)
, minimum'
, maximum'
) where


import Control.Arrow ((&&&))
import Control.Monad.Except (MonadError)
import Data.Hashable (Hashable)


class (Eq o, Hashable o, Ord k, MonadError String m) => Cache o k v m | m -> o, m -> k, m -> v where

  emptyCache :: m ()

  initializeCache :: [o] -> m ()

  eraseCache :: o -> m ()

  clearCache :: o -> m ()

  objectCount :: m Int

  objectList :: m [o]

  hasNoKeys :: o -> m Bool
  hasNoKeys = fmap null . keysList

  keysCount :: o -> m Int
  keysCount = fmap length . keysList

  keysList :: o -> m [k]

  keysMinimum :: o -> m (Maybe k)
  keysMinimum = fmap fst . keysRange

  keysMaximum :: o -> m (Maybe k)
  keysMaximum = fmap snd . keysRange

  keysRange :: o -> m (Maybe k, Maybe k)
  keysRange = fmap (\ks -> if null ks then (Nothing, Nothing) else (Just . minimum &&& Just . maximum) ks) . keysList

  lookupPoint :: (o -> k -> m (Maybe v)) -> o -> k -> m v

  lookupRange :: (o -> Maybe k -> Maybe k -> m [(k, v)]) -> o -> Maybe k -> Maybe k -> m [(k, v)]


minimum' :: Ord a => Maybe a -> Maybe a -> Maybe a
minimum' x y =
  do
    x' <- x
    y' <- y
    return $ minimum [x', y']


maximum' :: Ord a => Maybe a -> Maybe a -> Maybe a
maximum' x y =
  do
    x' <- x
    y' <- y
    return $ maximum [x', y']
