{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


module Data.Daft.Cache (
  Cache(..)
, minimum'
, maximum'
) where


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
  keysCount :: o -> m Int
  keysList :: o -> m [k]
  keysMinimum :: o -> m (Maybe k)
  keysMaximum :: o -> m (Maybe k)
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
