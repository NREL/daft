{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


module Data.Daft.Cache (
  Cache(..)
) where


import Control.Monad.Except (MonadError)
import Data.Hashable (Hashable)


class (Eq o, Hashable o, Ord k, MonadError String m) => Cache o k v m where
  emptyCache :: m ()
  initializeCache :: [o] -> m ()
  eraseCache :: o -> m ()
  clearCache :: o -> m ()
  lookupPoint :: (o -> k -> Maybe v) -> o -> k -> m v
  lookupRange :: (o -> Maybe k -> Maybe k -> [(k, v)]) -> o -> Maybe k -> Maybe k -> m [(k, v)]
