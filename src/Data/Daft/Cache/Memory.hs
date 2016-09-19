{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}


module Data.Daft.Cache.Memory (
  Container
, MemoryCacheT(..)
, execMemoryCache
) where


import Control.Arrow ((&&&))
import Control.Monad.Except (ExceptT, MonadError(..), MonadIO, runExceptT)
import Control.Monad.State (MonadState(..), StateT, runStateT, modify')
import Data.Daft.Cache (Cache(..), maximum', minimum')
import Data.Hashable (Hashable)

import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M


execMemoryCache :: Monad m => MemoryCacheT o k v m a -> Container o k v -> m (Either String (Container o k v))
execMemoryCache cache initial =
  do
    (x, cache') <- runStateT (runExceptT (runCacheT cache)) initial
    return $ const cache' <$> x


type Container o k v = H.HashMap o (M.Map k v)


newtype MemoryCacheT o k v m a = MemoryCacheM {runCacheT :: ExceptT String (StateT (Container o k v) m) a}
  deriving (Applicative, Functor, Monad, MonadError String, MonadIO, MonadState (Container o k v))

instance (Eq o, Hashable o, Ord k, Monad m) => Cache o k v (MemoryCacheT o k v m) where

  emptyCache = put H.empty

  initializeCache = ($ repeat M.empty) . ((put . H.fromList) .) . zip

  eraseCache = modify' . H.delete

  clearCache = modify' . flip H.insert M.empty

  objectCount =
    do
      container <- get
      return $ H.size container

  objectList =
    do
      container <- get
      return $ H.keys container

  hasNoKeys object =
    do
      container <- get
      case H.lookup object container of
        Nothing    -> return True
        Just inner -> return $ M.null inner

  keysCount object =
    do
      container <- get
      case H.lookup object container of
        Nothing    -> return 0
        Just inner -> return $ M.size inner

  keysList object =
    do
      container <- get
      case H.lookup object container of
        Nothing    -> return []
        Just inner -> return $ M.keys inner

  keysMinimum object =
    do
      container <- get
      case H.lookup object container of
        Nothing    -> return Nothing
        Just inner -> if M.null inner
                        then return Nothing
                        else return . Just . fst $ M.findMin inner

  keysMaximum object =
    do
      container <- get
      case H.lookup object container of
        Nothing    -> return Nothing
        Just inner -> if M.null inner
                        then return Nothing
                        else return . Just . fst $ M.findMax inner

  lookupPoint f object key =
    do
      value' <- maybe (throwError "MemoryCache: evalution failed.") return <$> f object key
      container <- get
      case H.lookup object container of
        Nothing    -> do
                        value <- value'
                        put $ H.insert object (M.singleton key value) container
                        return value
        Just inner -> case M.lookup key inner of
                        Nothing    -> do
                                        value <- value'
                                        put $ H.insert object (M.insert key value inner) container
                                        return value
                        Just value -> return value

  lookupRange f object lower upper =
    do
      container <- get
      case H.lookup object container of
        Nothing    -> do
                        values <- f object lower upper
                        put $ H.insert object (M.fromList values) container
                        return values
        Just inner -> if M.null inner
                        then do
                               values <- f object lower upper
                               put $ H.insert object (M.fromList values) container
                               return values
                        else do
                               let
                                 (lower', upper') = (Just . fst . M.findMin &&& Just . fst . M.findMax) inner
                                 (lower'', upper'') = (minimum' lower lower', maximum' upper upper')
                               innerLower <-
                                 if lower' == lower''
                                   then return []
                                   else f object lower'' lower'
                               innerUpper <-
                                 if upper' == upper''
                                   then return []
                                   else f object upper' upper''
                               let
                                 inner' = M.unions [inner, M.fromList innerLower, M.fromList innerUpper]
                               put $ H.insert object inner' container
                               return $ subrange lower upper inner'


splitLower :: Ord k => k -> M.Map k v -> M.Map k v
splitLower key container =
  let
    (lower, center, _) = M.splitLookup key container
  in
    maybe id (M.insert key) center lower


splitUpper :: Ord k => k -> M.Map k v -> M.Map k v
splitUpper key container =
  let
    (_, center, upper) = M.splitLookup key container
  in
    maybe id (M.insert key) center upper


subrange :: Ord k => Maybe k -> Maybe k -> M.Map k v -> [(k, v)]
subrange Nothing      Nothing      = M.toList                                      
subrange Nothing      (Just upper) = M.toList .                    splitUpper upper
subrange (Just lower) Nothing      = M.toList . splitLower lower                   
subrange (Just lower) (Just upper) = M.toList . splitLower lower . splitUpper upper
