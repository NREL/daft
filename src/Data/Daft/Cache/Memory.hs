{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}


module Data.Daft.Cache.Memory (
  Container
, emptyContainer
, MemoryCacheT(..)
, runCacheT
, evalCacheT
, execCacheT
) where


import Control.Applicative ((<|>))
import Control.Monad (guard, unless)
import Control.Monad.Except (MonadError(..), MonadIO)
import Control.Monad.State (MonadState(..), StateT, runStateT, modify')
import Data.Daft.Cache (Cache(..), maximum', minimum')
import Data.Hashable (Hashable)
import Data.Maybe (isNothing)

import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M


type Container o k v = H.HashMap o (Maybe k, Maybe k, M.Map k v)


emptyContainer :: (Eq o, Hashable o) => [o] -> Container o k v
emptyContainer = ($ repeat (Nothing, Nothing, M.empty)) . (H.fromList .) . zip


runCacheT :: MemoryCacheT o k v m a -> Container o k v -> m (a, Container o k v)
runCacheT = runStateT . runMemoryCacheT


evalCacheT :: Monad m => MemoryCacheT o k v m a -> Container o k v -> m a
evalCacheT = (fmap fst .) . runCacheT


execCacheT :: Monad m => MemoryCacheT o k v m a ->  Container o k v -> m (Container o k v)
execCacheT = (fmap snd .) . runCacheT


newtype MemoryCacheT o k v m a = MemoryCacheT {runMemoryCacheT :: StateT (Container o k v) m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadState (Container o k v))

deriving instance MonadError String m => MonadError String (MemoryCacheT o k v m)

instance (Eq o, Hashable o, Ord k, MonadError String m) => Cache o k v (MemoryCacheT o k v m) where

  emptyCache = put H.empty

  initializeCache = ($ repeat (Nothing, Nothing, M.empty)) . ((put . H.fromList) .) . zip

  eraseCache = modify' . H.delete

  clearCache = modify' . flip H.insert (Nothing, Nothing, M.empty)

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
        Nothing            -> return True
        Just (_, _, inner) -> return $ M.null inner

  keysCount object =
    do
      container <- get
      case H.lookup object container of
        Nothing            -> return 0
        Just (_, _, inner) -> return $ M.size inner

  keysList object =
    do
      container <- get
      case H.lookup object container of
        Nothing            -> return []
        Just (_, _, inner) -> return $ M.keys inner

  keysRange object =
    do
      container <- get
      case H.lookup object container of
        Nothing        -> return (Nothing, Nothing)
        Just (x, y, _) -> return (x, y)

  lookupPoint f object key =
    do
      value' <- maybe (throwError "MemoryCache: evalution failed.") return <$> f object key
      container <- get
      case H.lookup object container of
        Nothing            -> do
                                value <- value'
                                put $ H.insert object (Nothing, Nothing, M.singleton key value) container
                                return value
        Just (_, _, inner) -> case M.lookup key inner of
                                Nothing    -> do
                                                value <- value'
                                                put $ H.insert object (Nothing, Nothing, M.insert key value inner) container
                                                return value
                                Just value -> return value

  lookupRange f object lower upper =
    do
      container <- get
      let
        putNew =
          do
            values <- f object lower upper
            put
              $ H.insert object
                (
                  lower <|> (guard (not $ null values) >> (Just . minimum $ fst <$> values))
                , upper <|> (guard (not $ null values) >> (Just . maximum $ fst <$> values))
                , M.fromList values
                ) container
            return values
      case H.lookup object container of
        Nothing                      -> putNew
        Just (lower', upper', inner) -> if isNothing lower' || isNothing upper'
                                          then putNew
                                          else do
                                                 let
                                                   (lower'', upper'') = (minimum' lower lower', maximum' upper upper')
                                                 innerLower <-
                                                   if isNothing lower || lower' /= lower''
                                                     then f object lower'' lower'
                                                     else return []
                                                 innerUpper <-
                                                   if isNothing upper || upper' /= upper''
                                                     then f object upper' upper''
                                                     else return []
                                                 let
                                                   inner' = M.unions [inner, M.fromList innerLower, M.fromList innerUpper]
                                                 unless (null innerLower && null innerUpper)
                                                   . put
                                                   $ H.insert object
                                                     (
                                                       lower'' <|> Just (fst $ M.findMin inner')
                                                     , upper'' <|> Just (fst $ M.findMax inner')
                                                     , inner'
                                                     ) container
                                                 return $ subrange lower upper inner'


filterBelow :: Ord k => k -> M.Map k v -> M.Map k v
filterBelow key container =
  let
    (lower, center, _) = M.splitLookup key container
  in
    maybe id (M.insert key) center lower


filterAbove :: Ord k => k -> M.Map k v -> M.Map k v
filterAbove key container =
  let
    (_, center, upper) = M.splitLookup key container
  in
    maybe id (M.insert key) center upper


subrange :: Ord k => Maybe k -> Maybe k -> M.Map k v -> [(k, v)]
subrange Nothing      Nothing      = M.toList                                      
subrange Nothing      (Just upper) = M.toList .                     filterBelow upper
subrange (Just lower) Nothing      = M.toList . filterAbove lower                   
subrange (Just lower) (Just upper) = M.toList . filterAbove lower . filterBelow upper
