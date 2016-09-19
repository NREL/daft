{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}


module Data.Daft.Cache.Memory (
  MemoryCacheM(..)
) where


import Control.Arrow ((&&&))
import Control.Monad.Except (ExceptT, MonadError(..))
import Control.Monad.State (MonadState(..), State, modify')
import Data.Daft.Cache (Cache(..))
import Data.Hashable (Hashable)

import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M


type Container o k v = H.HashMap o (M.Map k v)


newtype MemoryCacheM o k v a = MemoryCacheM {runMemoryCacheM :: ExceptT String (State (Container o k v)) a}
  deriving (Applicative, Functor, MonadError String, Monad, MonadState (Container o k v))


instance (Eq o, Hashable o, Ord k) => Cache o k v (MemoryCacheM o k v) where

  emptyCache = put H.empty

  initializeCache = ($ repeat M.empty) . ((put . H.fromList) .) . zip

  eraseCache = modify' . H.delete

  clearCache = modify' . flip H.insert M.empty

  lookupPoint f object key =
    do
      let
        value' = maybe (throwError "MemoryCache: evalution failed.") return $ f object key
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
                        let
                          values = f object lower upper
                        put $ H.insert object (M.fromList values) container
                        return values
        Just inner -> if M.null inner
                        then do
                               let
                                 values = f object lower upper
                               put $ H.insert object (M.fromList values) container
                               return values
                        else do
                               let
                                 (lower', upper') = (Just . fst . M.findMin &&& Just . fst . M.findMax) inner
                                 (lower'', upper'') = (minimum' lower lower', maximum' upper upper')
                                 innerLower =
                                   if lower' == lower''
                                     then []
                                     else f object lower'' lower'
                                 innerUpper =
                                   if upper' == upper''
                                     then []
                                     else f object upper' upper''
                                 inner' = M.unions [inner, M.fromList innerLower, M.fromList innerUpper]
                               put $ H.insert object inner' container
                               return $ subrange lower upper inner'


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


subrange :: Ord k => Maybe k -> Maybe k -> M.Map k v -> [(k, v)]
subrange Nothing      Nothing      = M.toList                                      
subrange Nothing      (Just upper) = M.toList .                    splitUpper upper
subrange (Just lower) Nothing      = M.toList . splitLower lower                   
subrange (Just lower) (Just upper) = M.toList . splitLower lower . splitUpper upper


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
