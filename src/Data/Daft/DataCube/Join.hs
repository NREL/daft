{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


module Data.Daft.DataCube.Join (
-- * Types
  Join
, Joinable(..)
-- * Joins
, semijoin
, antijoin
) where


import Data.Daft.DataCube (DataCube(..), Joiner(..))
import Data.Daft.DataCube.Function (FunctionCube, joinAny)
import Data.Proxy (Proxy(..))



type family Join c1 c2 :: * -> * -> * where
  Join a a = a
  Join a b = FunctionCube


data JoinSelf
data JoinAny

type family JoinStyle (c1 :: * -> * -> *) (c2 :: * -> * -> *) where
  JoinStyle a a = JoinSelf
  JoinStyle a b = JoinAny


class Joinable c1 c2 where
  join :: (Key c1 k1, Key c2 k2, Key (Join c1 c2) k3) => Joiner k1 k2 k3 -> (v1 -> v2 -> v3) -> c1 k1 v1 -> c2 k2 v2 -> (Join c1 c2) k3 v3

instance (JoinStyle c1 c2 ~ flag, Joinable' flag c1 c2 (Join c1 c2)) => Joinable c1 c2 where
  join = join' (Proxy :: Proxy flag)


data Dummy1 k v
data Dummy2 k v

instance Joinable Dummy1 Dummy2 where
  join = undefined


class Joinable' flag c1 c2 c3 where
  join' :: (Key c1 k1, Key c2 k2, Key c3 k3) => Proxy flag -> Joiner k1 k2 k3 -> (v1 -> v2 -> v3) -> c1 k1 v1 -> c2 k2 v2 -> c3 k3 v3

instance Joinable' JoinSelf c1 c1 c1 where
  join' _ = joinSelf

instance Joinable' JoinAny c1 c2 FunctionCube where
  join' _ = joinAny


semijoin :: (Key cube2 k2, DataCube cube1, DataCube cube2) => Joiner k1 k2 k1 -> cube1 k1 v1 -> cube2 k2 v2 -> cube1 k1 v1
semijoin Joiner{..} = flip (selectKeys . (. castRight) . evaluable)


antijoin :: (Key cube2 k2, DataCube cube1, DataCube cube2) => Joiner k1 k2 k1 -> cube1 k1 v1 -> cube2 k2 v2 -> cube1 k1 v1
antijoin Joiner{..}= flip (selectKeys . (. castRight) . (not .) . evaluable)
