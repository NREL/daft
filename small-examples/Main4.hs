{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Proxy (Proxy(..))

class DataCube (a :: * -> * -> *) where
  joinSelf :: a k v -> a k v -> a k v

joinAny :: a k v -> b k v -> FunctionCube k v
joinAny = undefined

data TableCube k v = TableCube
instance DataCube TableCube  where
  joinSelf _ _ = TableCube

data FunctionCube k v = FunctionCube
instance DataCube FunctionCube where
  joinSelf _ _ = FunctionCube

type family Join (a :: * -> * -> *) (b :: * -> * -> *) where
  Join a a = a
  Join a b = FunctionCube

data SelfJoin
data OtherJoin
type family JoinStyle (a :: * -> * -> *) (b :: * -> * -> *)  where
  JoinStyle a a = SelfJoin
  JoinStyle a b = OtherJoin

class Joinable (a :: * -> * -> *) (b :: * -> * -> *) where
  join :: a k v -> b k v -> (Join a b) k v
instance (JoinStyle a b ~ flag, Joinable' flag a b (Join a b)) => Joinable a b where
  join = join' (Proxy :: Proxy flag)
instance Joinable Either (->) where

class Joinable' flag (a :: * -> * -> *) (b :: * -> * -> *) (c :: * -> * -> *) where
  join' :: Proxy flag -> a k v -> b k v -> c k v
instance (DataCube a) => Joinable' SelfJoin a a a where
  join' _ = joinSelf
instance Joinable' OtherJoin a b FunctionCube where
  join' _ = joinAny

triple :: (Joinable b c, Joinable a (Join b c)) => (a k v, b k v, c k v) -> (Join a (Join b c)) k v
triple (a, b, c) = join a (join b c)
  
x = TableCube :: TableCube () ()
y = TableCube :: TableCube () ()
z = x `join` y
w = TableCube :: TableCube () ()
u = w `join` z
v = FunctionCube :: FunctionCube () ()
s = v `join` x
t = triple (x, y, v)

main :: IO ()
main =
  let
  in
    return ()
