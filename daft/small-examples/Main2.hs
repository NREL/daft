{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Proxy (Proxy(..))

class DataCube a where
  joinSelf :: a -> a -> a

joinAny :: a -> b -> FunctionCube
joinAny = undefined

data TableCube = TableCube
instance DataCube TableCube where
  joinSelf _ _ = TableCube

data FunctionCube = FunctionCube
instance DataCube FunctionCube where
  joinSelf _ _ = FunctionCube

type family Join (c1 :: *) (c2 :: *) where
  Join a a = a
  Join a b = FunctionCube

data SelfJoin
data OtherJoin
type family JoinStyle c1 c2 where
  JoinStyle a a = SelfJoin
  JoinStyle a b = OtherJoin

class Joinable c1 c2 where
  join :: c1 -> c2 -> Join c1 c2
instance (Joinable c1 c2, JoinStyle c1 c2 ~ flag, Joinable' flag c1 c2 (Join c1 c2)) => Joinable c1 c2 where
  join = join' (Proxy :: Proxy flag)
instance Joinable Double Int where

class Joinable' flag c1 c2 c3 where
  join' :: Proxy flag -> c1 -> c2 -> c3
instance (DataCube c1) => Joinable' SelfJoin c1 c1 c1 where
  join' _ = joinSelf
instance Joinable' OtherJoin c1 c2 FunctionCube where
  join' _ = joinAny

--triple :: (Joinable c2 c3, Joinable c1 (Join c2 c3)) => (c1, c2, c3) -> Join c1 (Join c2 c3)
triple (c1, c2, c3) = join c1 (join c2 c3)
  
x = TableCube
y = TableCube
z = x `join` y
w = TableCube
u = w `join` z

main :: IO ()
main =
  let
  in
    return ()
