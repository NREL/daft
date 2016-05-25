{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}


module Data.Daft.Vinyl.Core (
  readRec
) where


import Control.Monad (liftM2)
import Data.Daft.String (readEither)
import Data.Vinyl.Core (Rec(..))


-- Class for reading a record.
class ReadRec (rs :: [*]) where
  readRec :: (forall t . Read t => t -> f t) -> [String] -> Either String (Rec f rs)

instance ReadRec '[] where
  readRec _ [] = Right RNil
  readRec _ _  = Left "too many data items for record"

instance (Read t, ReadRec ts) => ReadRec (t ': ts) where
  readRec f (x : xs) = liftM2 (:&) (f <$> readEither x) (readRec f xs)
  readRec _ []       = Left "too few data items for record"
