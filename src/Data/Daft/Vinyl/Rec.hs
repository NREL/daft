{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}


module Data.Daft.Vinyl.Rec (
  readRec
) where


import Control.Monad (liftM2)
import Control.Monad.Except (MonadError, throwError)
import Data.String (IsString)
import Data.String.ToString (ToString)
import Data.String.Util (readExcept)
import Data.Vinyl.Core (Rec(..))


-- Class for reading a record.
class ReadRec (rs :: [*]) where
  readRec :: (ToString s, IsString e, MonadError e m) => (forall t . Read t => t -> f t) -> [s] -> m (Rec f rs)

instance ReadRec '[] where
  readRec _ [] = return RNil
  readRec _ _  = throwError "too many data items for record"

instance (Read t, ReadRec ts) => ReadRec (t ': ts) where
  readRec f (x : xs) = liftM2 (:&) (f <$> readExcept x) (readRec f xs)
  readRec _ []       = throwError "too few data items for record"
