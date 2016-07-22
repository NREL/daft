{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}


module Data.String.Util (
  readExcept
) where


import Control.Monad.Except (MonadError, throwError)
import Data.String (IsString(..))
import Data.String.ToString (ToString(..))


-- Read a string or return a message that it cannot be parsed.
readExcept :: (ToString s, IsString e, MonadError e m, Read a) => s -> m a
readExcept x =
  let
    x' = toString x
  in
    case reads x' of
      [(result, [])] -> return result
      _              -> throwError . fromString $ "failed to parse \"" ++ x' ++ "\""
