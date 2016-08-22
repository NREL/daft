{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module Data.Daft.Source (
  DataSource(..)
, maybeWithSource
) where


import Control.Arrow (first)
import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)


data DataSource a =
    FileData
    {
      filePath :: FilePath
    }
  | TextData
    {
      parsableText :: String
    }
  | BuiltinData
    {
      builtin :: a
    }
    deriving (Eq, Generic, Ord)

instance Read a => Read (DataSource a) where
  readsPrec n ('f' : 'i' : 'l' : 'e' : ' '                         : x) = fmap (first FileData   ) $ readsPrec n x
  readsPrec n ('t' : 'e' : 'x' : 't' : ' '                         : x) = fmap (first TextData   ) $ readsPrec n x
  readsPrec n ('b' : 'u' : 'i' : 'l' : 't' : '-' : 'i' : 'n' : ' ' : x) = fmap (first BuiltinData) $ readsPrec n x
  readsPrec _ _                                                         = []

instance Show a => Show (DataSource a) where
  show FileData{..}    = "file "    ++ show filePath
  show TextData{..}    = "text "    ++ show parsableText
  show BuiltinData{..} = "built-in" ++ show builtin

instance FromJSON a => FromJSON (DataSource a)

instance ToJSON a => ToJSON (DataSource a)


maybeWithSource :: Monad m => Maybe (DataSource a) -> (DataSource a -> m ()) -> m ()
maybeWithSource Nothing       = const $ return ()
maybeWithSource (Just source) = ($ source)
