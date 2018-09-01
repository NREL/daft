{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main (
  main
) where


import Data.ByteString.Char8 (pack)
import Data.Daft.Vinyl.FieldCube.Example hiding (main)
import Data.Daft.Vinyl.FieldRec ((=:))
import Data.Daft.Vinyl.FieldRec.Hasql (selectList)
import Data.Daft.Vinyl.FieldRec.IO (showFieldRecs)
import Data.List.Util.Listable (toTabbeds)
import Data.Vinyl.Derived (FieldRec)
import Hasql.Connection (acquire, release, settings)
import Hasql.Session (Session, query, run)
import System.Environment (getArgs)



main :: IO ()
main =
  do
    [username, password, database] <- fmap pack <$> getArgs
    Right connection <- acquire $ settings "localhost" 5432 username password database
    Right positions <- run (queryPositions "CA") connection
    release connection
    putStrLn ""
    putStrLn . toTabbeds . showFieldRecs $ positions


queryPositions :: String -> Session [FieldRec '[StateUSPS, Longitude, Latitude]]
queryPositions state =
  query (sStateUSPS =: state)
    $ selectList "test"
