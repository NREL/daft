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
import System.Environment (getArgs)

import qualified Hasql.Connection as C
import qualified Hasql.Session as S


main :: IO ()
main =
  do
    [username, password, database] <- getArgs
    Right connection <- C.acquire $ C.settings "localhost" 5432 (pack username) (pack password) (pack database)
    Right positions <- S.run (queryPositions "CA") connection
    C.release connection
    putStrLn ""
    putStrLn . toTabbeds . showFieldRecs $ positions


queryPositions :: String -> S.Session [FieldRec '[StateUSPS, Longitude, Latitude]]
queryPositions state =
  S.query (sStateUSPS =: state)
    $ selectList "test"
