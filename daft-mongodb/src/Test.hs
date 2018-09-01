{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Main (
  main
) where


import Control.Monad (void)
import Control.Monad.Except (liftIO)
import Data.Bson (Field((:=)), Value(Doc, Float))
import Data.Daft.Vinyl.FieldCube (type (↝))
import Data.Daft.Vinyl.FieldCube.Example hiding (main)
import Data.Daft.Vinyl.FieldCube.IO (showFieldCube)
import Data.Daft.Vinyl.FieldCube.MongoDB (insertAll, saveAll, select, selectKey, rest)
import Data.Daft.Vinyl.FieldRec ((=:), (<+>))
import Data.Daft.Vinyl.FieldRec.MongoDB (findOne)
import Data.List.Util.Listable (toTabbeds)
import Data.Vinyl.Derived (FieldRec)
import Database.MongoDB (access, close, connect, dropCollection, find, host, master)

import qualified Database.MongoDB as M (select)


main :: IO ()
main =
  do
    pipe <- connect (host "127.0.0.1")
    void
      $ access pipe master "daft-mongodb"
      $ do
        saveAll "cities"               cities
        void $ dropCollection "states"
        void $ dropCollection "hashed-states-cities"
        x <-   insertAll "states" states
        void $ insertAll "hashed-states-cities" hashedStatesCities
        liftIO $ putStrLn ""
        liftIO $ putStrLn "Keys for states:"
        liftIO $ mapM_ print x
        Just y <- findOne (selectKey (sStateUSPS =: "CA" <+> sCityName =: "San Francisco") "cities")
        liftIO $ putStrLn ""
        liftIO $ putStrLn "Longitude and latitude of San Francisco:"
        liftIO $ print (y :: FieldRec '[Longitude, Latitude])
        liftIO $ putStrLn ""
        liftIO $ putStrLn "Cities greater north of 35 degress:"
        Just z <- rest =<< find (M.select ["Latitude [deg]" := Doc ["$gt" := Float 35]] "hashed-states-cities")
        liftIO . putStrLn . toTabbeds $ showFieldCube (z :: '[StateUSPS, CityName] ↝ '[StateName, StateHash, Longitude, Latitude])
        liftIO $ putStrLn ""
        liftIO $ putStrLn "Cities with state hash of 4355:"
        Just u <- rest =<< find (select (sStateHash =: 4355) "hashed-states-cities")
        liftIO . putStrLn . toTabbeds $ showFieldCube (u :: '[StateUSPS, CityName] ↝ '[StateName, StateHash, Longitude, Latitude])
        liftIO $ putStrLn ""
        liftIO $ putStrLn "Cities with California postal code:"
        Just v <- rest =<< find (selectKey (sStateUSPS =: "CA") "hashed-states-cities")
        liftIO . putStrLn . toTabbeds $ showFieldCube (v :: '[StateUSPS, CityName] ↝ '[StateName, StateHash, Longitude, Latitude])
        liftIO $ putStrLn ""
    close pipe
