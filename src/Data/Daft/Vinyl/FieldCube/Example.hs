{-# LANGUAGE DataKinds                       #-}
{-# LANGUAGE TypeOperators                   #-}

{-# OPTIONS_GHC -fno-warn-unused-binds       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module Data.Daft.Vinyl.FieldCube.Example (
  main
) where


import Data.ByteString.Lazy.Char8 (unpack)
import Data.Daft.DataCube (fromFunction)
import Data.Daft.Vinyl.FieldCube (type (↝), (⋈), (!), ρ)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCube, showFieldCube)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.List.Util.Listable (toTabbeds)
import Data.Maybe (isJust)
import Data.Set (Set, fromList)
import Data.Vinyl.Derived (FieldRec, SField(..))

import qualified Data.Aeson as A (encode)
import qualified Data.Daft.Vinyl.FieldCube.Aeson as A (fromArray, toArray)
import qualified Data.Daft.Vinyl.FieldCube.Bson as B (fromArray, toArray)


-- Types for field names.
type StateUSPS = '("State USPS"     , String)
type StateName = '("State Name"     , String)
type CityName  = '("City Name"      , String)
type StateHash = '("State Hash"     , Int   )
type Longitude = '("Longitude [deg]", Double)
type Latitude  = '("Latitude [deg]" , Double)


-- Functions for accessing fields.
sStateUSPS = SField :: SField StateUSPS
sStateName = SField :: SField StateName
sCityName  = SField :: SField CityName
sStateHash = SField :: SField StateHash
sLongitude = SField :: SField Longitude
sLatitude  = SField :: SField Latitude


-- Some data about states.
states :: '[StateUSPS] ↝ '[StateName]
states =
  let
    statesRaw =
      [
        ["State USPS", "State Name"     ]
      , ["\"CA\""    , "\"California\"" ]
      , ["\"CT\""    , "\"Connecticut\""]
      , ["\"NM\""    , "\"New Mexico\"" ]
      , ["\"CO\""    , "\"Colorado\""   ]
      ]
    Right stateRecs = readFieldCube statesRaw :: Either String ('[StateUSPS] ↝ '[StateName])
  in
    stateRecs


-- A hash function on state names.
hashStates :: '[StateUSPS] ↝ '[StateHash]
hashStates =
  fromFunction $ \k ->
    let
      stateUSPS = sStateUSPS <: k
    in
       return . (sStateHash =:) . product $ map fromEnum stateUSPS


-- Some data about cities.
cities :: '[StateUSPS, CityName] ↝ '[Longitude, Latitude]
cities =
  let
    citiesRaw =
      [
        ["State USPS", "City Name"        , "Longitude [deg]", "Latitude [deg]"]
      , ["\"CA\""    , "\"Los Angeles\""  , "-118.2437"      , "34.0522"       ]
      , ["\"CA\""    , "\"San Francisco\"", "-122.4194"      , "37.7749"       ]
      , ["\"NM\""    , "\"Santa Fe\""     , "-105.9378"      , "35.6870"       ]
      , ["\"CO\""    , "\"Golden\""       , "-105.2211"      , "39.7555"       ]
      , ["\"CO\""    , "\"Denver\""       , "-104.9903"      , "39.7392"       ]
      ]
    Right cityRecs = readFieldCube citiesRaw :: Either String ('[StateUSPS, CityName] ↝ '[Longitude, Latitude])
  in
    cityRecs


-- Some areas of interest.
interest :: Set (FieldRec '[StateUSPS, CityName])
interest =
  fromList [
    sStateUSPS =: "CA" <+> sCityName =: "Los Angeles"
  , sStateUSPS =: "CA" <+> sCityName =: "San Francisco"
  , sStateUSPS =: "CT" <+> sCityName =: "New Haven"
  , sStateUSPS =: "NM" <+> sCityName =: "Santa Fe"
  ]


-- Simple example of some joins of tables and functions.
main :: IO ()
main =
  do
    let
      x :: '[StateUSPS] ↝ '[StateName, StateHash]
      x = states ⋈  hashStates
    putStrLn ""
    putStrLn "Example of evaluation:"
    print $ x ! (sStateUSPS =: "CA")
    let
      y :: '[StateUSPS, CityName] ↝ '[Longitude, Latitude, StateName, StateHash]
      y = cities ⋈  x
      y' = ρ interest y
    putStrLn ""
    putStrLn "Result of some joins with tables and functions:"
    putStrLn . toTabbeds $ showFieldCube y'
    putStrLn "Encoding as JSON:"
    mapM_ (putStrLn . unpack . A.encode) $ A.toArray y'
    let
      y'' :: Maybe ('[StateUSPS, CityName] ↝ '[Longitude, Latitude, StateName, StateHash])
      y'' = A.fromArray $ A.toArray y' 
    putStrLn ""
    putStrLn "Correctly decoding JSON:"
    print $ isJust y''
    putStrLn ""
    putStrLn "Encoding as BSON:"
    mapM_ print $ B.toArray y'
    let
      y''' :: Maybe ('[StateUSPS, CityName] ↝ '[Longitude, Latitude, StateName, StateHash])
      y''' = B.fromArray $ B.toArray y' 
    putStrLn ""
    putStrLn "Correctly decoding BSON:"
    print $ isJust y'''
    putStrLn ""
