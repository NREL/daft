{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}


module Main
where


import Data.Char (toUpper)
import Data.Daft.Vinyl.Derived (readFieldRecs, replacing1, replacing2, using1)
import Data.Vinyl.Derived (FieldRec, SField(..))


main :: IO ()
main =
  do
    let
      locations :: [FieldRec '[City, State, Latitude, Longitude]]
      Right locations = readFieldRecs locationData
      rotated = rotate `replacing2` (sLongitude, sLatitude) <$> locations
    putStrLn "Locations:"
    putStrLn $ unlines $ map show locations
    putStrLn "Rotated locations:"
    putStrLn $ unlines $ map show rotated
    let
      populations :: [FieldRec '[Population, City]]
      Right populations = readFieldRecs populationData
      uppered = map toUpper `replacing1` sCity <$> populations
      onlyPopulations = ((/ 1000) . fromIntegral) `using1` (sPopulation, sPopulation') <$> populations
    putStrLn "Populations:"
    putStrLn $ unlines $ map show populations
    putStrLn "Uppercase populations:"
    putStrLn $ unlines $ map show uppered
    putStrLn "Just populations in thousands:"
    putStrLn $ unlines $ map show onlyPopulations


-- Types of fields.
type State       = '("State USPS"        , String)
type City        = '("City Name"         , String)
type Longitude   = '("Longitude [deg]"   , Double)
type Latitude    = '("Latitude [deg]"    , Double)
type Population  = '("Population"        , Int   )
type Population' = '("Population [1000s]", Double)


-- Proxy-like singletons for fields.  Note that we could just use proxies instead, like "Proxy :: Proxy City", scattered throughout the code.
sState       = SField :: SField State
sCity        = SField :: SField City
sLongitude   = SField :: SField Longitude
sLatitude    = SField :: SField Latitude
sPopulation  = SField :: SField Population
sPopulation' = SField :: SField Population'


-- Example tabular (CSV-like) data.
locationData :: [[String]]
locationData = [
                 ["State USPS", "City Name"      , "Longitude [deg]", "Latitude [deg]"]
               , ["\"CO\""    , "\"Denver\""     , "-104.9"         , "35.8"          ]
               , ["\"CA\""    , "\"Los Angeles\"", "-118.1"         , "34.0"          ]
               , ["\"DC\""    , "\"Washington\"" ,  "-77.0"         , "38.9"          ]
               , ["\"CO\""    , "\"Golden\""     , "-105.1"         , "35.8"          ]
               ]
populationData :: [[String]]
populationData = [
                   ["City Name"      , "Population"]
                 , ["\"Los Angeles\"", "12150996"  ]
                 , ["\"Denver\""     ,  "2374203"  ]
                 , ["\"Golden\""     ,    "18867"  ]
                 ]
  

-- Function for rotating.
rotate :: (Double, Double) -> (Double, Double)
rotate (longitude, latitude) = 
  let
    angle = pi / 3
    longitude' = longitude * cos angle - latitude * sin angle
    latitude'  = longitude * sin angle + latitude * cos angle
  in
    (longitude', latitude')
