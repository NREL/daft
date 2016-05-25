{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}


module Main
where


import Control.Arrow ((&&&))
import Data.Char (toUpper)
import Data.Daft.Keyed (Keyed, aggregateByKey)
import Data.Daft.Vinyl.Derived ((<:), aggregating1, keyed, replacing2, unkeyed, using1)
import Data.Daft.Vinyl.Derived.ReadWrite (readFieldRecs, displayFieldRecs)
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec, SField(..), (=:))
import Data.Vinyl.Lens (rcast)


main :: IO ()
main = do

    putStrLn ""

    let
      locations :: [FieldRec '[City, State, Latitude, Longitude]]
      Right locations = readFieldRecs locationData
    putStrLn "#### Locations"
    putStrLn $ displayFieldRecs locations

    let
      onlyCityStates :: [FieldRec '[State, City]]
      onlyCityStates = rcast <$> locations
    putStrLn "#### Just states and cities"
    putStrLn $ displayFieldRecs onlyCityStates

    let
      rotate :: (Double, Double) -> (Double, Double)
      rotate (longitude, latitude) = 
        let
          angle = pi / 3
          longitude' = longitude * cos angle - latitude * sin angle
          latitude'  = longitude * sin angle + latitude * cos angle
        in
          (longitude', latitude')
      rotated = rotate `replacing2` (sLongitude, sLatitude) <$> locations
    putStrLn "#### Rotated locations"
    putStrLn $ displayFieldRecs rotated

    let
      populations :: [FieldRec '[Population, City]]
      Right populations = readFieldRecs populationData
    putStrLn "#### Populations"
    putStrLn $ displayFieldRecs populations

    let
      uppered = (sCity =:) . map toUpper . (sCity <:) <$> populations
    putStrLn "#### Uppercase populations"
    putStrLn $ displayFieldRecs uppered

    let
      onlyPopulations = ((/ 1000) . fromIntegral) `using1` (sPopulation, sPopulation') <$> populations
    putStrLn "#### Just populations in thousands"
    putStrLn $ displayFieldRecs onlyPopulations

    let
      keyedLocations :: [Keyed (FieldRec '[State]) (FieldRec '[Longitude, Latitude])]
      keyedLocations = keyed <$> locations
    putStrLn "#### Keyed locations"
    print keyedLocations
    putStrLn ""

    let
      average :: [Double] -> Double
      average xs = sum xs / fromIntegral (length xs)
      averagedLocations :: [FieldRec '[State, Longitude, Latitude]]
      averagedLocations =
        unkeyed
          <$> aggregateByKey
                (uncurry (<+>) . (average `aggregating1` (sLongitude, sLongitude) &&& (sLatitude =:) . average . map (sLatitude <:)))
                keyedLocations
    putStrLn "#### Averaged locations"
    putStrLn $ displayFieldRecs averagedLocations


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
