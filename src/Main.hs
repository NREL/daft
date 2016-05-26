{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}


module Main
where


import Control.Arrow ((&&&))
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((%~))
import Data.Char (toUpper)
import Data.Daft.Keyed (Keyed, aggregateByKey)
import Data.Daft.Vinyl.Derived ((<:), fieldMap', keyed, replacing2, unkeyed, using1)
import Data.Daft.Vinyl.Derived.ReadWrite (readFieldRecs, displayFieldRecs)
import Data.Vinyl.Core (Rec(..), (<+>))
import Data.Vinyl.Derived (ElField, FieldRec, SField(..), (=:), rfield)
import Data.Vinyl.Lens (type (∈), rcast, rlens)


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
      rotate (longitude', latitude') = 
        let
          angle = pi / 3
          longitude'' = longitude' * cos angle - latitude' * sin angle
          latitude''  = longitude' * sin angle + latitude' * cos angle
        in
          (longitude'', latitude'')
      rotated = rotate `replacing2` (sLongitude, sLatitude) <$> locations
    putStrLn "#### Rotated locations"
    putStrLn $ displayFieldRecs rotated

    let
      populations :: [FieldRec '[Population, City]]
      Right populations = readFieldRecs populationData
    putStrLn "#### Populations"
    putStrLn $ displayFieldRecs populations

    let
      uppered = (city %~ map toUpper) <$> populations
    putStrLn "#### Uppercase populations"
    putStrLn $ displayFieldRecs uppered

    let
      toThousands :: Int -> Double
      toThousands = (/ 1000) . fromIntegral
      -- Without using lenses:
      onlyPopulations :: [FieldRec '[Population']]
      onlyPopulations = toThousands `using1` (sPopulation, sPopulation') <$> populations
      -- FIXME: It is hard to use lenses just to transform one type of record to another?  Here is an unsatisfactory attempt:
      onlyPopulations' = ((sPopulation' =:) . toThousands . (^. population)) <$> populations
    putStrLn "#### Just populations in thousands"
    putStrLn $ displayFieldRecs onlyPopulations
    print $ onlyPopulations == onlyPopulations'
    putStrLn ""

    let
      -- A pedestrian approach.  This should be straightforward for general users.
      modifiedPopulations :: [FieldRec '[Population', City]]
      modifiedPopulations =
        [
          let
            p = toThousands $ r ^. population
          in
            sPopulation' =: p <+> rcast r
        |
          r <- populations
        ]
      -- Using lenses and arrows to apply a function on a field in one record and create a new type of record.
      modifiedPopulations' =
        uncurry (:&)
          . ((toThousands `fieldMap'`) . (^. rlens sPopulation) &&& rcast)
          <$> populations
    putStrLn "#### City and population in thousands"
    putStrLn $ displayFieldRecs modifiedPopulations
    print $ modifiedPopulations == modifiedPopulations'
    putStrLn ""

    let
      keyedLocations :: [Keyed (FieldRec '[State]) (FieldRec '[Longitude, Latitude])]
      keyedLocations = keyed <$> locations
    putStrLn "#### Keyed locations"
    print keyedLocations
    putStrLn ""

    let
      average :: [Double] -> Double
      average xs = sum xs / fromIntegral (length xs)
      averageField s = (s =:) . average . map (s <:)
      averagedLocations :: [FieldRec '[State, Longitude, Latitude]]
      averagedLocations =
        unkeyed
          <$> aggregateByKey
                ((<+>) <$> averageField sLongitude <*> averageField sLatitude)
--              FIXME: Why can't we write the line above as the following?
--              (averageField sLongitude <~+~> averageField sLatitude) where ff1 <~+~> ff2 = (<+>) <$> ff1 <*> ff2
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


-- Lenses.

state :: (Functor f, t ~ ElField State, State ∈ rs) => (String -> f String) -> FieldRec rs -> f (FieldRec rs)
state = rlens sState . rfield

city :: (Functor f, t ~ ElField City, City ∈ rs) => (String -> f String) -> FieldRec rs -> f (FieldRec rs)
city = rlens sCity . rfield

longitude :: (Functor f, t ~ ElField Longitude, Longitude ∈ rs) => (Double -> f Double) -> FieldRec rs -> f (FieldRec rs)
longitude = rlens sLongitude . rfield

latitude :: (Functor f, t ~ ElField Latitude, Latitude ∈ rs) => (Double -> f Double) -> FieldRec rs -> f (FieldRec rs)
latitude = rlens sLatitude . rfield

population :: (Functor f, t ~ ElField Population, Population ∈ rs) => (Int -> f Int) -> FieldRec rs -> f (FieldRec rs)
population = rlens sPopulation . rfield

population' :: (Functor f, t ~ ElField Population', Population' ∈ rs) => (Double -> f Double) -> FieldRec rs -> f (FieldRec rs)
population' = rlens sPopulation' . rfield


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
