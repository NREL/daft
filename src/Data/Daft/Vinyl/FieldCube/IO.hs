{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll  #-}


module Data.Daft.Vinyl.FieldCube.IO (
-- * Input/output
  readFieldCube
, showFieldCube
{- TODO: Implement these:
, readFieldCubeFile
, readFiledCubeSource
, writeFieldCubeFile
, writeFieldCubeSource
-}
) where


import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Control.Monad.Except (MonadError, MonadIO, throwError)
import Control.Monad.Except.Util (tryIO)
import Data.Daft.DataCube (DataCube)
import Data.Daft.Source (DataSource(..))
import Data.Daft.TypeLevel (Intersection, Union)
import Data.Daft.Vinyl.FieldCube (FieldCube, fromRecords)
import Data.Daft.Vinyl.FieldRec (InternalLabeled)
import Data.Daft.Vinyl.FieldRec (InternalLabeled, labels)
import Data.Daft.Vinyl.FieldRec.IO (InternalDefault, InternalReadFieldRec, InternalShowFieldRec, readFieldRecs, showFieldRecs)
import Data.Daft.Vinyl.TypeLevel (RDistinct, RJoin(rjoin), RUnion(runion))
import Data.Default (Default(..))
import Data.List (nub)
import Data.List.Util (elemPermutation)
import Data.List.Util.Listable (fromTabbeds, toTabbeds)
import Data.Maybe (fromJust, isNothing)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String (IsString(..))
import Data.String.ToString (ToString(..))
import Data.String.Util (readExcept)
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (ElField(..), FieldRec, )
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆), rcast)
import Data.Vinyl.TypeLevel (RecAll)
import Data.Vinyl.TypeLevel (type (++))
import GHC.TypeLits (KnownSymbol, Symbol)

import qualified Data.Daft.DataCube as C (Gregator(..), Joiner(Joiner), aggregateWithKey, antijoin, disaggregateWithKey, evaluate, fromTable, join, knownKeys, projectWithKey, reify, selectWithKey, semijoin, toKnownTable, toTable)
import qualified Data.Set as S (fromDistinctAscList, map, toAscList)


readFieldCube :: forall ks vs s e m . (ks ⊆ Union ks vs, vs ⊆ Union ks vs, Ord (FieldRec ks), Eq s, IsString s, ToString s, IsString e, MonadError e m, InternalDefault (FieldRec (Union ks vs)), RecAll ElField (Union ks vs) InternalLabeled, InternalReadFieldRec (Union ks vs)) => [[s]] -> m (FieldCube ks vs)
readFieldCube =
  fmap (C.fromTable (rcast :: FieldRec (Union ks vs) -> FieldRec ks) (rcast :: FieldRec (Union ks vs) -> FieldRec vs))
    . readFieldRecs


showFieldCube :: forall ks vs . (Ord (FieldRec ks), InternalDefault (FieldRec (ks ++ vs)), RecAll ElField (ks ++ vs) InternalLabeled, InternalShowFieldRec (ks ++ vs)) => FieldCube ks vs -> [[String]]
showFieldCube = showFieldRecs . C.toKnownTable ((<+>) :: FieldRec ks -> FieldRec vs -> FieldRec (ks ++ vs))
