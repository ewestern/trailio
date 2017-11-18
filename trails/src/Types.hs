{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Database.Postgis.Geometry
import qualified Data.Vector as V
import qualified Data.Text as T

data SacScale
  = Hiking
  | MountainHiking
  | DemandingMountainHiking
  | AlpineHiking
  | DemandingAlpineHiking 
  | DifficultAlpineHiking deriving (Eq, Ord, Show, Read)



-- select r.name, count(s.*) from osm_routes r inner join  osm_route_members s on r.osm_id = s.osm_id group by r.name limit 10;

data TrailSegment
  = TrailSegment {
      _trailsegmentOsmId :: Int
    , _trailsegmentTrailType :: Maybe T.Text
    , _trailsegmentSacScale :: Maybe T.Text
    , _trailsegmentVisibility :: Maybe T.Text
    , _trailsegmentTrackType :: Maybe T.Text
    , _trailsegmentGeometry :: Geometry
  } deriving (Eq, Show)


data  Route
  = Route {
    _routeName :: T.Text,
    _routeSegments :: V.Vector TrailSegment,
    _routeOsmId :: Int
  } deriving (Eq, Show)


