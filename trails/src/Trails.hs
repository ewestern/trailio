{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Vector as V
import qualified Data.Text as T
import Database.Postgis.Geometry
import Database.PostgreSQL.Simple.SqlQQ
import qualified Data.Map as M
import DB

instance ToField Geometry where
  toField  =  Plain . fromByteString . writeGeometry 

instance FromField Geometry where
  fromField f m = case m of
            Just bs -> return $ readGeometry bs
            Nothing -> error "Invalid Field" 

data SacScale
  = Hiking
  | MountainHiking
  | DemandingMountainHiking
  | AlpineHiking
  | DemandingAlpineHiking 
  | DifficultAlpineHiking


-- select r.name, count(s.*) from osm_routes r inner join  osm_route_members s on r.osm_id = s.osm_id group by r.name limit 10;

data TrailSegment
  = TrailSegment {
      _trailsegmentOsmId :: Int
    , _trailsegmentTrailType :: Maybe T.Text
    , _trailsegmentSacScale :: Maybe T.Text
    , _trailsegmentVisibility :: Maybe T.Text
    , _trailsegmentTrackType :: Maybe T.Text
    , _trailsegmentGeometry :: LineString
  }

instance FromRow TrailSegment where
  fromRow = TrailSegment <$> field <*> field <*> field <*> field <*> field <*> field 

instance ToRow Service where
  toRow (TrailSegment osid tt ss v tt' g) = (osid, tt, ss, v, tt', g)

data Trail 
  = Trail {
    _trailName :: T.Text,
    _trailSegments :: V.Vector TrailSegment,
  }


type BizData = ()

type TrailioM = ReaderT BizData DB


type GeoBoxAPI = "box" 
                :> QueryParam "sw" LatLng
                :> QueryParam "ne" LatLng
                :> TrailioM GeoResponse

type GeoPointAPI = "point"
                :> QueryParam "point" LatLng
                :> QueryParam "distance" Float
                :> TrailioM GeoResponse

type TrailioAPI = 


class GeoQueryable a where
  type Condition v
  findWithin :: Maybe (Condition v) -> Bounds -> DB (M.Map (Ref a, a))
  findNear ::  Maybe (Condition v) -> LatLng -> Int -> DB [(Ref a, a)]

data SegmentCondition
  = LSEmpty

instance ToSql SegmentCondition where
  toSql LSEmpty = undefined
  
instance GeoQueryable TrailSegment where 
  type SegmentCondition v
  findWithin mc bounds = do
-- todo change operator
    cond <- toSql mc
    let q = [sql|
        SELECT id, trail_type, sac_scale, visibility, track_type, geometry
        FROM osm_trails
        WHERE geometry && ? |] <> cond
    rvs <- query q (Only bounds)
    return $ M.fromList $ map pairToTuple rvs
  findNear mv ll d = do
    cond <- toSql mc
    let q = [sql|
        SELECT id, trail_type, sac_scale, visibility, track_type, geometry
        FROM osm_trails
        WHERE geometry && ? |] <> cond
-- TODO!
    rvs <- query q (Only bounds)
    return $ M.fromList $ map pairToTuple rvs

-- member ~ osm_id

{-
getTrailByName :: T.Text -> DB [Trail]
getTrailByName t = 
  let q = [sql|
            SELECT
            FROM osm_routes r
            INNER JOIN osm_routes r, osm_route_members m

  |]
-}

{-
select array_agg((t.osm_id, t.trail_type, t.sac_scale, t.geometry)), m.relname, m.name from import.osm_route_members m
inner join import.osm_trails t
on t.osm_id = m.member
where m.name like '%John Muir%'
group by m.relname, m.name;


-}

