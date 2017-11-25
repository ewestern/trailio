{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Trails where


import Control.Monad.Trans.Reader
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid ((<>))
import Control.Monad.Except
import Database.Postgis.Geometry
import Database.Postgis.JSON ()
import Database.PostgreSQL.Simple hiding (query)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ
import Data.Binary.Builder
import qualified Data.ByteString as BS
import Servant hiding ((:.))
import Servant.Server hiding ((:.))
import Geo
import JSON
import Ref
import DB
import Types


instance FromRow TrailSegment where
  fromRow = TrailSegment <$> field <*> field <*> field <*> field <*> field <*> field <*> field 


fromComposite :: (Int, T.Text) :. V.Vector TrailSegment -> Route
fromComposite ((oid, n):.vs) = Route oid n vs

{-instance FromField TrailSegment where-}
  {-fromField = fromJSONField-}

instance FromRow Route where
  fromRow = fmap fromComposite fromRow

instance FromField TrailSegment where
  fromField = undefined

type BizData = ()

type TrailioM = ExceptT ServantErr (ReaderT BizData DB)

liftDB :: DB a -> TrailioM a
liftDB = lift . lift

type Response a = M.Map (Ref a) a

type SegmentAPI = "segment" 
                :> GeoAPI (Response TrailSegment) 

type NamedTrailApi = "trail" 
                  :> Capture "name" T.Text
                  :> Get '[JSON] (Response Route)

type GeoAPI a = GeoBoundsAPI a :<|> GeoProximityAPI a

type GeoBoundsAPI a = "bounds" 
                :> Capture "sw" LatLng
                :> Capture "ne" LatLng
                :> QueryParam "srid" Int
                :> Get '[JSON] a

type GeoProximityAPI a = "proximity"
                :> Capture "point" LatLng
                :> Capture "distance" Int
                :> QueryParam "srid" Int
                :> Get '[JSON] a


findSegmentBounds :: LatLng -> LatLng -> SRID -> TrailioM (Response TrailSegment)
findSegmentBounds sw ne srid = liftDB $ findWithin Nothing (Bounds sw ne) srid

findSegmentProximity :: LatLng -> Int -> SRID -> TrailioM (Response TrailSegment)
findSegmentProximity ll d srid = liftDB $ findNear Nothing ll d srid

findRouteByName :: T.Text -> TrailioM (Response Route)
findRouteByName name = liftDB $ findRouteByNameDB name

findRouteByNameDB :: T.Text -> DB (Response Route)
findRouteByNameDB name = do
    let q = [sql|
                SELECT osm_id, route_name, 
                array_agg(
                  ROW(
                    member_id,
                    nullif(segment_name, ''),
                    nullif(trail_type, ''),
                    nullif(sac_scale, ''),
                    nullif(visibility, ''),
                    nullif(track_type, ''),
                    st_transform(geometry, ?)
                  )
                )
                FROM osm_segments
                WHERE route_name like ('%' || ? || '%') 
                OR segment_name like ('%' || ? || '%')
                GROUP BY osm_id, route_name  |]
    rvs <- query q (4326::Int, name, name)
    return $ M.fromList $ map pairToTuple rvs


type TrailsAPI = SegmentAPI :<|> NamedTrailApi

segmentServer = findSegmentBounds :<|> findSegmentProximity

routeServer = findRouteByName


class GeoQueryable a where
  type GeoCondition a
  findWithin :: Maybe (GeoCondition a) -> Bounds -> SRID -> DB (Response a)
  findNear ::  Maybe (GeoCondition a) -> LatLng -> Int -> SRID -> DB (Response a)

data SegmentCondition
  = LSEmpty

instance ToSql SegmentCondition where
  toSql LSEmpty = undefined
  
{-
instance GeoQueryable Route where
  type GeoCondition TrailSegment = SegmentCondition
  findWithin mc bounds srid = do
    cond <- toSql mc
    let srid' = maybe 4326 id srid
        q = [sql|
          SELECT osm_id, nullif(trail_type, ''), nullif(segment_name, '')
          nullif(sac_scale, ''), nullif(visibility, ''), 
          nullif(track_type, ''), st_transform(geometry, ?)
          FROM osm_trails
          WHERE geometry && ? and |] <> cond
    rvs <- query q (srid', bounds)
    return $ M.fromList $ map pairToTuple rvs
-}

instance GeoQueryable TrailSegment where 
  type GeoCondition TrailSegment = SegmentCondition 
  findWithin mc bounds srid = do
-- todo change operator
    cond <- toSql mc
    let srid' = maybe 4326 id srid
        q = [sql|
          SELECT id, osm_id, nullif(segment_name), nullif(trail_type, ''), 
          nullif(sac_scale, ''), nullif(visibility, ''), 
          nullif(track_type, ''), st_transform(geometry, ?)
          FROM osm_segments
          WHERE geometry && ? and |] <> cond
    rvs <- query q (srid', bounds)
    return $ M.fromList $ map pairToTuple rvs
  findNear mc ll d srid = do
    cond <- toSql mc
    let srid' = maybe 4326 id srid
        q = [sql|
          SELECT id, osm_id, nullif(segment_name), nullif(trail_type, ''), 
          nullif(sac_scale, ''), nullif(visibility, ''), 
          nullif(track_type, ''), st_transform(geometry, ?)
          FROM osm_trails
          WHERE geometry <-> ? < ? and |] <> cond
    rvs <- query q (srid', ll, d)
    return $ M.fromList $ map pairToTuple rvs


$(deriveJSON' ''TrailSegment)
$(deriveJSON' ''Route)
