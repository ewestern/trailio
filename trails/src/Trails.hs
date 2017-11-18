{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Servant
import Servant.Server
import Geo
import JSON
import Ref
import DB
import Types


instance FromRow TrailSegment where
  fromRow = TrailSegment <$> field <*> field <*> field <*> field <*> field <*> field 

instance ToRow TrailSegment where
  toRow (TrailSegment osid tt ss v tt' g) = toRow (osid, tt, ss, v, tt', g)

type BizData = ()

type TrailioM = ExceptT ServantErr (ReaderT BizData DB)

liftDB :: DB a -> TrailioM a
liftDB = lift . lift



type TrailsAPI = SegmentAPI

type Response a = M.Map (Ref a) a

type SegmentAPI = "segment" 
                :> GeoAPI (Response TrailSegment) 

type NamedTrailApi = "trail" 
                  :> GeoAPI (Response Route)

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

segmentServer = findSegmentBounds :<|> findSegmentProximity


class GeoQueryable a where
  type GeoCondition a
  findWithin :: Maybe (GeoCondition a) -> Bounds -> SRID -> DB (Response a)
  findNear ::  Maybe (GeoCondition a) -> LatLng -> Int -> SRID -> DB (Response a)

data SegmentCondition
  = LSEmpty

instance ToSql SegmentCondition where
  toSql LSEmpty = undefined
  
instance GeoQueryable Route where
  type GeoCondition TrailSegment = SegmentCondition
  findWithin mc bounds srid = do
    cond <- toSql mc
    let srid' = maybe 4326 id srid
        q = [sql|
          SELECT osm_id, nullif(trail_type, ''), 
          nullif(sac_scale, ''), nullif(visibility, ''), 
          nullif(track_type, ''), st_transform(geometry, ?)
          FROM osm_trails
          WHERE geometry && ? and |] <> cond
    rvs <- query q (srid', bounds)
    return $ M.fromList $ map pairToTuple rvs

instance GeoQueryable TrailSegment where 
  type GeoCondition TrailSegment = SegmentCondition 
  findWithin mc bounds srid = do
-- todo change operator
    cond <- toSql mc
    let srid' = maybe 4326 id srid
        q = [sql|
          SELECT id, osm_id, nullif(trail_type, ''), 
          nullif(sac_scale, ''), nullif(visibility, ''), 
          nullif(track_type, ''), st_transform(geometry, ?)
          FROM osm_trails
          WHERE geometry && ? and |] <> cond
    rvs <- query q (srid', bounds)
    return $ M.fromList $ map pairToTuple rvs
  findNear mc ll d srid = do
    cond <- toSql mc
    let srid' = maybe 4326 id srid
        q = [sql|
          SELECT id, osm_id, nullif(trail_type, ''), 
          nullif(sac_scale, ''), nullif(visibility, ''), 
          nullif(track_type, ''), st_transform(geometry, ?)
          FROM osm_trails
          WHERE geometry <-> ? < ? and |] <> cond
    rvs <- query q (srid', ll, d)
    return $ M.fromList $ map pairToTuple rvs


$(deriveJSON' ''TrailSegment)
$(deriveJSON' ''Route)
