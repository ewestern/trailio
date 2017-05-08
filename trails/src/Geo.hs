module Geo where

{-import GEOS.Serialize-}
{-import GEOS.Types-}
import Data.Monoid ((<>))
import Data.ByteString.Builder 
import Data.ByteString.Lazy (fromStrict)

import Database.Postgis.Geometry
import Database.Postgis.Serialize
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import DB


instance ToField Geometry where
  toField  =  Plain . lazyByteString . writeGeometry

instance FromField Geometry where
  fromField f m = case m of
      Just bs -> return $ readGeometry $ fromStrict bs
      Nothing -> error "Invalid Field" 

data LatLng
  = LatLng {
    _lat :: Float
  , _lng :: Float
  } deriving (Read, Eq, Ord)

renderLatLng :: LatLng -> String
renderLatLng (LatLng t g) = "ST_transform("
                          <> "st_setsrid("
                          <> "ST_Point(" 
                          <> (show g ) <> " :: double precision"
                          <> ","
                          <> (show t ) <> ":: double precision"
                          <> "), 4326), 3857)"

instance ToField LatLng where
  toField  = Plain  . string8 . renderLatLng

instance FromField LatLng where
  fromField = error "not yet defined"

renderBounds :: Bounds -> String
renderBounds (Bounds sw ne) = "ST_MakeBox2D("
                      <> (renderLatLng sw)
                      <> ","
                      <> (renderLatLng ne)
                      <> ")"

instance ToField Bounds where
  toField = Plain . string8 . renderBounds 

instance FromField Bounds where
  fromField = error ""

data Bounds = Bounds 
  { _sw :: LatLng
  , _ne :: LatLng
  }


