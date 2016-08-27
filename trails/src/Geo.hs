
import GEOS.Serialize
import GEOS.Types
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField


instance ToField Geometry where
  toField  =  Plain . fromByteString . writeHex 

instance FromField Geometry where
  fromField f m = case m of
      Just bs -> return $ readHex bs
      Nothing -> error "Invalid Field" 

data LatLng
  = LatLng {
    _lat :: Float
  , _lng :: Float
  } deriving (Read, Eq, Ord)

instance Show LatLng where
  show (LatLng t g) = 

renderLatLng :: LatLng -> String
renderLatLng (LatLng t g) = "ST_MakePoint(" 
                          <> (show t )
                          <> ","
                          <> (show g )
                          <> ")"

instance ToField LatLng where
  toField  = Plain  . fromString . renderLatLng

instance FromField LatLng where
  fromField = error "not yet defined"

renderBox :: Box -> String
renderBox (Box sw ne) = "ST_MakeBox2D("
                      <> (renderLatLng sw)
                      <> ","
                      <> (renderLatLng ne)
                      <> ")"

instance ToField Box where
  toField = Plain . fromString . renderBox 

instance FromField Box where
  fromField = error ""

data Box
  = Box {
    _sw :: LatLng
  , _ne :: LatLng
  }

instance GeoQueryable LineString where 

class GeoQueryable a where
  findWithin :: Box -> DB a

{-instance -}
