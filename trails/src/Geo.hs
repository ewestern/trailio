module Geo where

import GEOS.Serialize
import GEOS.Types
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import DB


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

renderBounds :: Bounds -> String
renderBounds (Bounds sw ne) = "ST_MakeBounds2D("
                      <> (renderLatLng sw)
                      <> ","
                      <> (renderLatLng ne)
                      <> ")"

instance ToField Bounds where
  toField = Plain . fromString . renderBounds 

instance FromField Bounds where
  fromField = error ""

data Bounds = Bounds 
  { _sw :: LatLng
  , _ne :: LatLng
  }


