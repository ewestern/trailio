
import qualified Data.Vector as V
import qualified Data.Text as T

data SacScale
  = Hiking
  | MountainHiking
  | DemandingMountainHiking
  | AlpineHiking
  | DemandingAlpineHiking 
  | DifficultAlpineHiking



data TrailSegment
  = TrailSegment {
    _trailsegmentGeometry :: LineString
  }

data Trail 
  = Trail {
    _trailName :: T.Text,
    _trailSegments :: V.Vector TrailSegment,

  }



