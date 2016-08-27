
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module API where

import Data.Geometry.Geos.Serialize
import Data.Geometry.Geos.Types
import Servant.API
import Data.Monoid ((<>))
import qualified Data.Text as T

showText :: Show a => a -> T.Text
showText = T.pack . show

readText :: Read a => T.Text -> a
readText = read . T.unpack

instance ToHttpApiData LatLng where
  toUrlPiece (LatLng t g) = (showText t) <> "," <> (showText g)  

instance FromHttpApiData LatLng where
  parseUrlPiece t = case fmap readText $ T.splitOn "," t  of
    [lat, lng]  -> Right $ LatLng lat lng
    _           -> Left "Indecipherable LatLng"

type BizData = ()

type TrailioM = ReaderT BizData IO

type GeoBoxAPI = "box" 
                :> QueryParam "sw" LatLng
                :> QueryParam "ne" LatLng
                :> TrailioM GeoResponse

type GeoPointAPI = "point"
                :> QueryParam "point" LatLng
                :> QueryParam "distance" Float
                :> TrailioM GeoResponse

          

boxServer :: 
