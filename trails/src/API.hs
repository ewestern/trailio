
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module API (
    server
  , api
) where

import Data.Geometry.Geos.Serialize
import Data.Geometry.Geos.Types
import Servant.API
import Data.Monoid ((<>))
import qualified Data.Text as T
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Geo

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

          
runTrailio :: BizData -> TrailioM a -> DB a
runTrailio bd = runReaderT bd

boxServer :: Maybe LatLng -> Maybe LatLng -> TrailioM GeoResponse
boxServer la ln = do
  

enter' :: DBContext -> BizData -> TrailioM :~> ExceptT ServantErr IO
enter' ctx bd =  runDB ctx . runBiz bd

server :: ServerT API TrailioM
server = enter  (enter' ctx bd) server'
  where
    server' = boxServer :<|> pointServer
