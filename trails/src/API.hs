
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

import Servant.API
import Servant.Server 
import Servant
import Data.Monoid ((<>))
import qualified Data.Text as T
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Network.URI.Encode


import Trails
import Data.Proxy
import DB
import Geo


showText :: Show a => a -> T.Text
showText = T.pack . show

readText :: Read a => T.Text -> a
readText = Prelude.read . T.unpack

instance ToHttpApiData LatLng where
  toUrlPiece (LatLng t g) = encodeText $ (showText t) <> "," <> (showText g)  

instance FromHttpApiData LatLng where
  parseUrlPiece t = case fmap readText $ T.splitOn "," $ decodeText t  of
    [lat, lng]  -> Right $ LatLng lat lng
    _           -> Left "Indecipherable LatLng"

 
enterTrailio :: DBContext -> BizData -> TrailioM :~> Handler
--enterTrailio ctx bd = Nat $ mapExceptT (\m -> runDB ctx $ runReaderT m bd )
enterTrailio ctx bd = Nat $ mapExceptT (\m -> runDB ctx $ runReaderT m bd )


trailServer :: ServerT TrailsAPI TrailioM
trailServer = segmentServer -- :<|> trailServer
{-enter' :: DBContext -> BizData -> TrailioM :~> ExceptT ServantErr IO-}
{-enter' ctx bd =  runDB ctx . runTrailio bd-}

server :: DBContext -> ServerT TrailsAPI Handler
server ctx = enter  (enterTrailio ctx ()) trailServer

api :: Proxy TrailsAPI
api = Proxy
