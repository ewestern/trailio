{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-} 
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Network.HTTP.Client (newManager, defaultManagerSettings)
import Options.Applicative
import Options.Generic
import Data.Monoid ((<>))
import Data.Char (toLower, isLower)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Servant.API
import Servant.Client
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.Text as T

{-import Database.Postgis.Geometry (SRID)-}

import API
import Trails (TrailsAPI, SegmentAPI)
import Geo (LatLng(..))

type SRID = Maybe Int

data SubCommand
  = Segment_Command GeoCommand
  | Route_Command T.Text
  deriving (Generic, Show)

data GeoCommand 
  = GeoBounds LatLng LatLng SRID 
  | GeoProximity LatLng Int SRID deriving (Generic, Show, Read)

modifiers :: Modifiers
modifiers = defaultModifiers {
    fieldNameModifier = dropWhile (\c -> c == '_')
  , constructorNameModifier = map toLower
}

instance ParseRecord LatLng where
  parseRecord = parseRecordWithModifiers modifiers


prettyPrint :: ToJSON a => Either ServantError a -> IO ()
prettyPrint a = case a of
  Left err -> print err
  Right a' -> BCL.putStrLn . encodePretty $ a'

makeQuery :: ToJSON a => ClientEnv -> ClientM a -> IO ()
makeQuery env m = runClientM m env  >>= prettyPrint

runSegmentQuery :: Client SegmentAPI -> ClientEnv -> GeoCommand -> IO ()
runSegmentQuery serv env comm = do
  let getSegmentBounds :<|> getSegmentProximity = serv
  case comm of
    GeoBounds sw ne srid          ->  makeQuery env (getSegmentBounds sw ne srid) 
    GeoProximity center dis srid  -> makeQuery env (getSegmentProximity center dis srid)

{-runRouteQuery :: Client NamedTrailApi -> ClientEnv -> T.Text -> IO ()-}
{-runRouteQuery serv env comm = makeQuery env (serv comm)-}

runQuery :: ClientEnv -> SubCommand -> IO ()
runQuery env comm = do
    let segment :<|> route = client api
    case comm of
      Route_Command cmd -> makeQuery env (route cmd)
      Segment_Command cmd -> runSegmentQuery segment env cmd
      _ -> error "FOO"


geoParser :: Parser GeoCommand
geoParser = 
  subparser
  (command "bounds"
    (info (GeoBounds <$> parseRecord <*> parseRecord <*> parseRecord) fullDesc)
  <> command "proximity"
    (info (GeoProximity <$> parseRecord <*> parseRecord <*> parseRecord) fullDesc))


data Options 
  = Options {
    subcommand :: SubCommand
  , url :: String
  , port :: Int
  } deriving (Show, Generic)

parseOptions :: Parser Options
parseOptions = 
  Options
    <$> parseRecord
    <*> strOption
      (  value "localhost" 
      <> long "url"
      <> short 'u')
    <*> option auto
      (  value 80
      <> long "port"
      <> short 'p' )


routeParser :: Parser T.Text
routeParser = T.pack <$> (strOption (long "name" <> short 'n'))


subcommandParser :: Parser SubCommand
subcommandParser =  
    hsubparser 
      ( command "segment" 
        (info (Segment_Command <$> geoParser) fullDesc)
      <> command "route"
          (info (Route_Command <$> routeParser) fullDesc)
      )


instance ParseRecord SubCommand where
  parseRecord = subcommandParser

main :: IO ()
main = do
  let p = info (helper <*> parseOptions) (fullDesc <> progDesc "Foo desc")
  Options{..} <- customExecParser (prefs disambiguate) p
  manager <- newManager defaultManagerSettings
  let env = (ClientEnv manager (BaseUrl Http url port ""))
  runQuery env subcommand
