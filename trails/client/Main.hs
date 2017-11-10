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
import Data.Aeson
import Servant.API
import Servant.Client

import API
import Trails
import Ref
import Types

data SubCommand
  = Segment_Command (CrudCommand TrailSegment TrailSegment) 
  | Trail_Command (CrudCommand Trail Trail)
  deriving (Generic, Show)

data CrudCommand a b
  = Create b
  | Read (Ref a)
  | List
  | Update (Ref a) b
  | Delete (Ref a) 
  deriving (Generic, Show, Read)



makeQuery :: ToJSON a => ClientEnv -> ClientM a -> IO ()
makeQuery env m = runClientM m env  >>= prettyPrint


runSegmentQuery :: Client SegmentAPI -> ClientEnv -> CrudCommand Trail Trail -> IO ()
runSegmentQuery serv env comm = do
  let getSegmentBounds :<|> getSegmentProximity = serv
  case comm of
    {-Read a      ->  makeQuery env (getAccount a) -}
    List        ->  makeQuery env listAccounts
    {-Create a    -> makeQuery env (createAccount a) -}
    {-Update r a  ->  makeQuery env (updateAccount (Just r) a)-}
    _ -> undefined

runQuery :: ClientEnv -> SubCommand -> IO ()
runQuery env comm = do
    let segment = client api
    case comm of
      {-Trail_Command cmd -> -}
      Segment_Command cmd -> runSegmentQuery segment env cmd
{-
runServiceQuery serviceServer env cmd

runAccountQuery accountServer env cmd
      Person  cmd -> runPersonQuery personServer env cmd
      Booking cmd -> runBookingQuery bookingServer env cmd
      Field   cmd -> runFieldQuery fieldsServer env cmd
      Login _ -> undefined
-}

crudParser = 
  subparser
    (command "create"
      (info (Create <$> parseRecord)
        (fullDesc <> progDesc ""))
    <> command "read"
      (info (Read <$> parseRecord)
        (fullDesc <> progDesc "test of read"))
    <> command "list"
      (info ( pure List )
        (fullDesc <> progDesc "test of read"))
    <> command "update"
      (info (Update <$> parseRecord <*> parseRecord)
        (fullDesc <> progDesc "test of update")))



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


subcommandParser :: Parser SubCommand
subcommandParser =  
    subparser 
      (command "segment" 
        (info (Segment_Command <$> crudParser)
          (progDesc "segment foo"))
      <> command "trail"
          (info (Trail_Command <$> crudParser)
            (progDesc "person foo")))



main :: IO ()
main = do
  let p = info (helper <*> parseOptions) (fullDesc <> progDesc "Foo desc")
  Options{..} <- customExecParser (prefs disambiguate) p
  manager <- newManager defaultManagerSettings
  let env = (ClientEnv manager (BaseUrl Http url port ""))
  runQuery env subcommand
