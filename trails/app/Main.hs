{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import API (server, api)
import DB
import Servant.Server
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Network.Wai.Middleware.Cors
import Data.Default
import Data.Aeson
import Data.Monoid ((<>))
import Control.Monad.Trans.Maybe
import System.Environment
import System.IO
import System.Directory
import System.FilePath.Posix ((</>))
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Char8 as BC
import Options.Applicative



app1 ::  Bool ->  DBContext -> Application
app1 _ ctx = serve api (server ctx)

makeConnectionString :: IO BC.ByteString
makeConnectionString = do
    connStr <- runMaybeT $ do
        let keys = [ " host="
                   , " port="
                   , " user="
                   , " password="
                   , " dbname="
                   ]
            envs = [ "PGHOST"
                   , "PGPORT"
                   , "PGUSER"
                   , "PGPASS"
                   , "PGDATABASE"
                   ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        return $ mconcat $ zipWith (<>) keys envVars
    case connStr of
      Just c -> return $ BC.pack c
      Nothing -> error "envars not properly set for postgres"

data Args 
  = Args {
    logDir :: String
  , logFileName :: String
  , cacheNumStripes :: Int
  , cacheSizeStripe :: Int
  , appPort :: Int 
  , devMode :: Bool  }
    
parser :: Parser Args
parser = Args
      <$> strOption (long "log-dir" <> short 'd' <> metavar "DIR" <> value  "/var/log/trailio" <> help "Use DIR for log")
      <*> strOption (long "log-file-name" <> short 'l' <> metavar "LOG" <> value  "server.log" <> help "Use LOG for log")
      <*> option auto (long "cache-stripes" <> short 's' <> metavar "STRIPES" <> value 64  <> help "Number of stripes in cache.")
      <*> option auto (long "cache-stripe-size" <> short 'n' <> metavar "SIZE" <> value 1000  <> help "Number of items in stripe.")
      <*> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 8081  <> help "Port to listen on.")
      <*> switch (long "dev" <> help "Run in dev mode.")


main :: IO ()
main = do
  Args{..} <- execParser (info (helper <*> parser) fullDesc)
  connStr <- makeConnectionString
  createDirectoryIfMissing True logDir
  fileHandle <- openFile (logDir </> logFileName) AppendMode
  let ctx = DBContext connStr Nothing devMode
  tl <- mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON}
  if devMode
    then run appPort $ tl . simpleCors $ app1 True ctx
        -- then run appPort $ logStdoutDev . simpleCors $ app1 True staticPath ctx
    else do
      logger <- mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON, destination = Handle fileHandle }
      run appPort . logger $ app1 False ctx
      

