{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Servant.JS
import System.FilePath
import Options.Applicative
import Data.Monoid((<>))


import API (api)


data Args = Args 
  { path :: String }

parser :: Parser Args
parser = Args
    <$> strOption 
          (long "path"
          <> help "The path to generate the javascript" )

main :: IO ()
main = do
  args <- execParser opts
  -- write the JS code to www/api.js at startup
  writeJSForAPI api vanillaJS (path args)
  where
    opts = info ( parser <**> helper)
            (fullDesc <> progDesc "Generate javascript for the api")

