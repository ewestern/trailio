module Main where

import Servant.Server
import API (api, server)

main :: IO ()
main = run serve api server



