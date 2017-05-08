
{-# LANGUAGE TemplateHaskell #-}

module JSON (
  deriveJSON',
  options

) where

import Data.Aeson.TH
import Language.Haskell.TH
import Data.Char


stripRecordPrefix :: String -> String
stripRecordPrefix = dropWhile (not . isUpper)

deriveJSON' :: Name -> Q [Dec]
deriveJSON' = deriveJSON options

unCap :: String -> String
unCap [] = []
unCap (x:xs) = (toLower x):xs

options :: Options
options = defaultOptions {
  fieldLabelModifier = unCap . stripRecordPrefix,
  constructorTagModifier = unCap . stripRecordPrefix,
  sumEncoding = ObjectWithSingleField
}


