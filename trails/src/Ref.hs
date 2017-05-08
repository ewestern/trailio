{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ref where

import Data.Int
import GHC.Generics
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Data.Aeson
import qualified Data.Map as M
import Data.Hashable
import Data.Data
import Servant.API
import Servant.Docs

data Ref a = Ref { unRef :: Int64 } deriving (Generic, Eq, Ord, Data, Typeable, Read)

deriving instance Typeable [Ref a]

instance Hashable (Ref a)

instance Show (Ref a)  where
  show (Ref r) = show r

instance PG.FromRow (Ref a) where
  fromRow = Ref <$> field

instance FromField (Ref a) where
  fromField f b = Ref <$> (fromField f b)


instance ToSample (Ref a) where
  toSamples _ = singleSample $ Ref 1

{-instance ToJSON a => ToJSON (M.Map (Ref b) a) where-}
  {-toJSON = toJSON . M.mapKeys (\(Ref v) -> show v)-}

{-instance FromJSON a => FromJSON (M.Map (Ref b) a) where-}
  {-parseJSON v = M.mapKeys (\s -> Ref $ Prelude.read s ) <$>  parseJSON v-}

instance ToJSON (Ref a) where
  toJSON (Ref v) = toJSON . show $ v

instance FromJSON (Ref a) where
  parseJSON v = (Ref . Prelude.read) <$> (parseJSON  v)

instance FromHttpApiData (Ref a) where
  parseUrlPiece t = Ref <$> parseQueryParam t

instance ToHttpApiData (Ref a) where
  toUrlPiece (Ref a) = toUrlPiece a

