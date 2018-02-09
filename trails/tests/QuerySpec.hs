{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module QuerySpec where


import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import qualified Data.ByteString as B
import Test.Hspec
import Trails
import Types









{-
data CompositeType
  = CompositeType {
  member_id :: Int,
  segmentName :: Maybe String,
  trailType :: Maybe String
}
  

data Foo = Foo {
  foo :: B.ByteString
} deriving (Show)

instance FromField Foo where
  fromField f v = Foo <$> (fromField  f v)

{-instance FromRow Foo where-}
  {-fromRow = Foo <$> field-}

{-instance FromField (Maybe B.ByteString) where-}
    {-fromField f _mval = return _mval-}

statement = [sql|
          SELECT 
          route_name,
          array_agg(
            ROW(
              member_id,
              nullif(segment_name, ''),
              nullif(trail_type, '')
            )
          )
          FROM import.osm_segments
          group by route_name
          LIMIT 10  |]

-}



spec = do
  describe "query tests" $ do
    it "tests record parsing" $ do
      {-s <- readFile "output"-}
      c <- connectPostgreSQL "host=localhost port=5432 dbname=trailio user=trailio"
      (x:xs) <- query c routeNameQuery (4326 ::Int, "Hollywood"::String, "Hollywood"::String)
      print ( x :: Route)
      {-print $ ((fromPGArray arr)::[Foo])-}
      {-print (arr :: (String, [B.ByteString]-}
      close c


