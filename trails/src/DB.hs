
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings         #-}
module DB where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Monoid
import Error

import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PT
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Database.PostgreSQL.Simple.ToRow (toRow, ToRow)
import GHC.Generics
import Data.Aeson
import Ref

-- TODO: readCached
-- TODO: add created and updated timestamps
pairToTuple :: Ref a PG.:. a -> (Ref a, a) 
pairToTuple (r PG.:. v) = (r, v)

instance PG.ToRow (Ref a) where
  toRow (Ref i ) =  [toField i]

instance ToField (Ref a) where
  toField (Ref a) = toField a

instance ToJSON (Ref a) where
  toJSON (Ref v) = toJSON v

instance FromJSON (Ref a) where
  parseJSON v = Ref <$> parseJSON v

data DBContext = DBContext {
  _connection :: PG.Connection
}


class ToSql a where
  toSql :: a -> DB PG.Query

instance ToSql a => ToSql (Maybe a) where
  toSql (Just a) = toSql a
  toSql Nothing = return (" " <> [sql| true |])

{-class (PG.ToRow v, PG.FromRow v, ToSql (Condition v)) =>  Queryable v where-}
  {-type Condition v-}
  {-createMany :: [v] -> DB [(Ref v, v)]-}
  {-updateMany :: Maybe (Condition v) -> [(Ref v, v)] -> DB ()-}
  {-readMany :: Maybe (Condition v) -> [Ref v] -> DB [(Ref v, v)]-}
  {-deleteMany :: Maybe (Condition v) -> [Ref v] -> DB ()-}
{----}
  {-create :: v -> DB (Ref v, v)-}
  {-create v = do-}
    {-vs <- createMany [v]-}
    {-case vs of-}
      {-x:_ -> return x-}
      {-[] -> throwError err500-}

  {-update :: Maybe (Condition v) -> (Ref v, v) -> DB ()-}
  {-update c (r, v) = updateMany c [(r, v)]-}

  {-read :: Maybe (Condition v) -> (Ref v) ->  DB v-}
  {-read c r = do-}
    {-rs <- readMany c [r]-}
    {-case rs of-}
      {-(_,v):_ -> return v-}
      {-[] -> throwError err500-}

  {-delete :: Maybe (Condition v) -> (Ref v) -> DB ()-}
  {-delete c r = deleteMany c [r]-}


type DB = ReaderT DBContext (ExceptT Error IO)

formatQuery :: ToRow q => PG.Query -> q -> DB PG.Query
formatQuery q v = do
  conn <- asks _connection
  liftIO $ (($) ((<>) " ")) <$> PT.Query <$> PG.formatQuery conn q v

query ::(PG.ToRow q, PG.FromRow r) => PG.Query -> q -> DB [r]
query q v = do
  ctx <- ask
  liftIO $ PG.query (_connection ctx) q v

executeMany :: PG.ToRow q => PG.Query -> [q] -> DB () 
executeMany q v = do
  ctx <- ask
  liftIO $ void $ PG.executeMany (_connection ctx) q v

execute :: PG.ToRow q => PG.Query -> q -> DB () 
execute q v = do
  ctx <- ask
  liftIO $ void $ PG.execute (_connection ctx) q v


returning :: (PG.ToRow q, PG.FromRow r) => PG.Query -> [q] -> DB [r]
returning q v = do
  ctx <- ask
  liftIO $ PG.returning (_connection ctx) q v

runDB :: DBContext -> DB a -> (ExceptT Error IO a)
runDB ctx db =  do
  mapExceptT (PG.withTransaction ( _connection ctx) ) $ runReaderT db ctx
 
