
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE GADTs #-}

module DB where

import Prelude hiding (read)
import Control.Monad
import Control.Monad.Reader
import Data.Monoid

import qualified Data.ByteString as B
import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PT
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Database.PostgreSQL.Simple.ToRow (toRow, ToRow)
import GHC.Generics
import Data.Hashable
import qualified Data.Text as T
import Ref
import Types
import Data.Typeable


data DBError
  = DBError_NoConnection
  | DBError_ItemExistsError String deriving (Show, Eq, Ord)

type DB = ReaderT DBContext IO

pairToTuple :: Ref a PG.:. a -> (Ref a, a) 
pairToTuple (r PG.:. v) = (r, v)

instance PG.ToRow (Ref a) where
  toRow (Ref i ) =  [toField i]

instance ToField (Ref a) where
  toField (Ref a) = toField a


data DBContext = DBContext {
  {-_cacheHandle :: StripedHandle CacheKey CacheValue,-}
  _connectionString :: B.ByteString,
  _connection :: Maybe PG.Connection,
  _staging :: Bool
}


askConnection :: DB PG.Connection
askConnection = do 
  mc <- asks _connection
  case mc of 
    Just conn -> return conn
    Nothing -> error "Connection not supplied"
-- throwError DBError_NoConnection

{-askHandle :: DB (StripedHandle CacheKey CacheValue)-}
{-askHandle = asks _cacheHandle-}

class ToSql a where 
  toSql :: a -> DB PG.Query

instance ToSql a => ToSql (Maybe a) where
  toSql (Just a) = toSql a
  toSql Nothing = return (" " <> [sql| true |])

{-
readCached :: (Queryable a, Cacheable a) => Maybe (Condition a) -> Ref a -> DB (Maybe a)
readCached cond r = do 
  handle <- askHandle
  let m = (fmap . fmap) makeValue (read cond r)
  mv <- stripedCached handle (makeKey r) m
  case getValue mv  of
    Just val -> do
      verify <- maybeVerify cond (r, val)
      if verify
        then return $ Just val
        else return Nothing
    Nothing -> return Nothing

updateCached :: (Queryable a, Cacheable a) => Maybe (Condition a) -> (Ref a, a) -> DB ()
updateCached cond (r, v) = do
  handle <- askHandle
  update cond (r, v)
  insertStripedCached handle (makeKey r, makeValue v)
  
createCached :: (Queryable a, Cacheable a) => a -> DB (Ref a, a)
createCached v = do
  handle <- askHandle
  (r, v') <- create v
  insertStripedCached handle (makeKey r, makeValue v')
  return (r, v')

deleteCached :: (Queryable a, Cacheable a) => Maybe (Condition a) -> (Ref a) -> DB ()
deleteCached _ _ = undefined
-}

maybeVerify :: Queryable a => Maybe (Condition a) -> (Ref a, a) -> DB Bool
maybeVerify (Just c) tup = verifyCondition c tup
maybeVerify Nothing _ = return True

class (PG.ToRow v, PG.FromRow v, Typeable v) =>  Queryable v where
  type Condition v
  createMany :: [v] -> DB [(Ref v, v)]
  updateMany :: Maybe (Condition v) -> [(Ref v, v)] -> DB ()
  readMany :: Maybe (Condition v) -> [Ref v] -> DB [(Ref v, v)]
  deleteMany :: Maybe (Condition v) -> [Ref v] -> DB ()
--
  create :: v -> DB (Ref v, v)
  create v = do
    vs <- createMany [v]
    case vs of
      x:_ -> return x
      [] ->  error "Shouldn't happen"

  update :: Maybe (Condition v) -> (Ref v, v) -> DB ()
  update c (r, v) = updateMany c [(r, v)]

  read :: Maybe (Condition v) -> (Ref v) ->  DB (Maybe v)
  read c r =  do
    rs <- readMany c [r]
    case rs of
      (_,v):_ -> return $ Just v
      []      -> return Nothing

  delete :: Maybe (Condition v) -> (Ref v) -> DB ()
  delete c r = deleteMany c [r]

  verifyCondition :: Condition v -> (Ref v, v) -> DB Bool




formatQuery :: ToRow q => PG.Query -> q -> DB PG.Query
formatQuery q v = do
  conn <- askConnection
  liftIO $ (($) ((<>) " ")) <$> PT.Query <$> PG.formatQuery conn q v


query ::(PG.ToRow q, PG.FromRow r) => PG.Query -> q -> DB [r]
query q v = do
  conn <- askConnection
  liftIO $ PG.query conn q v

query_ :: PG.FromRow r => PG.Query ->  DB [r]
query_ q = do
  conn <- askConnection
  liftIO $ PG.query_ conn q

executeMany :: PG.ToRow q => PG.Query -> [q] -> DB () 
executeMany q v = do
  conn <- askConnection
  liftIO $ void $ PG.executeMany conn q v

execute :: PG.ToRow q => PG.Query -> q -> DB () 
execute q v = do
  conn <- askConnection
  liftIO $ void $ PG.execute conn q v


returning :: (PG.ToRow q, PG.FromRow r) => PG.Query -> [q] -> DB [r]
returning q v = do
  conn <- askConnection
  liftIO $ PG.returning conn q v


nameLike :: T.Text -> T.Text -> DB Bool
nameLike s t = do
  let q = [sql| select ? like %?% |]
  [PT.Only b] <- query q (s, t)
  return b


class Cacheable a where
  makeKey :: Ref a -> CacheKey
  getValue :: CacheValue ->  a
  makeValue :: a -> CacheValue



instance Cacheable Route where
  makeKey = CacheKey_Route
  getValue (CacheValue_Route t) = t
  getValue _ = error "Shouldn't happen"
  makeValue = CacheValue_Route

instance Cacheable TrailSegment where
  makeKey = CacheKey_TrailSegment
  getValue (CacheValue_TrailSegment t) = t
  getValue _ = error "Shouldn't happen"
  makeValue = CacheValue_TrailSegment

data CacheKey 
  = CacheKey_Route (Ref Route)
  | CacheKey_TrailSegment (Ref TrailSegment)
  deriving (Ord, Eq, Generic)

instance Hashable CacheKey 

data CacheValue 
  = CacheValue_Route Route
  | CacheValue_TrailSegment TrailSegment
  {-deriving (Ord, Eq, Generic)-}


runDB :: DBContext -> DB a -> IO a
runDB ctx db =  do
  conn <- liftIO $ PG.connectPostgreSQL $ _connectionString ctx
  if _staging ctx
    then liftIO (PG.execute_ conn "set search_path to import, public") >> return ()
    else return ()
  PG.withTransaction conn $ runReaderT db $ ctx { _connection = Just conn }
