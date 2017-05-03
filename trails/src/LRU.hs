{-# LANGUAGE BangPatterns #-}
module LRU where

import           Data.Hashable       (Hashable, hash)
import qualified Data.HashPSQ        as HashPSQ
import           Data.IORef          (IORef, newIORef, atomicModifyIORef')
import           Data.Int            (Int64)
import           Data.Maybe          (isNothing)
import qualified Data.Vector         as V
import           Prelude             hiding (lookup)
import           Control.Monad 
import           Control.Monad.IO.Class

data Cache k v = Cache
    { cCapacity :: !Int       -- ^ The maximum number of elements in the queue
    , cSize     :: !Int       -- ^ The current number of elements in the queue
    , cTick     :: !Priority  -- ^ The next logical time
    , cQueue    :: !(HashPSQ.HashPSQ k Priority v)
    } deriving (Eq, Show)

type Priority = Int64

empty :: Int -> Cache k v
empty capacity
    | capacity < 1 = error "Cache.empty: capacity < 1"
    | otherwise    = Cache
        { cCapacity = capacity
        , cSize     = 0
        , cTick     = 0
        , cQueue    = HashPSQ.empty
        }

trim :: (Hashable k, Ord k) => Cache k v -> Cache k v
trim c
    | cTick c == maxBound  = empty (cCapacity c)
    | cSize c > cCapacity c = c
        { cSize  = cSize c - 1
        , cQueue = HashPSQ.deleteMin (cQueue c)
        }
    | otherwise             = c


insert :: (Hashable k, Ord k) => k -> v -> Cache k v -> Cache k v
insert key val c = trim $!
    let (mbOldVal, queue) = HashPSQ.insertView key (cTick c) val (cQueue c)
    in c
        { cSize  = if isNothing mbOldVal then cSize c + 1 else cSize c
        , cTick  = cTick c + 1
        , cQueue = queue
        }

lookup
    :: (Hashable k, Ord k) => k -> Cache k v -> Maybe (v, Cache k v)
lookup k c = case HashPSQ.alter lookupAndBump k (cQueue c) of
    (Nothing, _) -> Nothing
    (Just x, q)  ->
        let !c' = trim $ c {cTick = cTick c + 1, cQueue = q}
        in Just (x, c')
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x,  Just ((cTick c), x))

newtype Handle k v = Handle (IORef (Cache k v))

newHandle :: Int -> IO (Handle k v)
newHandle capacity = Handle <$> newIORef (empty capacity)

cached
    :: (Hashable k, Ord k, MonadIO m)
    => Handle k v -> k -> m v -> m v
cached (Handle ref) k io = do
    lookupRes <- liftIO $ atomicModifyIORef' ref $ \c -> case lookup k c of
        Nothing      -> (c,  Nothing)
        Just (v, c') ->  (c', Just v)
    case lookupRes of
        Just v  -> return v
        Nothing -> do
            v <- io
            liftIO $ atomicModifyIORef' ref $ \c -> (insert k v c, ())
            return v


insertManyCached 
    :: (Hashable k, Ord k, MonadIO m)
    => Handle k v -> [(k, v)] -> m ()
insertManyCached h rvs = liftIO $ forM_ rvs $ insertCached h


insertCached
    :: (Hashable k, Ord k, MonadIO m)
    => Handle k v -> (k, v) -> m ()
insertCached (Handle ref) (k, v) =  liftIO $ atomicModifyIORef' ref $ \c -> (insert k v c, ())



newtype StripedHandle k v = StripedHandle (V.Vector (Handle k v))


newStripedHandle :: Int -> Int -> IO (StripedHandle k v)
newStripedHandle numStripes capacityPerStripe =
    StripedHandle <$> V.replicateM numStripes (newHandle capacityPerStripe)

stripedCached
    :: (Hashable k, Ord k, MonadIO m)
    => StripedHandle k v -> k -> m v -> m v
stripedCached (StripedHandle v) k =
    cached (v V.! idx) k
  where
    idx = hash k `mod` V.length v

insertStripedCached
    :: (Hashable k, Ord k, MonadIO m)
    => StripedHandle k v -> (k, v) -> m ()
insertStripedCached (StripedHandle v) (k, val) = 
  let idx = hash k `mod` V.length v 
  in insertCached (v V.! idx) (k, val)

