{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Pure database implementation using 'Data.Map'. More efficient
-- option would be something akin to muteble prefix
-- tree. http://hackage.haskell.org/package/bytestring-trie might do
-- better though it's immutable.

module Pos.DB.Pure
       ( DBPureMap
       , DBPure
       , pureBlockIndexDB
       , pureGStateDB
       , pureLrcDB
       , pureMiscDB
       , pureBlocksStorage

       , MonadPureDB
       , dbPureDump
       , dbPureReset

       , DBPureVar
       , newDBPureVar
       , cloneDBPure

       , dbGetPureDefault
       , dbIterSourcePureDefault
       , dbPutPureDefault
       , dbDeletePureDefault
       , dbWriteBatchPureDefault
       , atomicModifyIORefPure

       , DBPureDiff(..)
       , dbPureDiff
       ) where

import           Universum

import           Control.Lens (at, makeLenses)
import qualified Data.ByteString as BS
import           Data.Conduit (ConduitT)
import qualified Data.Conduit.List as CL
import           Data.Default (Default (..))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Database.RocksDB as Rocks

import           Pos.Binary.Class (Bi)
import           Pos.Chain.Block (HeaderHash)
import           Pos.DB.Class (DBIteratorClass (..), DBTag (..), IterType,
                     iterKeyPrefix)
import           Pos.DB.Functions (processIterEntry)
import           Pos.Util.Util (HasLens (..))

-- | Bytestring to Bytestring mapping mimicking rocks kv storage.
type DBPureMap = Map ByteString ByteString

-- | Pure database datatype. Includes 4 subdatabases mocking rocks dbs
-- and a map for storing blocks data (we store them in files directly
-- in real implementation).
data DBPure = DBPure
    { _pureBlockIndexDB  :: DBPureMap
    , _pureGStateDB      :: DBPureMap
    , _pureLrcDB         :: DBPureMap
    , _pureMiscDB        :: DBPureMap
    , _pureBlocksStorage :: Map HeaderHash ByteString
    } deriving (Eq)

makeLenses ''DBPure

instance Default DBPure where
    def = DBPure mempty mempty mempty mempty mempty

type DBPureVar = IORef DBPure

-- | Creates new db var.
newDBPureVar :: MonadIO m => m DBPureVar
newDBPureVar = newIORef def

-- | Clones existing pure DB into a new one with the same contents.
-- TODO: implement for any 'MonadDBRead' (requires iteration over whole DB).
cloneDBPure :: MonadIO m => DBPureVar -> m DBPureVar
cloneDBPure = readIORef >=> newIORef

tagToLens :: DBTag -> Lens' DBPure DBPureMap
tagToLens BlockIndexDB = pureBlockIndexDB
tagToLens GStateDB     = pureGStateDB
tagToLens LrcDB        = pureLrcDB
tagToLens MiscDB       = pureMiscDB

-- | Monad having access to the pure database.
type MonadPureDB ctx m =
    ( MonadReader ctx m
    , HasLens DBPureVar ctx DBPureVar
    , MonadMask m
    , MonadIO m
    )

dbPureDump :: MonadPureDB ctx m => m DBPure
dbPureDump = view (lensOf @DBPureVar) >>= readIORef

dbPureReset :: MonadPureDB ctx m => DBPure -> m ()
dbPureReset dbPure = do
    ref <- view (lensOf @DBPureVar)
    writeIORef ref dbPure

----------------------------------------------------------------------------
-- MonadDBRead / MonadDB
----------------------------------------------------------------------------

dbGetPureDefault :: MonadPureDB ctx m => DBTag -> ByteString -> m (Maybe ByteString)
dbGetPureDefault (tagToLens -> l) key =
    view (l . at key) <$> (view (lensOf @DBPureVar) >>= readIORef)

dbIterSourcePureDefault ::
       ( MonadPureDB ctx m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       )
    => DBTag
    -> Proxy i
    -> ConduitT () (IterType i) m ()
dbIterSourcePureDefault (tagToLens -> l) (_ :: Proxy i) = do
    let filterPrefix = M.filterWithKey $ \k _ -> iterKeyPrefix @i `BS.isPrefixOf` k
    (dbPureVar :: DBPureVar) <- lift $ view (lensOf @DBPureVar)
    (filtered :: [(ByteString, ByteString)]) <-
        M.toList . filterPrefix . (view l) <$> lift (readIORef dbPureVar)
    deserialized <- catMaybes <$> mapM (processIterEntry @i) filtered
    CL.sourceList deserialized

dbPutPureDefault :: MonadPureDB ctx m => DBTag -> ByteString -> ByteString -> m ()
dbPutPureDefault (tagToLens -> l) key val =
    view (lensOf @DBPureVar) >>= atomicModifyIORefPure (l . at key .~ Just val)

dbDeletePureDefault :: MonadPureDB ctx m => DBTag -> ByteString -> m ()
dbDeletePureDefault (tagToLens -> l) key =
    view (lensOf @DBPureVar) >>= atomicModifyIORefPure (l . at key .~ Nothing)

dbWriteBatchPureDefault :: MonadPureDB ctx m => DBTag -> [Rocks.BatchOp] -> m ()
dbWriteBatchPureDefault (tagToLens -> l) batchOps =
    view (lensOf @DBPureVar) >>= atomicModifyIORefPure action
  where
    -- Apply actions from left to right (right fold, but reverse application order)
    action = foldr (\batchop acc -> acc . processOp batchop) identity batchOps
    processOp (Rocks.Put k v) = l . at k .~ Just v
    processOp (Rocks.Del k)   = l . at k .~ Nothing

atomicModifyIORefPure :: (MonadIO m) => (a -> a) -> IORef a -> m ()
atomicModifyIORefPure foo = flip atomicModifyIORef $ \a -> (foo a, ())

-- | A diff between a pair of bytestrings. We could compute some sort of
-- string difference here, but instead just store them both, as the user will
-- probably want to preprocess them before comparison.
-- Invariant: '_bsdLeft /= _bsdRight'.
data ByteStringDiff = ByteStringDiff
    { _bsdLeft  :: ByteString
    , _bsdRight :: ByteString
    } deriving (Show)

bsDiff :: ByteString -> ByteString -> Maybe ByteStringDiff
bsDiff b1 b2
    | b1 == b2 = Nothing
    | otherwise = Just $ ByteStringDiff b1 b2

-- | A diff between two maps. '_mdMissingKeysLeft' contains the set of keys
-- not present in the first map (but present in the second).
-- '_mdMissingKeysRight' containts the set of keys not present in the second
-- map (but present in the first one).
-- '_mdDifferentElements' contains the difference between elements present
-- in both maps (for different elements).
-- Invariant: at least one of '_mdMissingKeys', '_mdMissingKeysRight', or
-- '_mdDifferentElements' fields is non-empty.
data MapDiff k vDiff = MapDiff
    { _mdMissingKeysLeft   :: !(Set k)
    , _mdMissingKeysRight  :: !(Set k)
    , _mdDifferentElements :: !(Map k vDiff)
    } deriving (Show)

mapDiff ::
       Ord k
    => (v -> v -> Maybe vDiff)
    -> Map k v
    -> Map k v
    -> Maybe (MapDiff k vDiff)
mapDiff elemDiff m1 m2 = [ result | not emptyDiff ]
  where
    mCommon = M.intersectionWith (,) m1 m2
    missingKeysLeft  = M.keysSet (m2 M.\\ mCommon)
    missingKeysRight = M.keysSet (m1 M.\\ mCommon)
    mCommonDiff = M.mapMaybe (uncurry elemDiff) mCommon
    result = MapDiff
        { _mdMissingKeysLeft = missingKeysLeft
        , _mdMissingKeysRight = missingKeysRight
        , _mdDifferentElements = mCommonDiff
        }
    emptyDiff =
        S.null missingKeysLeft &&
        S.null missingKeysRight &&
        M.null mCommonDiff

type DBPureMapDiff = MapDiff ByteString ByteStringDiff

-- | A diff between two pure databases.
-- Invariant: at least one of the fields is not 'Nothing'.
data DBPureDiff = DBPureDiff
    { _pdBlockIndexDB  :: Maybe DBPureMapDiff
    , _pdGStateDB      :: Maybe DBPureMapDiff
    , _pdLrcDB         :: Maybe DBPureMapDiff
    , _pdMiscDB        :: Maybe DBPureMapDiff
    , _pdBlocksStorage :: Maybe (MapDiff HeaderHash ByteStringDiff)
    } deriving (Show)

dbPureDiff :: DBPure -> DBPure -> Maybe DBPureDiff
dbPureDiff dbp1 dbp2 = [ result | not emptyDiff ]
  where
    mapDiffOn ::
           Ord k
        => (DBPure -> Map k ByteString)
        -> Maybe (MapDiff k ByteStringDiff)
    mapDiffOn f = mapDiff bsDiff (f dbp1) (f dbp2)
    purgeVolatileGState =
        M.delete "c/maxsd" -- this key is not supposed to be rollbacked, so
                           -- we don't want to take it into account
    result = DBPureDiff
        { _pdBlockIndexDB  = mapDiffOn _pureBlockIndexDB
        , _pdGStateDB      = mapDiffOn (purgeVolatileGState . _pureGStateDB)
        , _pdLrcDB         = mapDiffOn _pureLrcDB
        , _pdMiscDB        = mapDiffOn _pureMiscDB
        , _pdBlocksStorage = mapDiffOn _pureBlocksStorage
        }
    emptyDiff = isNothing (_pdGStateDB result)
