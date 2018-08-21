{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Sqlite database for the 'TxMeta' portion of the wallet kernel.
module Cardano.Wallet.Kernel.DB.Sqlite (

    -- * Resource creation and acquisition
      newConnection
    , closeMetaDB

    -- * Basic API
    , putTxMeta
    , getTxMeta
    , getTxMetas

    -- * Unsafe functions
    , unsafeMigrateMetaDB

    -- * testing
    , mkInputs
    , fromInputs
    , mkOutputs
    , fromOutputs
    ) where

import           Universum

import           Database.Beam.Backend.SQL (FromBackendRow,
                     HasSqlValueSyntax (..), IsSql92DataTypeSyntax, intType,
                     valueE, varCharType)
import           Database.Beam.Backend.SQL.SQL92 (Sql92OrderingExpressionSyntax,
                     Sql92SelectOrderingSyntax)
import           Database.Beam.Query (HasSqlEqualityCheck, between_, in_, (&&.),
                     (<.), (<=.), (==.), (>.), (>=.))
import qualified Database.Beam.Query as SQL
import qualified Database.Beam.Query.Internal as SQL
import           Database.Beam.Schema (Beamable, Database, DatabaseSettings,
                     PrimaryKey, Table)
import qualified Database.Beam.Schema as Beam
import           Database.Beam.Sqlite.Connection (Sqlite, runBeamSqlite,
                     runBeamSqlite)
import           Database.Beam.Sqlite.Syntax (SqliteCommandSyntax,
                     SqliteDataTypeSyntax, SqliteExpressionSyntax,
                     SqliteSelectSyntax, SqliteValueSyntax, fromSqliteCommand,
                     sqliteBigIntType, sqliteRenderSyntaxScript)
import qualified Database.SQLite.Simple as Sqlite
import           Database.SQLite.Simple.FromField (FromField (..), returnError)
import qualified Database.SQLite.SimpleErrors as Sqlite
import qualified Database.SQLite.SimpleErrors.Types as Sqlite

import           Control.Exception (throwIO, toException)
import qualified Data.Foldable as Foldable
import           Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Time.Units (Second, fromMicroseconds, toMicroseconds)
import           Database.Beam.Migrate (CheckedDatabaseSettings, DataType (..),
                     Migration, MigrationSteps, boolean, createTable,
                     evaluateDatabase, executeMigration, field, migrationStep,
                     notNull, runMigrationSteps, unCheckDatabase, unique)
import           Formatting (sformat)
import           GHC.Generics (Generic)

import           Cardano.Wallet.Kernel.DB.TxMeta.Types (AccountFops (..),
                     FilterOperation (..), Limit (..), Offset (..),
                     SortCriteria (..), SortDirection (..), Sorting (..))
import qualified Cardano.Wallet.Kernel.DB.TxMeta.Types as Kernel
-- execution time should move out of WalletLayer so that we don`t depend on it.
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core
import           Pos.Crypto.Hashing (decodeAbstractHash, hashHexF)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | A type modelling the underlying SQL database.
data MetaDB f = MetaDB { _mDbMeta    :: f (Beam.TableEntity TxMetaT)
                       , _mDbInputs  :: f (Beam.TableEntity TxInputT)
                       , _mDbOutputs :: f (Beam.TableEntity TxOutputT)
                       } deriving Generic

instance Database Sqlite MetaDB

{--

* Table 1: ~tx_metas~
** Primary kehy: ~tx_meta_id~

| tx_meta_id | tx_meta_amount | tx_meta_created_at | tx_meta_is_local | tx_meta_is_outgoing | tx_meta_account | tx_meta_wallet
|------------+----------------+--------------------+------------------+---------------------+-----------------+-----------------
| Txp.TxId   | Core.Coin      | Core.Timestamp     | Bool             | Bool                | Word32          | Core.Address

--}
data TxMetaT f = TxMeta {
      _txMetaTableId         :: Beam.Columnar f Txp.TxId
    , _txMetaTableAmount     :: Beam.Columnar f Core.Coin
    , _txMetaTableCreatedAt  :: Beam.Columnar f Core.Timestamp
    , _txMetaTableIsLocal    :: Beam.Columnar f Bool
    , _txMetaTableIsOutgoing :: Beam.Columnar f Bool
    , _txMetaTableWalletId   :: Beam.Columnar f Core.Address
    , _txMetaTableAccountIx  :: Beam.Columnar f Word32
    } deriving Generic

type TxMeta = TxMetaT Identity

deriving instance Eq TxMeta
deriving instance Show TxMeta

-- | Creates a storage-specific 'TxMeta' out of a 'Kernel.TxMeta'.
mkTxMeta :: Kernel.TxMeta -> TxMeta
mkTxMeta txMeta = TxMeta {
                  _txMetaTableId         = txMeta ^. Kernel.txMetaId
                , _txMetaTableAmount     = txMeta ^. Kernel.txMetaAmount
                , _txMetaTableCreatedAt  = txMeta ^. Kernel.txMetaCreationAt
                , _txMetaTableIsLocal    = txMeta ^. Kernel.txMetaIsLocal
                , _txMetaTableIsOutgoing = txMeta ^. Kernel.txMetaIsOutgoing
                , _txMetaTableWalletId   = txMeta ^. Kernel.txMetaWalletId
                , _txMetaTableAccountIx  = txMeta ^. Kernel.txMetaAccountIx
                }

instance Beamable TxMetaT

instance Table TxMetaT where
    data PrimaryKey TxMetaT f = TxIdPrimKey (Beam.Columnar f Txp.TxId) deriving Generic
    primaryKey = TxIdPrimKey . _txMetaTableId

instance Beamable (PrimaryKey TxMetaT)

{--

* Table 2: ~tx_meta_inputs~
** Primary key: ~input_id~, ~input_foreign_txid~, ~input_foreign_index~

input_id   | input_address | input_coin | input_foreign_txid | input_foreign_index
-----------|---------------+------------+--------------------+--------------------
Txp.TxId   | Core.Address  | Core.Coin  | Txp.TxId           | Word32


--}

-- | The inputs' table.
data TxInputT f = TxInputT {
      _inputTableTxId         :: Beam.PrimaryKey TxMetaT f
    , _inputTableAddress      :: Beam.Columnar f Core.Address
    , _inputTableCoin         :: Beam.Columnar f Core.Coin
    , _inputTableForeignTxid  :: Beam.Columnar f Txp.TxId
    , _inputTableForeignIndex :: Beam.Columnar f Word32
    } deriving Generic

type TxInput = TxInputT Identity

instance Beamable TxInputT

instance Table TxInputT where
    data PrimaryKey TxInputT f = TxInputPrimKey
                         (Beam.PrimaryKey TxMetaT f)
                         (Beam.Columnar f Txp.TxId)
                         (Beam.Columnar f Word32) deriving Generic
    primaryKey TxInputT{..} = TxInputPrimKey
                         (_inputTableTxId)
                         (_inputTableForeignTxid)
                         (_inputTableForeignIndex)

instance Beamable (PrimaryKey TxInputT)

-- | Convenient constructor of a list of 'TxInput' from a 'Kernel.TxMeta'.
mkInputs :: Kernel.TxMeta -> NonEmpty TxInput
mkInputs Kernel.TxMeta{..}  = toTxInput <$> _txMetaInputs
    where
        toTxInput :: (Core.Address, Core.Coin, Txp.TxId, Word32) -> TxInput
        toTxInput (addr, coin, fTxId, fIndex) =
            TxInputT {
              _inputTableTxId = TxIdPrimKey _txMetaId
            , _inputTableAddress = addr
            , _inputTableCoin = coin
            , _inputTableForeignTxid = fTxId
            , _inputTableForeignIndex = fIndex
            }

fromInputs :: NonEmpty TxInput -> NonEmpty (Core.Address, Core.Coin, Txp.TxId, Word32)
fromInputs ls = go <$> ls
    where
        go TxInputT{..} = (_inputTableAddress, _inputTableCoin,
                          _inputTableForeignTxid, _inputTableForeignIndex)

getTxIdfromInput :: (TxInputT f) -> Beam.Columnar f Txp.TxId
getTxIdfromInput inp =
    let TxIdPrimKey txid = _inputTableTxId inp
    in txid

{--

** Table 3: ~tx_meta_outputs~
** Primary Key: ~output_id~, ~output_index~

output_id   | output_address | output_coin | output_index
------------|----------------+-------------+--------------------
Txp.TxId    | Core.Address   | Core.Coin   | Word32

--}

-- | The outputs' table. Order of fields is important for right Ord instance.
data TxOutputT f = TxOutputT {
          _outputTableTxId    :: Beam.PrimaryKey TxMetaT f
        , _outputTableIndex   :: Beam.Columnar f Word32
        , _outputTableAddress :: Beam.Columnar f Core.Address
        , _outputTableCoin    :: Beam.Columnar f Core.Coin
    } deriving Generic

type TxOutput = TxOutputT Identity

forOrder :: TxOutputT f0
         -> (Beam.Columnar f0 Txp.TxId, Beam.Columnar f0 Word32,
             Beam.Columnar f0 Core.Address, Beam.Columnar f0 Core.Coin)
forOrder t@TxOutputT{..} = (getTxIdfromOutput t, _outputTableIndex, _outputTableAddress, _outputTableCoin)

instance Eq TxOutput where
    a == b = (forOrder a) == (forOrder b)

instance Ord TxOutput where
    compare a b = compare (forOrder a) (forOrder b)

instance Beamable TxOutputT

instance Table TxOutputT where
    data PrimaryKey TxOutputT f = TxOutputPrimKey
                (Beam.PrimaryKey TxMetaT f)
                (Beam.Columnar f Word32) deriving Generic
    primaryKey TxOutputT{..} = TxOutputPrimKey _outputTableTxId _outputTableIndex

instance Beamable (PrimaryKey TxOutputT)

-- | Convenient constructor of a list of 'TxInput' from a 'Kernel.TxMeta'.
-- The list returned should ensure the uniqueness of Indexes.
mkOutputs :: Kernel.TxMeta -> NonEmpty TxOutput
mkOutputs Kernel.TxMeta{..} = toTxOutput <$> NonEmpty.zip _txMetaOutputs (NonEmpty.fromList [0..])
    where
        toTxOutput :: ((Core.Address, Core.Coin), Word32) -> TxOutput
        toTxOutput ((addr, coin), index) =
            TxOutputT {
              _outputTableTxId = TxIdPrimKey _txMetaId
            , _outputTableIndex = index
            , _outputTableAddress = addr
            , _outputTableCoin = coin
            }

-- | The invarint here is that the list of TxOutput should have all the same
-- TxId and include all indexes starting from 0.
fromOutputs :: NonEmpty TxOutput -> NonEmpty (Core.Address, Core.Coin)
fromOutputs ls = go <$> NonEmpty.sort ls
  where
    go TxOutputT {..} = (_outputTableAddress, _outputTableCoin)

getTxIdfromOutput :: (TxOutputT f) -> Beam.Columnar f Txp.TxId
getTxIdfromOutput out =
    let TxIdPrimKey txid = _outputTableTxId out
    in txid

-- Orphans & other boilerplate

instance HasSqlValueSyntax SqliteValueSyntax Txp.TxId where
    sqlValueSyntax txid = sqlValueSyntax (sformat hashHexF txid)

instance HasSqlValueSyntax SqliteValueSyntax Core.Coin where
    sqlValueSyntax = sqlValueSyntax . Core.unsafeGetCoin

-- NOTE(adn) As reported by our good lad Matt Parsons, 'Word64' has enough
-- precision to justify the downcast:
--
-- >>> λ> import Data.Time
-- >>> λ> import Data.Time.Clock.POSIX
-- >>> λ> import Data.Word
-- >>> λ> :set -XNumDecimals
-- >>> λ> posixSecondsToUTCTime (fromIntegral ((maxBound :: Word64) `div` 1e6))
-- 586524-01-19 08:01:49 UTC
--
instance HasSqlValueSyntax SqliteValueSyntax Core.Timestamp where
    sqlValueSyntax ts = sqlValueSyntax (fromIntegral @Integer @Word64 . toMicroseconds . Core.getTimestamp $ ts)

instance HasSqlValueSyntax SqliteValueSyntax Core.Address where
    sqlValueSyntax addr = sqlValueSyntax (sformat Core.addressF addr)

instance HasSqlEqualityCheck SqliteExpressionSyntax Txp.TxId

instance FromField Txp.TxId where
    fromField f = do
        h <- decodeAbstractHash <$> fromField f
        case h of
             Left _     -> returnError Sqlite.ConversionFailed f "not a valid hex hash"
             Right txid -> pure txid

instance FromBackendRow Sqlite Txp.TxId

instance HasSqlEqualityCheck SqliteExpressionSyntax Core.Address

instance HasSqlEqualityCheck SqliteExpressionSyntax Core.Timestamp

instance FromField Core.Coin where
    fromField f = Core.Coin <$> fromField f

instance FromBackendRow Sqlite Core.Coin

instance FromField Core.Timestamp where
    fromField f = Core.Timestamp . fromMicroseconds . toInteger @Word64 <$> fromField f

instance FromBackendRow Sqlite Core.Timestamp

instance FromField Core.Address where
    fromField f = do
        addr <- Core.decodeTextAddress <$> fromField f
        case addr of
           Left _  -> returnError Sqlite.ConversionFailed f "not a valid Address"
           Right a -> pure a

instance FromBackendRow Sqlite Core.Address


-- | Creates new 'DatabaseSettings' for the 'MetaDB', locking the backend to
-- be 'Sqlite'.
metaDB :: DatabaseSettings Sqlite MetaDB
metaDB = unCheckDatabase (evaluateDatabase migrateMetaDB)

-- | 'DataType' declaration to convince @Beam@ treating 'Core.Address'(es) as
-- varchars of arbitrary length.
addressDT :: DataType SqliteDataTypeSyntax Core.Address
addressDT = DataType (varCharType Nothing Nothing)

-- | 'DataType' declaration to convince @Beam@ treating 'Core.Timestamp'(s) as
-- SQLite BIG INTEGER.
timestampDT :: DataType SqliteDataTypeSyntax Core.Timestamp
timestampDT = DataType sqliteBigIntType

-- | 'DataType' declaration to convince @Beam@ treating 'Txp.TxId(s) as
-- varchars of arbitrary length.
txIdDT :: IsSql92DataTypeSyntax syntax => DataType syntax Txp.TxId
txIdDT = DataType (varCharType Nothing Nothing)

-- | 'DataType' declaration to convince @Beam@ treating 'Core.Coin(s) as
-- SQLite BIG INTEGER.
coinDT :: DataType SqliteDataTypeSyntax Core.Coin
coinDT = DataType sqliteBigIntType

outputIndexDT :: DataType SqliteDataTypeSyntax Word32
outputIndexDT = DataType intType

walletidDT :: DataType SqliteDataTypeSyntax Core.Address
walletidDT = DataType (varCharType Nothing Nothing)

accountixDT :: DataType SqliteDataTypeSyntax Word32
accountixDT = DataType intType

-- | Beam's 'Migration' to create a new 'MetaDB' Sqlite database.
initialMigration :: () -> Migration SqliteCommandSyntax (CheckedDatabaseSettings Sqlite MetaDB)
initialMigration () = do
    MetaDB <$> createTable "tx_metas"
                 (TxMeta (field "meta_id" txIdDT notNull unique)
                         (field "meta_amount" coinDT notNull)
                         (field "meta_created_at" timestampDT notNull)
                         (field "meta_is_local" boolean notNull)
                         (field "meta_is_outgoing" boolean notNull)
                         (field "meta_wallet_id" walletidDT notNull)
                         (field "meta_account_ix" accountixDT notNull))
           <*> createTable "tx_metas_inputs"
                 (TxInputT (TxIdPrimKey (field "meta_id" txIdDT notNull))
                                                   (field "input_address" addressDT notNull)
                                                   (field "input_coin" coinDT notNull)
                                                   (field "input_foreign_id" txIdDT notNull)
                                                   (field "input_foreign_index" outputIndexDT notNull)
                          )
           <*> createTable "tx_metas_outputs"
                 (TxOutputT (TxIdPrimKey (field "meta_id" txIdDT notNull))
                                                    (field "output_index" outputIndexDT notNull)
                                                    (field "output_address" addressDT notNull)
                                                    (field "output_coin" coinDT notNull)

                           )

--- | The full list of migrations available for this 'MetaDB'.
-- For a more interesting migration, see: https://github.com/tathougies/beam/blob/d3baf0c77b76b008ad34901b47a818ea79439529/beam-postgres/examples/Pagila/Schema.hs#L17-L19
migrateMetaDB :: MigrationSteps SqliteCommandSyntax () (CheckedDatabaseSettings Sqlite MetaDB)
migrateMetaDB = migrationStep "Initial migration" initialMigration

-- | Migrates the 'MetaDB', potentially mangling the input database.
-- TODO(adinapoli): Make it safe.
unsafeMigrateMetaDB :: Sqlite.Connection -> IO ()
unsafeMigrateMetaDB conn = do
    void $ runMigrationSteps 0 Nothing migrateMetaDB (\_ _ -> executeMigration (Sqlite.execute_ conn . newSqlQuery))
    -- We don`t add Indexes on tx_metas (meta_id) because it`s unecessary for the PrimaryKey.
    Sqlite.execute_ conn "CREATE INDEX meta_created_at ON tx_metas (meta_created_at)"
    Sqlite.execute_ conn "CREATE INDEX meta_query ON tx_metas (meta_wallet_id, meta_account_ix, meta_id, meta_created_at)"
    Sqlite.execute_ conn "CREATE INDEX inputs_address ON tx_metas_inputs (input_address)"
    Sqlite.execute_ conn "CREATE INDEX outputs_address ON tx_metas_outputs (output_address)"
    where
        newSqlQuery :: SqliteCommandSyntax -> Sqlite.Query
        newSqlQuery syntax =
            let sqlFragment = sqliteRenderSyntaxScript . fromSqliteCommand $ syntax
                in Sqlite.Query (decodeUtf8 sqlFragment)

-- | Simply a conveniency wrapper to avoid 'Kernel.TxMeta' to explicitly
-- import Sqlite modules.
newConnection :: FilePath -> IO Sqlite.Connection
newConnection path = Sqlite.open path

-- | Closes an open 'Connection' to the @Sqlite@ database stored in the
-- input 'MetaDBHandle'.
-- Even if open failed with error, this function should be called http://www.sqlite.org/c3ref/open.html
-- TODO: provide a bracket style interface to ensure this.
closeMetaDB :: Sqlite.Connection -> IO ()
closeMetaDB = Sqlite.close

-- | Inserts a new 'Kernel.TxMeta' in the database, given its opaque
-- 'MetaDBHandle'.
putTxMeta :: Sqlite.Connection -> Kernel.TxMeta -> IO ()
putTxMeta conn txMeta =
    let tMeta   = mkTxMeta txMeta
        inputs  = mkInputs txMeta
        outputs = mkOutputs txMeta
    in do
        res <- Sqlite.withTransaction conn $ Sqlite.runDBAction $ runBeamSqlite conn $ do
            SQL.runInsert $ SQL.insert (_mDbMeta metaDB)    $ SQL.insertValues [tMeta]
            SQL.runInsert $ SQL.insert (_mDbInputs metaDB)  $ SQL.insertValues (NonEmpty.toList inputs)
            SQL.runInsert $ SQL.insert (_mDbOutputs metaDB) $ SQL.insertValues (NonEmpty.toList outputs)
        case res of
             Left e   -> handleResponse e
             Right () -> return ()
    where
        -- Handle the 'SQLiteResponse', rethrowing the exception or turning
        -- \"controlled failures\" (like the presence of a duplicated
        -- transaction) in a no-op.
        handleResponse :: Sqlite.SQLiteResponse -> IO ()
        handleResponse e =
            let txid  = txMeta ^. Kernel.txMetaId
            in case e of
                -- NOTE(adinapoli): It's probably possible to make this match the
                -- Beam schema by using something like 'IsDatabaseEntity' from
                -- 'Database.Beam.Schema.Tables', but we have a test to catch
                -- regression in this area.
                (Sqlite.SQLConstraintError Sqlite.Unique "tx_metas_inputs.meta_id, tx_metas_inputs.input_foreign_id, tx_metas_inputs.input_foreign_index") -> do
                   let err = Kernel.DuplicatedInputIn txid
                   throwIO $ Kernel.InvariantViolated err

                (Sqlite.SQLConstraintError Sqlite.Unique "tx_metas_outputs.meta_id, tx_metas_outputs.output_index") -> do
                   let err = Kernel.DuplicatedOutputIn txid
                   throwIO $ Kernel.InvariantViolated err

                (Sqlite.SQLConstraintError Sqlite.Unique "tx_metas.meta_id") -> do
                    -- Check if the 'TxMeta' already present is a @different@
                    -- one, in which case this is a proper bug there is no
                    -- recover from. If the 'TxMeta' is the same, this is effectively
                    -- a no-op.
                    -- NOTE: We use a shallow equality check here because if the
                    -- input 'TxMeta' has the same inputs & outputs but in a different
                    -- order, the 'consistencyCheck' would yield 'False', when in
                    -- reality should be virtually considered the same 'TxMeta'.
                    consistencyCheck <- fmap (Kernel.isomorphicTo txMeta) <$> getTxMeta conn txid
                    case consistencyCheck of
                         Nothing    ->
                             throwIO $ Kernel.InvariantViolated (Kernel.UndisputableLookupFailed "consistencyCheck" txid)
                         Just False ->
                             throwIO $ Kernel.InvariantViolated (Kernel.DuplicatedTransactionWithDifferentHash txid)
                         Just True  ->
                             -- both "hashes" matched, this is genuinely the
                             -- same 'Tx' being inserted twice, probably as
                             -- part of a rollback.
                             return ()

                _ -> throwIO $ Kernel.StorageFailure (toException e)

-- | Converts a database-fetched 'TxMeta' into a domain-specific 'Kernel.TxMeta'.
toTxMeta :: TxMeta -> NonEmpty TxInput -> NonEmpty TxOutput -> Kernel.TxMeta
toTxMeta TxMeta{..} inputs outputs = Kernel.TxMeta {
      _txMetaId         = _txMetaTableId
    , _txMetaAmount     = _txMetaTableAmount
    , _txMetaInputs     = fromInputs inputs
    , _txMetaOutputs    = fromOutputs outputs
    , _txMetaCreationAt = _txMetaTableCreatedAt
    , _txMetaIsLocal    = _txMetaTableIsLocal
    , _txMetaIsOutgoing = _txMetaTableIsOutgoing
    , _txMetaWalletId   = _txMetaTableWalletId
    , _txMetaAccountIx  = _txMetaTableAccountIx
    }

-- | Fetches a 'Kernel.TxMeta' from the database, given its 'Txp.TxId'.
getTxMeta :: Sqlite.Connection -> Txp.TxId -> IO (Maybe Kernel.TxMeta)
getTxMeta conn txid = do
    res <- Sqlite.runDBAction $ runBeamSqlite conn $ do
        metas <- SQL.runSelectReturningList txMetaById
        case metas of
            [txMeta] -> do
                inputs  <- nonEmpty <$> SQL.runSelectReturningList getInputs
                outputs <- nonEmpty <$> SQL.runSelectReturningList getOutputs
                pure $ toTxMeta <$> Just txMeta <*> inputs <*> outputs
            _        -> pure Nothing
    case res of
         Left e  -> throwIO $ Kernel.StorageFailure (toException e)
         Right r -> return r
    where
        txMetaById = SQL.lookup_ (_mDbMeta metaDB) (TxIdPrimKey txid)
        getInputs  = SQL.select $ do
            txInput <- SQL.all_ (_mDbInputs metaDB)
            SQL.guard_ ((_inputTableTxId txInput) ==. (SQL.val_ $ TxIdPrimKey txid))
            pure txInput
        getOutputs = SQL.select $ do
            txOutput <- SQL.all_ (_mDbOutputs metaDB)
            SQL.guard_ ((_outputTableTxId txOutput) ==. (SQL.val_ $ TxIdPrimKey txid))
            pure txOutput

getTxMetas :: Sqlite.Connection
           -> Offset
           -> Limit
           -> AccountFops
           -> Maybe Core.Address
           -> FilterOperation Txp.TxId
           -> FilterOperation Core.Timestamp
           -> Maybe Sorting
           -> IO ([Kernel.TxMeta], Maybe Int)
getTxMetas conn (Offset offset) (Limit limit) accountFops mbAddress fopTxId fopTimestamp mbSorting = do
    res <- Sqlite.runDBAction $ runBeamSqlite conn $ do

        -- The following 3 queries are disjointed and both fetches, respectively,
        -- @TxMeta@ @TxInput@ and @TxOutput@.
        -- The rationale behind doing three separate queries is that SQlite does not support
        -- array types, neither array aggregations (https://www.sqlite.org/lang_aggfunc.html).
        -- This in particular means the list of Inputs and Outputs for each TxId must be assembled
        -- in memory. One workarroun is to use group_concat(see link above), but this would return
        -- the array in a string format, separated by commas.

        -- The length of meta list is bounded by 50 in each realistic senario.
        -- So the marshalling between Haskell types - UTF8 - binary SQlite types shouldn`t
        -- be costly.
        meta <- SQL.runSelectReturningList $ SQL.select $ do
            case mbAddress of
                Nothing   -> SQL.limit_ limit $ SQL.offset_ offset $ metaQuery
                Just addr -> SQL.limit_ limit $ SQL.offset_ offset $ metaQueryWithAddr addr
        let txids = map (SQL.val_ . _txMetaTableId) meta
        input <- SQL.runSelectReturningList $ SQL.select $ do
                input <- SQL.all_ $ _mDbInputs metaDB
                let txid = getTxIdfromInput input
                SQL.guard_ $ in_ txid txids
                pure input
        output <- SQL.runSelectReturningList $ SQL.select $ do
                output <- SQL.all_ $ _mDbOutputs metaDB
                let txid = getTxIdfromOutput output
                SQL.guard_ $ in_ txid txids
                pure output
        return $ do
             mt  <- nonEmpty meta
             inp <- nonEmpty input
             out <- nonEmpty output
             return (mt, inp, out)

    case res of
        Left e -> throwIO $ Kernel.StorageFailure (toException e)
        Right Nothing -> return ([], Just 0)
        Right (Just (meta, inputs, outputs)) ->  do
            eiCount <- limitExecutionTimeTo (25 :: Second) (\ _ -> ()) $ ignoreLeft $ Sqlite.runDBAction $ runBeamSqlite conn $
                case mbAddress of
                    Nothing   -> SQL.runSelectReturningOne $ SQL.select metaQueryC
                    Just addr -> SQL.runSelectReturningOne $ SQL.select $ metaQueryWithAddrC addr
            let mapWithInputs  = transform $ map (\inp -> (getTxIdfromInput inp, inp)) inputs
            let mapWithOutputs = transform $ map (\out -> (getTxIdfromOutput out, out)) outputs
            let txMeta = toValidKernelTxMeta mapWithInputs mapWithOutputs $ NonEmpty.toList meta
                count = case eiCount of
                    Left _  -> Nothing
                    Right c -> c
            return (txMeta, count)

    where
        ignoreLeft :: IO (Either a b) -> IO (Either () b)
        ignoreLeft m  = do
            x <- m
            case x of
                Left _  -> return $ Left ()
                Right r -> return $ Right r

        filters meta = do
            SQL.guard_ $ filterAccs meta accountFops
            SQL.guard_ $ applyFilter (_txMetaTableId meta) fopTxId
            SQL.guard_ $ applyFilter (_txMetaTableCreatedAt meta) fopTimestamp
            pure ()

        filtersC meta = do
            SQL.guard_ $ filterAccs meta accountFops
            SQL.guard_ $ applyFilter (_txMetaTableId meta) fopTxId
            SQL.guard_ $ applyFilter (_txMetaTableCreatedAt meta) fopTimestamp
            pure ()

        metaQuery = do
            let query = SQL.all_ $ _mDbMeta metaDB
            meta <- case mbSorting of
                    Nothing -> query
                    Just (Sorting SortByCreationAt dir) ->
                        SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableCreatedAt) query
                    Just (Sorting SortByAmount     dir) ->
                        SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableAmount) query
            filters meta
            return meta

        metaQueryC = SQL.aggregate_ (\_ -> SQL.as_ SQL.countAll_) $ do
            meta <-  SQL.all_ $ _mDbMeta metaDB
            filtersC meta
            return meta

        findAndUnion addr = do
                let input = do
                        inp <- SQL.all_ $ _mDbInputs metaDB
                        SQL.guard_ $ ((_inputTableAddress inp) ==. (SQL.val_ addr))
                        pure $ _inputTableTxId inp
                let output = do
                        out <- SQL.all_ $ _mDbOutputs metaDB
                        SQL.guard_ $ ((_outputTableAddress out) ==. (SQL.val_ addr))
                        pure $ _outputTableTxId out
                -- union removes txId duplicates.
                txid <- SQL.union_ input output
                SQL.join_ (_mDbMeta metaDB)
                    (\ mt -> ((TxIdPrimKey $ _txMetaTableId mt) ==. txid))

        metaQueryWithAddr addr = do
            meta <- case mbSorting of
                Nothing -> findAndUnion addr
                Just (Sorting SortByCreationAt dir) ->
                    SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableCreatedAt) (findAndUnion addr)
                Just (Sorting SortByAmount     dir) ->
                    SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableAmount) (findAndUnion addr)
            filters meta
            return meta

        metaQueryWithAddrC addr = SQL.aggregate_ (\_ -> SQL.as_ SQL.countAll_) $ do
            meta <- findAndUnion addr
            filtersC meta
            pure meta

        filterAccs _ Everything = SQL.QExpr (pure (valueE (sqlValueSyntax True)))
        filterAccs meta (AccountFops rootAddr (mbAccountIx)) =
            (_txMetaTableWalletId meta ==. SQL.val_ rootAddr) &&.
                case mbAccountIx of
                    Nothing -> SQL.QExpr (pure (valueE (sqlValueSyntax True)))
                    Just accountIx -> _txMetaTableAccountIx meta ==. SQL.val_ accountIx

        applyFilter inputData fop =
            let byPredicate o i = case o of
                    Kernel.Equal            -> inputData ==. i
                    Kernel.LesserThan       -> inputData <. i
                    Kernel.GreaterThan      -> inputData >. i
                    Kernel.LesserThanEqual  -> inputData <=. i
                    Kernel.GreaterThanEqual -> inputData >=. i
            in case fop of
                NoFilterOp -> SQL.val_ True
                FilterByIndex a -> byPredicate Kernel.Equal (SQL.val_ a)
                FilterByPredicate ford a -> byPredicate ford (SQL.val_ a)
                FilterByRange from to -> between_ inputData (SQL.val_ from) (SQL.val_ to)
                FilterIn ls -> in_ inputData (map SQL.val_ ls)

        transform :: NonEmpty (Txp.TxId, a) -> M.Map Txp.TxId (NonEmpty a)
        transform = Foldable.foldl' updateFn M.empty

        updateFn :: M.Map Txp.TxId (NonEmpty a)
                -> (Txp.TxId, a)
                -> M.Map Txp.TxId (NonEmpty a)
        updateFn acc (txid, new) =
            M.insertWith (<>) txid (new :| []) acc

        toValidKernelTxMeta :: M.Map Txp.TxId (NonEmpty TxInput)
                             -> M.Map Txp.TxId (NonEmpty TxOutput)
                             -> [TxMeta]
                             -> [Kernel.TxMeta]
        toValidKernelTxMeta _ _ [] = []
        toValidKernelTxMeta inputMap outputMap (m : meta) =
            let txid = _txMetaTableId m
                io = do
                    inp <- M.lookup txid inputMap
                    out <- M.lookup txid outputMap
                    pure (inp, out)
            in case io of
                Nothing -> toValidKernelTxMeta inputMap outputMap meta
                Just (inputs, outputs) -> toTxMeta m inputs outputs : toValidKernelTxMeta inputMap outputMap meta

-- | Generates a Beam's AST fragment for use within a SQL query, to order
-- the results of a @SELECT@.
toBeamSortDirection :: SortDirection
                    -> SQL.QExpr (Sql92OrderingExpressionSyntax (Sql92SelectOrderingSyntax SqliteSelectSyntax)) s a
                    -> SQL.QOrd (Sql92SelectOrderingSyntax SqliteSelectSyntax) s a
toBeamSortDirection Ascending  = SQL.asc_
toBeamSortDirection Descending = SQL.desc_
