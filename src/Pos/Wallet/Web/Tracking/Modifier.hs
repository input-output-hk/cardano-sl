{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Wallet info modifier

module Pos.Wallet.Web.Tracking.Modifier
       ( CAccModifier (..)
       , sortedInsertions

       , syncWalletsWithGState
       , trackingApplyTxs
       , trackingRollbackTxs
       , applyModifierToWallet
       , rollbackModifierFromWallet
       , BlockLockMode
       , MonadWalletTracking (..)

       , syncWalletOnImportWebWallet
       , txMempoolToModifierWebWallet

       , CachedCAccModifier
       , fixingCachedAccModifier
       , fixCachedAccModifierFor

       , getWalletAddrMetasDB
       ) where

import           Universum
import           Unsafe                     (unsafeLast)

import           Control.Lens               (to)
import           Control.Monad.Catch        (handleAll)
import           Control.Monad.Trans        (MonadTrans)
import           Data.DList                 (DList)
import qualified Data.DList                 as DL
import qualified Data.HashMap.Strict        as HM
import           Data.List                  ((!!))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Text.Buildable
import           Ether.Internal             (HasLens (..))
import           Formatting                 (bprint, build, sformat, (%))
import           Mockable                   (SharedAtomicT)
import           Serokell.Util              (listJson, listJsonIndent)
import           System.Wlog                (HasLoggerName, WithLogger, logError, logInfo,
                                             logWarning, modifyLoggerName)

import           Pos.Block.Core             (BlockHeader, getBlockHeader,
                                             mainBlockTxPayload)
import           Pos.Block.Logic            (withBlkSemaphore_)
import           Pos.Block.Types            (Blund, undoTx)
import           Pos.Client.Txp.History     (TxHistoryEntry (..))
import           Pos.Constants              (blkSecurityParam, genesisHash)
import           Pos.Context                (BlkSemaphore, GenesisUtxo (..), genesisUtxoM)
import           Pos.Core                   (AddrPkAttrs (..), Address (..),
                                             BlockHeaderStub, ChainDifficulty,
                                             HasDifficulty (..), HeaderHash, Timestamp,
                                             headerHash, headerSlotL, makePubKeyAddress)
import           Pos.Crypto                 (EncryptedSecretKey, HDPassphrase,
                                             WithHash (..), deriveHDPassphrase,
                                             encToPublic, hash, shortHashF,
                                             unpackHDAddressAttr)
import           Pos.Data.Attributes        (Attributes (..))
import qualified Pos.DB.Block               as DB
import qualified Pos.DB.DB                  as DB
import           Pos.DB.Error               (DBError (DBMalformed))
import           Pos.DB.Rocks               (MonadRealDB)
import           Pos.GState.BlockExtra      (foldlUpWhileM, resolveForwardLink)
import           Pos.Slotting               (MonadSlotsData (..), getSlotStartPure)
import           Pos.Txp.Core               (Tx (..), TxAux (..), TxId, TxIn (..),
                                             TxOutAux (..), TxUndo, flattenTxPayload,
                                             getTxDistribution, toaOut, topsortTxs,
                                             txOutAddress)
import           Pos.Txp.MemState.Class     (MonadTxpMem, getLocalTxsNUndo)
import           Pos.Txp.Toil               (UtxoModifier)
import           Pos.Util.Chrono            (getNewestFirst)
import           Pos.Util.Modifier          (MapModifier)
import qualified Pos.Util.Modifier          as MM
import           Pos.Util.Util              (maybeThrow)

import           Pos.Ssc.Class              (SscHelpersClass)
import           Pos.Wallet.SscType         (WalletSscType)
import           Pos.Wallet.Web.Account     (MonadKeySearch (..))
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId,
                                             CWAddressMeta (..), Wal, addressToCId, aiWId,
                                             encToCId, isTxLocalAddress)
import           Pos.Wallet.Web.Error.Types (WalletError (..))
import           Pos.Wallet.Web.State       (AddressLookupMode (..),
                                             CustomAddressType (..), WalletTip (..),
                                             WebWalletModeDB)
import qualified Pos.Wallet.Web.State       as WS

-- VoidModifier describes a difference between two states.
-- It's (set of added k, set of deleted k) essentially.
type VoidModifier a = MapModifier a ()

data IndexedMapModifier a = IndexedMapModifier
    { immModifier :: MM.MapModifier a Int
    , immCounter  :: Int
    }

sortedInsertions :: IndexedMapModifier a -> [a]
sortedInsertions = map fst . sortWith snd . MM.insertions . immModifier

indexedDeletions :: IndexedMapModifier a -> [a]
indexedDeletions = MM.deletions . immModifier

instance (Eq a, Hashable a) => Monoid (IndexedMapModifier a) where
    mempty = IndexedMapModifier mempty 0
    IndexedMapModifier m1 c1 `mappend` IndexedMapModifier m2 c2 =
        IndexedMapModifier (m1 <> fmap (+ c1) m2) (c1 + c2)

data CAccModifier = CAccModifier
    { camAddresses      :: !(IndexedMapModifier CWAddressMeta)
    , camUsed           :: !(VoidModifier (CId Addr, HeaderHash))
    , camChange         :: !(VoidModifier (CId Addr, HeaderHash))
    , camUtxo           :: !UtxoModifier
    , camAddedHistory   :: !(DList TxHistoryEntry)
    , camDeletedHistory :: !(DList TxId)
    }

instance Monoid CAccModifier where
    mempty = CAccModifier mempty mempty mempty mempty mempty mempty
    (CAccModifier a b c d ah dh) `mappend` (CAccModifier a1 b1 c1 d1 ah1 dh1) =
        CAccModifier (a <> a1) (b <> b1) (c <> c1) (d <> d1) (ah1 <> ah) (dh <> dh1)

instance Buildable CAccModifier where
    build CAccModifier{..} =
        bprint
            ( "\n    added addresses: "%listJsonIndent 8
            %",\n    deleted addresses: "%listJsonIndent 8
            %",\n    used addresses: "%listJson
            %",\n    change addresses: "%listJson
            %",\n    local utxo (difference): "%build
            %",\n    added history entries: "%listJsonIndent 8
            %",\n    deleted history entries: "%listJsonIndent 8)
        (sortedInsertions camAddresses)
        (indexedDeletions camAddresses)
        (map (fst . fst) $ MM.insertions camUsed)
        (map (fst . fst) $ MM.insertions camChange)
        camUtxo
        camAddedHistory
        camDeletedHistory

----------------------------------------------------------------------------
-- Cached modifier
----------------------------------------------------------------------------

-- | `txMempoolToModifier`, once evaluated, is passed around under this type in
-- scope of single request.
type CachedCAccModifier = CAccModifier

-- | Evaluates `txMempoolToModifier` and provides result as a parameter
-- to given function.
fixingCachedAccModifier
    :: (MonadWalletTracking m, MonadKeySearch key m)
    => (CachedCAccModifier -> key -> m a)
    -> key -> m a
fixingCachedAccModifier action key =
    findKey key >>= txMempoolToModifier >>= flip action key

fixCachedAccModifierFor
    :: (MonadWalletTracking m, MonadKeySearch key m)
    => key
    -> (CachedCAccModifier -> m a)
    -> m a
fixCachedAccModifierFor key action =
    fixingCachedAccModifier (const . action) key

----------------------------------------------------------------------------
-- Funcs
----------------------------------------------------------------------------

insertIMM
    :: (Eq a, Hashable a)
    => a -> IndexedMapModifier a -> IndexedMapModifier a
insertIMM k IndexedMapModifier {..} =
    IndexedMapModifier
    { immModifier = MM.insert k immCounter immModifier
    , immCounter  = immCounter + 1
    }

deleteIMM
    :: (Eq a, Hashable a)
    => a -> IndexedMapModifier a -> IndexedMapModifier a
deleteIMM k IndexedMapModifier {..} =
    IndexedMapModifier
    { immModifier = MM.delete k immModifier
    , ..
    }

deleteAndInsertIMM
    :: (Eq a, Hashable a)
    => [a] -> [a] -> IndexedMapModifier a -> IndexedMapModifier a
deleteAndInsertIMM dels ins mapModifier =
    -- Insert CWAddressMeta coressponding to outputs of tx.
    (\mm -> foldl' (flip insertIMM) mm ins) $
    -- Delete CWAddressMeta coressponding to inputs of tx.
    foldl' (flip deleteIMM) mapModifier dels

deleteAndInsertVM :: (Eq a, Hashable a) => [a] -> [a] -> VoidModifier a -> VoidModifier a
deleteAndInsertVM dels ins mapModifier = deleteAndInsertMM dels (zip ins $ repeat ()) mapModifier

deleteAndInsertMM :: (Eq k, Hashable k) => [k] -> [(k, v)] -> MM.MapModifier k v -> MM.MapModifier k v
deleteAndInsertMM dels ins mapModifier =
    -- Insert CWAddressMeta coressponding to outputs of tx (2)
    (\mm -> foldl' insertAcc mm ins) $
    -- Delete CWAddressMeta coressponding to inputs of tx (1)
    foldl' deleteAcc mapModifier dels
  where
    insertAcc :: (Hashable k, Eq k) => MapModifier k v -> (k, v) -> MapModifier k v
    insertAcc modifier (k, v) = MM.insert k v modifier

    deleteAcc :: (Hashable k, Eq k) => MapModifier k v -> k -> MapModifier k v
    deleteAcc = flip MM.delete

