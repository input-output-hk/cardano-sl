{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Kernel.Restore
 ( initWalletRestoration
 ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Acid (AcidState)
import Data.Acid.Advanced (update')
import qualified Data.Map as M
import System.Wlog (WithLogger, logInfo, modifyLoggerName)
import Universum

import Pos.Core (ChainDifficulty, difficultyL)
import Pos.Core.Block (headerHash)
import Pos.DB.BlockIndex (getTipHeader)
import Pos.DB.Class (MonadDBRead)
import Pos.DB.Txp.Utxo (filterUtxo)
import Pos.Txp (TxIn, TxOutAux, toaOut, txOutAddress, genesisUtxo)
import Pos.Txp.Toil.Types (GenesisUtxo, unGenesisUtxo, utxoToModifier)
import Pos.Wallet.Web.Tracking.Decrypt (decryptAddress)
import Pos.Wallet.Web.Tracking.Sync (firstGenesisHeader, processSyncError)
import Pos.Wallet.Web.Tracking.Types (SyncError)

import Cardano.Wallet.Kernel.DB.AcidState (CreateHdAddress(..),
    UpdateCurrentCheckpointUtxo(..), DB, SetWalletRestorationSyncTip(..))
import Cardano.Wallet.Kernel.DB.HdWallet (UnknownHdRoot)
import Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdAddress,
    CreateHdAddressError)
import Cardano.Wallet.Kernel.DB.InDb (InDb(..))
import Cardano.Wallet.Kernel.MonadDBReadAdaptor (MonadDBReadAdaptor,
    withMonadDBRead)
import Cardano.Wallet.Kernel.PrefilterTx (WalletKey, prefilter, toHdAddressId)
import Cardano.Wallet.Kernel.Types (WalletId(WalletIdHdRnd))

--------------------------------------------------------------------------------

data Err_InitWalletRestoration
  = Err_InitWalletRestoration_CreateHdAddressError !CreateHdAddressError
  | Err_InitWalletRestoration_SyncError !SyncError
  | Err_InitWalletRestoration_DecryptAddress
  | Err_InitWalletRestoration_UnknownHdRoot !UnknownHdRoot

initWalletRestoration
  :: forall m
  .  (WithLogger m, MonadUnliftIO m, MonadCatch m)
  => MonadDBReadAdaptor m
  -> AcidState DB
  -> WalletKey
  -> m (Either Err_InitWalletRestoration ChainDifficulty)
  -- ^ Block depth from where the wallet worker should start restoring.
initWalletRestoration mdbra db wkey@(wId,_) = runExceptT $ do
  gu <- lift (withMonadDBRead mdbra (pure genesisUtxo))
  modifyLoggerName (const "initWalletRestoration") $ do
     logInfo "New Restoration request for a wallet..."
     lift (withMonadDBRead mdbra firstGenesisHeader) >>= \case
        Left se -> do
           processSyncError se
           ExceptT (pure (Left (Err_InitWalletRestoration_SyncError se)))
        Right genesisBlock -> do
           ExceptT (fmap (bimap Err_InitWalletRestoration_CreateHdAddressError id)
                         (liftIO (restoreGenesisAddresses gu db wkey)))
           ExceptT (fmap (bimap (const Err_InitWalletRestoration_DecryptAddress) id)
                         (withMonadDBRead mdbra (restoreWalletBalance db wkey)))
           bdepth <- fmap (view difficultyL)
                          (lift (withMonadDBRead mdbra getTipHeader))
           let WalletIdHdRnd hdrId = wId
           ExceptT (fmap (bimap Err_InitWalletRestoration_UnknownHdRoot id)
                         (update' db (SetWalletRestorationSyncTip hdrId
                                        bdepth (headerHash genesisBlock))))
           pure bdepth

---
restoreGenesisAddresses
  :: GenesisUtxo -> AcidState DB -> WalletKey
  -> IO (Either CreateHdAddressError ())
restoreGenesisAddresses gu db wkey = do
    let xs = map (\(a, b) -> (a, txOutAddress (toaOut b)))
                 (M.toList (unGenesisUtxo gu))
    runExceptT $ for_ (prefilter wkey snd xs) $ \((_,a),aId) -> do
        ExceptT $ update' db (CreateHdAddress (initHdAddress aId (InDb a)))

---
data Err_DecryptAddress = Err_DecryptAddress

restoreWalletBalance
  :: (MonadDBRead m, MonadUnliftIO m)
  => AcidState DB
  -> WalletKey
  -> m (Either Err_DecryptAddress ())
restoreWalletBalance db (wId, wdc) = runExceptT $ do
    utxo <- lift (filterUtxo isWalletUtxo)
    for_ (M.elems utxo) $ \toa -> do
       let a = txOutAddress (toaOut toa)
       case decryptAddress wdc a of
          Nothing -> ExceptT (pure (Left Err_DecryptAddress))
          Just wam -> do
             let aId = toHdAddressId wId wam
             update' db (CreateHdAddress (initHdAddress aId (InDb a)))
    update' db (UpdateCurrentCheckpointUtxo (utxoToModifier utxo))
  where
    isWalletUtxo :: (TxIn, TxOutAux) -> Bool
    isWalletUtxo (_, toa) =
      isJust (decryptAddress wdc (txOutAddress (toaOut toa)))
