module Cardano.Wallet.API.V1.LegacyHandlers.Restore
  ( restoreWalletFromSeed
  ) where


import System.Wlog (Severity)
import Universum

import qualified Pos.Core as Core
import Pos.Crypto (PassPhrase)
import Pos.Wallet.Web.ClientTypes
  (CWallet (..), CWalletInit (..), CWalletMeta (..), mkCCoin,
   CWalletAssurance(CWANormal, CWAStrict), CHash(CHash), CId(CId))
import qualified Pos.Wallet.Web.Methods.Logic as L

import Cardano.Wallet.API.V1.Types (V1(V1), unV1)
import qualified Cardano.Wallet.API.V1.Types as V1T
import Cardano.Wallet.WalletLayer (bracketKernelPassiveWallet)
import Cardano.Wallet.WalletLayer.Types (PassiveWalletLayer(_pwlCreateWallet))


{- | Restores a wallet from a seed. The process is conceptually divided into
-- two parts:
-- 1. Recover this wallet balance from the global Utxo (fast, and synchronous);
-- 2. Recover the full transaction history from the blockchain (slow, asynchronous).
-}
restoreWalletFromSeed
  :: L.MonadWalletLogic ctx m
  => (Severity -> Text -> IO ()) -- ^ Logging function.
  -> PassPhrase
  -> CWalletInit
  -> m CWallet
restoreWalletFromSeed logf passphrase cwInit = do
  let cWM = cwInitMeta cwInit :: CWalletMeta
  let nw = V1T.NewWallet
           { V1T.newwalBackupPhrase = V1 (cwBackupPhrase cwInit)
           , V1T.newwalSpendingPassword = Just (V1 passphrase)
           , V1T.newwalAssuranceLevel = case cwAssurance cWM of
                CWANormal -> V1T.NormalAssurance
                CWAStrict -> V1T.StrictAssurance
           , V1T.newwalName = cwName cWM
           , V1T.newwalOperation = V1T.RestoreWallet }
  bracketKernelPassiveWallet logf $ \pwl -> do
     w <- _pwlCreateWallet pwl nw

     pure (CWallet
        { cwId = case V1T.walId w of V1T.WalletId x -> CId (CHash x)
        , cwMeta = cWM
        , cwAccountsNumber = 0 -- TODO: Is this OK?
        , cwAmount = mkCCoin (unV1 (V1T.walBalance w))
        , cwHasPassphrase = V1T.walHasSpendingPassword w
        , cwPassphraseLU = view Core.timestampSeconds
                                (unV1 (V1T.walSpendingPasswordLastUpdate w)) })

-- restoreWallet :: ( L.MonadWalletLogic ctx m
--                  , MonadUnliftIO m
--                  , HasLens SyncQueue ctx SyncQueue
--                  ) => EncryptedSecretKey -> m CWallet
-- restoreWallet sk = do
--     db <- WS.askWalletDB
--     let credentials@(_, wId) = eskToWalletDecrCredentials sk
--     Restore.restoreWallet credentials
--     WS.setWalletReady db wId True
--     L.getWallet wId

-- | Restores a wallet from seed, by synchronously restoring its balance (and the initial address
-- set) from the global UTXO, and then asynchronously restoring this wallet transaction history.
restoreWallet
  :: (Severity -> Text -> IO ()) -- ^ Logging function.
  -> HdWallets
  -> EncryptedSecretKey
  -> V1T.WalletId
  -> m ()
restoreWallet logf db esk wId = do
        logf Debug "New Restoration request for a wallet..."
        genesisBlockHeaderE <- firstGenesisHeader
        case genesisBlockHeaderE of
            Left syncError -> processSyncError syncError
            Right genesisBlock -> do
                restoreGenesisAddresses db credentials
                restoreWalletBalance db credentials
                -- At this point, we consider ourselves synced with the UTXO up-to the
                -- 'RestorationBlockDepth' we compute now. During 'syncWalletWithBlockchain',
                -- we will restore the wallet history from the beginning of the chain by ignoring
                -- any Utxo changes, but we will always add transactions to the pool of known ones.
                -- By doing so, the BListener is free to track new blocks (both in terms of balance update
                -- & tx tracking), allowing the user to use the wallet even if is technically restoring.
                restorationBlockDepth <- WS.RestorationBlockDepth . view difficultyL <$> DB.getTipHeader

                -- Mark this wallet as officially in restore. As soon as we will pass the point where
                -- the 'RestorationBlockDepth' is greater than the current store one, we would flip the
                -- state of this wallet to a "normal" sync, and the two paths will be reunited once for all.
                setWalletRestorationSyncTip db walletId restorationBlockDepth (headerHash genesisBlock)

                -- Once we have a consistent update of the model, we submit the request to the worker.
                submitSyncRequest (newRestoreRequest credentials restorationBlockDepth)

-- | Restores the genesis addresses for a wallet, given its 'WalletDecrCredentials'.
-- NOTE: This doesn't have any effect on the balance as if these addresses still have
-- coins on them, this will be captured by the call to 'restoreWalletBalance', but yet
-- we want to add them to the pool of known addresses for history-rebuilding purposes.
restoreGenesisAddresses
  :: WalletDB
  -> WalletDecrCredentials
  -> m ()
data HdWallets = HdWallets {
restoreGenesisAddresses db credentials =
    let ownGenesisData =
            selectOwnAddresses credentials (txOutAddress . toaOut . snd) $
            M.toList $ unGenesisUtxo genesisUtxo
        ownGenesisAddrs = map snd ownGenesisData
    in mapM_ (WS.addWAddress db) ownGenesisAddrs
