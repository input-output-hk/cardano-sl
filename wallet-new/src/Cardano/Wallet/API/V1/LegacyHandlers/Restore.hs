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
--
