module Cardano.Wallet.API.V1.LegacyHandlers.Restore
  ( restoreWalletFromSeed
  ) where


import Crypto.Hash (Blake2b_224)
import System.Wlog (Severity)
import Universum

import qualified Pos.Core as Core
import Pos.Crypto (AbstractHash, EncryptedSecretKey, PassPhrase, PublicKey)
import qualified Pos.Txp.Toil.Types
import Pos.Wallet.Web.Account (genSaveRootKey)
import Pos.Wallet.Web.ClientTypes
  (CId, CWallet (..), CWalletInit (..), CWalletMeta (..), Wal, mkCCoin,
   CWalletAssurance(CWANormal, CWAStrict), encToCId)
import qualified Pos.Wallet.Web.Methods.Logic as L

import Cardano.Wallet.Kernel
  (PassiveWallet, createWalletHdRnd, bracketPassiveWallet, getCurrentTimestamp)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HdW
import Cardano.Wallet.Kernel.DB.InDb (InDb(InDb))


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
  now <- getCurrentTimestamp
  let cWM :: CWalletMeta = cwInitMeta cwInit
  esk :: EncryptedSecretKey <- genSaveRootKey passphrase (cwBackupPhrase cwInit)
  let cIdWal :: CId Wal = encToCId esk
  let cIdWalToHashPublicKey :: CId Wal -> AbstractHash Blake2b_224 PublicKey = undefined
  bracketPassiveWallet logf $ \pw -> do
     ea <- liftIO $ createWalletHdRnd
        (pw :: PassiveWallet)
        (HdW.WalletName (cwName cWM))
        (HdW.HasSpendingPassword (InDb now))
        (case cwAssurance cWM of
            CWAStrict -> HdW.AssuranceLevelStrict
            CWANormal -> HdW.AssuranceLevelNormal)
        (cIdWalToHashPublicKey cIdWal, esk)
        (mempty :: Pos.Txp.Toil.Types.Utxo)
     case ea of
        Left _ -> error "TODO error 400 something"
        Right (hdRoot, accs) -> do
           -- restoreWallet hdRoot
           pure $ CWallet
              { cwId = cIdWal
              , cwMeta = cWM
              , cwAccountsNumber = length accs
              , cwAmount = mkCCoin (Core.Coin (fromIntegral (cwUnit cWM)))
              , cwHasPassphrase = True
              , cwPassphraseLU = let InDb x = HdW._hdRootCreatedAt hdRoot
                                 in view Core.timestampSeconds x }


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
