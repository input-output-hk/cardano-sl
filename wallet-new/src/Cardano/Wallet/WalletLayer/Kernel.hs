{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( withPassiveWalletLayer
    , bracketActiveWallet
    ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Crypto.Hash (Blake2b_224)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           System.Wlog (Severity(Debug))

import           Pos.Block.Types (Blund, Undo (..))
import qualified Pos.Core as Core
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Crypto
                   (PassPhrase(..), AbstractHash, EncryptedSecretKey, PublicKey,
                    encToPublic, abstractHash)
import           Pos.Util.BackupPhrase (BackupPhrase, safeKeysFromPhrase)
import qualified Pos.Wallet.Web.ClientTypes.Types as WebTypes

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Actions as Actions
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb(InDb))
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Types
                   (RawResolvedBlock (..), fromRawResolvedBlock)
import           Cardano.Wallet.API.V1.Types (V1, unV1)
import qualified Cardano.Wallet.API.V1.Types as V1T
import           Cardano.Wallet.WalletLayer.Types
                   (ActiveWalletLayer(..), PassiveWalletLayer (..))

passiveWalletActionInterp
  :: MonadIO m
  => (Severity -> Text -> IO ())
  -> Kernel.PassiveWallet
  -> Actions.WalletActionInterp m Blund
passiveWalletActionInterp logf pw =
    Actions.WalletActionInterp
    { Actions.applyBlocks = \blunds -> liftIO $ do
        Kernel.applyBlocks pw
          (OldestFirst (mapMaybe blundToResolvedBlock
                                 (toList (getOldestFirst blunds))))
    , Actions.switchToFork = \_ _ -> liftIO $ do
        logf Debug "<passiveWalletActionInterp.switchToFork>"
    , Actions.emit = \t -> liftIO $ do
        logf Debug t
    }
  where
    blundToResolvedBlock :: Blund -> Maybe ResolvedBlock
    blundToResolvedBlock (eb, u) = do
      Right (mb :: Core.MainBlock) <- pure eb
      let spent :: [NonEmpty Core.TxOutAux]
          spent = map fromJust <$> undoTx u
      Just (fromRawResolvedBlock (UnsafeRawResolvedBlock mb spent))


-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
withPassiveWalletLayer
  :: forall n m a
  .  (MonadIO n, MonadIO m, MonadMask m)
  => (Severity -> Text -> IO ())
  -> (PassiveWalletLayer n -> m a)
  -> m a
withPassiveWalletLayer logf k =
   Kernel.withPassiveWallet logf $ \pw -> do
      let wai :: Actions.WalletActionInterp IO Blund
          wai = passiveWalletActionInterp logf pw
      Actions.withWalletWorker wai $ \send -> do
         let send' :: Actions.WalletAction Blund -> n ()
             send' = liftIO . STM.atomically . send
         k $ PassiveWalletLayer
             { _pwlCreateWallet = mkPwlCreateWallet pw
             , _pwlGetWalletIds   = error "Not implemented!"
             , _pwlGetWallet      = error "Not implemented!"
             , _pwlUpdateWallet   = error "Not implemented!"
             , _pwlDeleteWallet   = error "Not implemented!"

             , _pwlCreateAccount  = error "Not implemented!"
             , _pwlGetAccounts    = error "Not implemented!"
             , _pwlGetAccount     = error "Not implemented!"
             , _pwlUpdateAccount  = error "Not implemented!"
             , _pwlDeleteAccount  = error "Not implemented!"

             , _pwlGetAddresses   = error "Not implemented!"

             , _pwlApplyBlocks    = send' . Actions.ApplyBlocks
             , _pwlRollbackBlocks = send' . Actions.RollbackBlocks
             }

mkPwlCreateWallet
  :: MonadIO m => Kernel.PassiveWallet -> V1T.NewWallet -> m V1T.Wallet
mkPwlCreateWallet pw nw = liftIO $ do
  let bp :: BackupPhrase = unV1 (V1T.newwalBackupPhrase nw)
  let pp :: PassPhrase = maybe (PassPhrase mempty) unV1
                               (V1T.newwalSpendingPassword nw)
  esk :: EncryptedSecretKey <- case safeKeysFromPhrase pp bp of
     Left _ -> fail "Can't obtain encrypted secret key"
     Right (x,_) -> pure x
  let pubkh :: AbstractHash Blake2b_224 PublicKey
      pubkh = abstractHash (encToPublic esk)
  hsp <- case V1T.newwalSpendingPassword nw of
     Nothing -> pure HD.NoSpendingPassword
     Just _ -> do
        now :: Core.Timestamp <- Kernel.getCurrentTimestamp
        pure (HD.HasSpendingPassword (InDb now))
  let al = case V1T.newwalAssuranceLevel nw of
         V1T.NormalAssurance -> HD.AssuranceLevelNormal
         V1T.StrictAssurance -> HD.AssuranceLevelStrict
  let wn = HD.WalletName (V1T.newwalName nw)
  Kernel.createWalletHdRnd pw wn hsp al pubkh esk Map.empty >>= \case
     Left _ -> fail "Couldn't createWalletHdRnd"
     Right _ -> pure $ V1T.Wallet
        { V1T.walId                         = undefined :: V1T.WalletId
        , V1T.walName                       = undefined :: V1T.WalletName
        , V1T.walBalance                    = undefined :: (V1 Core.Coin)
        , V1T.walHasSpendingPassword        = undefined :: Bool
        , V1T.walSpendingPasswordLastUpdate = undefined :: (V1 Core.Timestamp)
        , V1T.walCreatedAt                  = undefined :: (V1 Core.Timestamp)
        , V1T.walAssuranceLevel             = undefined :: V1T.AssuranceLevel
        , V1T.walSyncState                  = undefined :: V1T.SyncState
        }


-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall m n a. (MonadIO m, MonadMask m)
    => PassiveWalletLayer n
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet walletPassiveLayer _walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())
