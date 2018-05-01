{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Data.Maybe (fromJust)
import           System.Wlog (Severity(Debug))

import           Pos.Block.Types (Blund, Undo (..))
import           Pos.Core (HasConfiguration)

import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Types (RawResolvedBlock (..), fromRawResolvedBlock)
import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))

import           Pos.Util.Chrono (mapMaybeChrono)

import qualified Cardano.Wallet.Kernel.Actions as Actions
import qualified Data.Map.Strict as Map
import           Pos.Util.BackupPhrase
import           Pos.Crypto.Signing

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (HasConfiguration, MonadIO n, MonadIO m, MonadMask m, Monad n)
    => (Severity -> Text -> IO ())
    -> (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWallet logFunction f =
    Kernel.bracketPassiveWallet logFunction $ \w -> do

      invoke <- Actions.forkWalletWorker $ Actions.WalletActionInterp
               { Actions.applyBlocks  = \blunds ->
                   let resolvedBlocks = mapMaybeChrono blundToResolvedBlock blunds
                   in  liftIO $ Kernel.applyBlocks w resolvedBlocks
               , Actions.findUtxos    = logFunction Debug "(I'm supposed to be finding my utxos now)"
               , Actions.switchToFork = \_ _ -> logFunction Debug "<switchToFork>"
               , Actions.emit         = logFunction Debug
               }
      _ <- liftIO $ do
        let backup = BackupPhrase
                     { bpToList = ["squirrel", "material", "silly",   "twice",
                                    "direct",  "slush",    "pistol",  "razor",
                                    "become",  "junk",     "kingdom", "flee" ]
                     }
            Right (esk, _) = safeKeysFromPhrase emptyPassphrase backup
        Kernel.newWalletHdRnd w esk Map.empty

      f (passiveWalletLayer w invoke)

  where
    -- | TODO(ks): Currently not implemented!
    passiveWalletLayer _wallet inv =
        PassiveWalletLayer
            { _pwlCreateWallet  = error "Not implemented!"
            , _pwlGetWalletIds  = error "Not implemented!"
            , _pwlGetWallet     = error "Not implemented!"
            , _pwlUpdateWallet  = error "Not implemented!"
            , _pwlDeleteWallet  = error "Not implemented!"

            , _pwlCreateAccount = error "Not implemented!"
            , _pwlGetAccounts   = error "Not implemented!"
            , _pwlGetAccount    = error "Not implemented!"
            , _pwlUpdateAccount = error "Not implemented!"
            , _pwlDeleteAccount = error "Not implemented!"

            , _pwlGetAddresses  = error "Not implemented!"

            , _pwlApplyBlocks    = inv . Actions.ApplyBlocks
            , _pwlRollbackBlocks = inv . Actions.RollbackBlocks
            }

    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: Blund -> Maybe ResolvedBlock
    blundToResolvedBlock (b,u)
        = rightToJust b <&> \mainBlock ->
            fromRawResolvedBlock
            $ UnsafeRawResolvedBlock mainBlock spentOutputs'
        where
            spentOutputs' = map (map fromJust) $ undoTx u
            rightToJust   = either (const Nothing) Just

-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall m n a. (MonadIO n, MonadIO m, MonadMask m, Monad n)
    => PassiveWalletLayer n
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet walletPassiveLayer _walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())
