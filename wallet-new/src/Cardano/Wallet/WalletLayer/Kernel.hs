{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Pos.Core (HasConfiguration)
import           System.Wlog (Severity)
import           Universum

import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))

import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Pos.Block.Types (Blund, Undo (..))
import           Pos.Util.Chrono (NE, OldestFirst (..))

import           Cardano.Wallet.Kernel.Types (RawResolvedBlock (..), ResolvedBlock (..),
                                              fromRawResolvedBlock)
import           Data.Maybe (fromJust)

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (HasConfiguration, MonadIO n, MonadIO m, MonadMask m, Monad n)
    => (Severity -> Text -> IO ())
    -> (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWallet logFunction f =
    Kernel.bracketPassiveWallet logFunction $ \w ->
                f (passiveWalletLayer w)

  where
    -- | TODO(ks): Currently not implemented!
    passiveWalletLayer _wallet =
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

            , _pwlApplyBlocks = applyBlocks' _wallet
            }

    applyBlocks' :: forall n''. (HasConfiguration, MonadIO n'')
                 => Kernel.PassiveWallet -> OldestFirst NE Blund -> n'' ()
    applyBlocks' w blunds
        = do
            let resolvedBlocks = map blundToResolvedBlock blunds
            liftIO $ Kernel.applyBlocks w resolvedBlocks

    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: Blund -> ResolvedBlock
    blundToResolvedBlock (b,u)
        = case b of
            Left _ -> error "genesis block, expecting a MainBlock"
            Right mainBlock ->
                fromRawResolvedBlock
              $ UnsafeRawResolvedBlock mainBlock spentOutputs'
        where
            spentOutputs' = map (map fromJust) $ undoTx u

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
