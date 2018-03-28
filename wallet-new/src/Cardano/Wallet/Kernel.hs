{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}

-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Cardano.Wallet.Kernel (
    -- * Passive wallet
    PassiveWalletLayer -- opaque
  , bracketPassiveWallet
  , bracketPassiveWalletLegacy
  , bracketPassiveWalletArbitrary
  , init
    -- * Active wallet
  , ActiveWalletLayer -- opaque
  , bracketActiveWallet
  , newPending
  , hasPending
  ) where

import           System.Wlog (Severity (..))
import           Universum

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
-- import           Pos.Wallet.Web.ClientTypes (AccountId, Addr, CAccountMeta, CId, CProfile, CTxId, CTxMeta, CUpdateInfo, CWAddressMeta, CWalletMeta,PassPhraseLU, Wal)

import           Cardano.Wallet.API.V1.Migration (migrate)
import           Cardano.Wallet.API.V1.Types
import           Pos.Wallet.Web.ClientTypes (CWalletMeta)
import qualified Pos.Wallet.Web.State.State as S
import           Pos.Core (TxAux)

import           Cardano.Wallet.Orphans.Arbitrary () -- Arbitrary instances
import           QuickCheck.GenT (MonadGen (..))
import           Test.QuickCheck (arbitrary)

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.
--
-- TODO: This is just a placeholder for now, we'll want all kinds of state
-- in here.
data PassiveWalletLayer m = PassiveWalletLayer {
    --  applyBlock      :: ...
    --, rollbackBlock   :: ...
      getWalletAddresses :: m [WalletId]
    , getWalletMeta      :: WalletId -> m (Maybe CWalletMeta)
      -- | Send log message
    , walletLogMessage   :: Severity -> Text -> IO ()
    }

--TODO(ks): makeLenses ''PassiveWalletLayer ?

-- | Allocate wallet resources
--
-- NOTE: See also 'init'.
-- TODO(ks): Left to implement after we define the new wallet
-- DB layer.
--
bracketPassiveWallet
    :: forall m n a. (Monad n, MonadMask m)
    => (Severity -> Text -> IO ())
    -> (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWallet walletLogMessage =
    bracket
      (return passiveWalletLayer)
      (\_ -> return ())
  where
    passiveWalletLayer :: PassiveWalletLayer n
    passiveWalletLayer = PassiveWalletLayer
        { getWalletAddresses  = error "Not implemented!"
        , getWalletMeta       = error "Not implemented!"
        , walletLogMessage    = walletLogMessage
        }

-- | Allocation of wallet resources for the legacy wallet.
bracketPassiveWalletLegacy
    :: forall ctx m n a. (S.WalletDbReader ctx n, MonadIO n, MonadThrow n, MonadMask m)
    => (Severity -> Text -> IO ())
    -> (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWalletLegacy walletLogMessage =
    bracket
      (return passiveWalletLayer)
      (\_ -> return ())
  where
    -- | We are going to need this on `set` functions.
    _walletDb :: S.WalletDbReader ctx n => n S.WalletDB
    _walletDb = S.askWalletDB

    walletSnapshot :: n S.WalletSnapshot
    walletSnapshot = S.askWalletSnapshot

    passiveWalletLayer :: PassiveWalletLayer n
    passiveWalletLayer = PassiveWalletLayer
        { getWalletAddresses  = walletSnapshot >>= \ws -> migrate $ S.getWalletAddresses ws
        , getWalletMeta       = \cIdWal -> walletSnapshot >>= \ws -> pure $ S.getWalletMeta ws =<< (migrate cIdWal)
        , walletLogMessage    = walletLogMessage
        }

-- | Allocation of the wallet resources for the arbitrary wallet.
bracketPassiveWalletArbitrary
    :: forall m n a. (MonadGen n, MonadMask m)
    => (Severity -> Text -> IO ())
    -> (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWalletArbitrary walletLogMessage =
    bracket
      (return passiveWalletLayer)
      (\_ -> return ())
  where
    passiveWalletLayer :: PassiveWalletLayer n
    passiveWalletLayer = PassiveWalletLayer
        { getWalletAddresses  = liftGen arbitrary
        , getWalletMeta       = \_ -> liftGen arbitrary
        , walletLogMessage    = walletLogMessage
        }

-- | Initialize the wallet
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWalletLayer m -> IO ()
init PassiveWalletLayer{..} = do
    walletLogMessage Info "Wallet kernel initialized"

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet
      walletPassive   :: PassiveWalletLayer m

      -- | The wallet diffusion layer
    , walletDiffusion :: WalletDiffusion
    }

-- | Initialize the active wallet
bracketActiveWallet :: forall m n a. (Monad n, MonadMask m)
                    => PassiveWalletLayer n
                    -> WalletDiffusion
                    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet walletPassive walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())

-- | Submit a new pending transaction
newPending :: ActiveWalletLayer m -> TxAux -> IO ()
newPending ActiveWalletLayer{..} _tx = do
    walletLogMessage Error "TODO: Cardano.Wallet.Kernel.newPending"
  where
    PassiveWalletLayer{..} = walletPassive

-- | Return True if there are pending transactions
hasPending :: ActiveWalletLayer m -> IO Bool
hasPending _ = return False
