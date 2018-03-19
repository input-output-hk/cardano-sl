{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Cardano.Wallet.Kernel (
    -- * Passive wallet
    PassiveWallet -- opaque
  , bracketPassiveWallet
  , init
  , getWalletUtxo
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  , hasPending
  , updateUtxo
  , applyBlock
  , applyBlocks
  ) where

import Universum
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
--import Control.Concurrent.MVar
import System.Wlog (Severity(..))

import Cardano.Wallet.Kernel.Diffusion (WalletDiffusion(..))
import Cardano.Wallet.Kernel.PrefilterTx (prefilterTxs)

import Pos.Core (TxAux, HasConfiguration)
import Pos.Core.Txp (TxIn (..))
import Cardano.Wallet.Kernel.Types (ResolvedBlock(..),
                                    ResolvedTx(..),
                                    ResolvedTxPair)

import Pos.Crypto (EncryptedSecretKey)
import Pos.Txp.Toil.Types (Utxo)
import Pos.Util.Chrono (OldestFirst, NE)
{-------------------------------------------------------------------------------
  Apply Block/Blocks
-------------------------------------------------------------------------------}

applyBlock :: HasConfiguration
              => PassiveWallet
              -> ResolvedBlock
              -> IO ()
applyBlock _w _b = do
    utxo <- getWalletUtxo _w

    let resolvedTxs :: [ResolvedTx]
        resolvedTxs = rbTxs _b
        prefilteredTxs = prefilterTxs (walletESK _w) resolvedTxs

        utxo' = updateUtxo prefilteredTxs utxo

    updateWalletUtxo _w utxo'
    return ()

applyBlocks :: HasConfiguration
              => PassiveWallet
              -> OldestFirst NE ResolvedBlock
              -> IO ()
applyBlocks w bs = do
    mapM_ (applyBlock w) bs
    return ()

updateUtxo :: [ResolvedTx]  -- ^ Prefiltered [(inputs, outputsUtxo)]
            -> Utxo -> Utxo
updateUtxo prefilteredTxs = remSpent . addNew
  where
    txIns = unionTxIns $ map rtxInputs prefilteredTxs
    txOuts = unionTxOuts $ map rtxOutputs prefilteredTxs

    addNew :: Utxo -> Utxo
    addNew = Map.union txOuts

    remSpent :: Utxo -> Utxo
    remSpent = (`withoutKeys_` txIns)

withoutKeys_ :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys_` s = m `Map.difference` Map.fromSet (const ()) s

unionTxIns :: [[ResolvedTxPair]] -> Set TxIn
unionTxIns allTxIns = Set.fromList $ map fst $ concatMap toList allTxIns

unionTxOuts :: [Utxo] -> Utxo
unionTxOuts allUtxo = Map.unions allUtxo

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.
data PassiveWallet = PassiveWallet {
      -- | Send log message
      walletLogMessage :: Severity -> Text -> IO ()
    , walletESK :: EncryptedSecretKey
    , walletDb :: MVar Utxo -- temporary DB -> to come... AcidState
    }

getWalletUtxo :: PassiveWallet -> IO Utxo
getWalletUtxo w = takeMVar (walletDb w)

updateWalletUtxo :: PassiveWallet -> Utxo -> IO ()
updateWalletUtxo w utxo = putMVar (walletDb w) utxo

-- | Allocate wallet resources
--
-- NOTE: See also 'init'.
--
-- TODO: Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet :: (MonadMask m, MonadIO m)
                     => (Severity -> Text -> IO ())
                     -> EncryptedSecretKey
                     -> Utxo
                     -> (PassiveWallet -> m a) -> m a
bracketPassiveWallet walletLogMessage walletESK utxo =
    bracket
      (do
          walletDb <- Universum.newMVar utxo
          return PassiveWallet{..})
      (\_ -> return ())


-- | Initialize the wallet
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init PassiveWallet{..} = do
    walletLogMessage Info "Wallet kernel initialized"

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive   :: PassiveWallet

      -- | The wallet diffusion layer
    , walletDiffusion :: WalletDiffusion
    }

-- | Initialize the active wallet
bracketActiveWallet :: MonadMask m
                    => PassiveWallet
                    -> WalletDiffusion
                    -> (ActiveWallet -> m a) -> m a
bracketActiveWallet walletPassive walletDiffusion =
    bracket
      (return ActiveWallet{..})
      (\_ -> return ())

-- | Submit a new pending transaction
newPending :: ActiveWallet -> TxAux -> IO ()
newPending ActiveWallet{..} _tx = do
    walletLogMessage Error "TODO: Cardano.Wallet.Kernel.newPending"
  where
    PassiveWallet{..} = walletPassive

-- | Return True if there are pending transactions
hasPending :: ActiveWallet -> IO Bool
hasPending _ = return False
