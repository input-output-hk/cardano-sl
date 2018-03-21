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
  , getWalletPending
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  , hasPending
  , updateUtxo
  , applyBlock
  , applyBlocks
  ) where

import Universum hiding (State)
import           Control.Lens.TH
import           Control.Concurrent.MVar(modifyMVar_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import           System.Wlog (Severity(..))

import Cardano.Wallet.Kernel.Diffusion (WalletDiffusion(..))
import Cardano.Wallet.Kernel.PrefilterTx (prefilterTxs)

import Pos.Core (TxAux, HasConfiguration)
import Pos.Core.Txp (TxIn (..), Tx (..), TxAux (..))
import Cardano.Wallet.Kernel.Types (ResolvedBlock(..),
                                    ResolvedTx(..),
                                    ResolvedTxPair)

import Pos.Crypto (EncryptedSecretKey)
import Pos.Txp.Toil.Types (Utxo)
import Pos.Util.Chrono (OldestFirst, NE)

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.

type Pending = Set TxAux

data State = State {
      _stateUtxo        :: MVar Utxo -- temporary DB before AcidState DB...
    , _statePending     :: MVar Pending
    }

data PassiveWallet = PassiveWallet {
      -- | Send log message
      _walletLogMessage :: Severity -> Text -> IO ()
    , _walletESK :: EncryptedSecretKey
    , _walletState :: State
    }

makeLenses ''PassiveWallet
makeLenses ''State

-- DB stubs
getWalletUtxo :: PassiveWallet -> IO Utxo
getWalletUtxo w = takeMVar (w ^. walletState ^. stateUtxo)

updateWalletUtxo :: PassiveWallet -> Utxo -> IO ()
updateWalletUtxo w utxo = putMVar (w ^. walletState ^. stateUtxo) utxo

getWalletPending :: PassiveWallet -> IO Pending
getWalletPending w = takeMVar (w ^. walletState ^. statePending)

updateWalletPending :: PassiveWallet -> Pending -> IO ()
updateWalletPending w = putMVar (w ^. walletState ^. statePending)

insertWalletPending :: ActiveWallet -> TxAux -> IO ()
insertWalletPending ActiveWallet{..} tx
    = modifyMVar_ (walletPassive ^. walletState ^. statePending)
                  (\pending -> return (Set.insert tx pending))

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
bracketPassiveWallet _walletLogMessage _walletESK utxo =
    bracket
      (do
          _stateUtxo <- Universum.newMVar utxo
          _statePending <- Universum.newMVar Set.empty
          let _walletState = State{..}

          return PassiveWallet{..})
      (\_ -> return ())


-- | Initialize the wallet
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init PassiveWallet{..} = do
    _walletLogMessage Info "Wallet kernel initialized"

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

{-------------------------------------------------------------------------------
  Apply Block - updateUtxo, updatePending
-------------------------------------------------------------------------------}

applyBlock :: HasConfiguration
              => PassiveWallet
              -> ResolvedBlock
              -> IO ()
applyBlock _w _b = do
    utxo <- getWalletUtxo _w
    pending <- getWalletPending _w

    let prefilteredTxs = prefilterTxs (_w ^. walletESK) (rbTxs _b)

    updateWalletUtxo    _w  $ updateUtxo prefilteredTxs utxo
    updateWalletPending _w  $ updatePending prefilteredTxs pending
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
updateUtxo prefilteredTxs = utxoRemoveInputs txIns . addNew
  where
    txIns = unionTxIns $ map rtxInputs prefilteredTxs
    txOuts = unionTxOuts $ map rtxOutputs prefilteredTxs

    addNew :: Utxo -> Utxo
    addNew = Map.union txOuts

updatePending :: [ResolvedTx]  -- ^ Prefiltered [(inputs, outputsUtxo)]
              -> Pending -> Pending
updatePending prefilteredTxs = remSpent
  where
    txIns = unionTxIns $ map rtxInputs prefilteredTxs

    remSpent :: Pending -> Pending -- TODO reduce type transformations
    remSpent = Set.filter (\t -> disjoint (txAuxInputSet t) txIns)

txAuxInputSet :: TxAux -> Set TxIn
txAuxInputSet = Set.fromList . NE.toList . _txInputs . taTx

withoutKeys_ :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys_` s = m `Map.difference` Map.fromSet (const ()) s

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = Set.null (a `Set.intersection` b)

unionTxIns :: [[ResolvedTxPair]] -> Set TxIn
unionTxIns allTxIns = Set.fromList $ map fst $ concatMap toList allTxIns

unionTxOuts :: [Utxo] -> Utxo
unionTxOuts allUtxo = Map.unions allUtxo

utxoRemoveInputs :: Set TxIn -> Utxo -> Utxo
utxoRemoveInputs txIns = (`withoutKeys_` txIns)

utxoInputs :: Utxo -> Set TxIn
utxoInputs = Map.keysSet

{-------------------------------------------------------------------------------
  New Pending
-------------------------------------------------------------------------------}

-- | Return True if there are pending transactions
hasPending :: ActiveWallet -> IO Bool
hasPending ActiveWallet{..} = do
    pending' <- getWalletPending walletPassive
    return $ Set.size pending' > 0

available :: PassiveWallet -> IO Utxo
available w = do
    utxo' <- getWalletUtxo w
    pending' <- getWalletPending w

    return $ utxoRemoveInputs (txIns pending') utxo'

    where
        txIns :: Set TxAux -> Set TxIn
        txIns txs = Set.fromList . concatMap (NE.toList . _txInputs . taTx) $ txs

-- | Submit a new pending transaction
newPending :: ActiveWallet -> TxAux -> IO ()
newPending activeWallet@ActiveWallet{..} _tx = do
    availableInputs <- utxoInputs <$> available walletPassive
    guard $ txAuxInputSet _tx `Set.isSubsetOf` availableInputs

    insertWalletPending activeWallet _tx
    return ()
