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
import Cardano.Wallet.Kernel.PrefilterTx (prefilterTxs, ourUtxo)

import Pos.Core (TxAux, HasConfiguration, sumCoins)
import Pos.Core.Txp (TxIn (..), Tx (..), TxAux (..), TxOutAux (..), TxOut (..))
import Cardano.Wallet.Kernel.Types (ResolvedBlock(..),
                                    ResolvedTx(..),
                                    ResolvedTxPair,
                                    txUtxo)

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
type Balance = Integer

data State = State {
      _stateUtxo        :: MVar Utxo -- temporary DB before AcidState DB...
    , _statePending     :: MVar Pending
    , _stateUtxoBalance :: MVar Balance
    }

data PassiveWallet = PassiveWallet {
      -- | Send log message
      _walletLogMessage :: Severity -> Text -> IO ()
    , _walletESK :: EncryptedSecretKey -- TODO MVar [EncryptedSecretKey] or [esk + [State]]
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

getWalletUtxoBalance :: PassiveWallet -> IO Balance
getWalletUtxoBalance w = takeMVar (w ^. walletState ^. stateUtxoBalance)

updateWalletUtxoBalance :: PassiveWallet -> Balance -> IO ()
updateWalletUtxoBalance w = putMVar (w ^. walletState ^. stateUtxoBalance)

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
          _stateUtxoBalance <- Universum.newMVar 0
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
applyBlock w b = do
    utxo <- getWalletUtxo w
    pending <- getWalletPending w
    utxoBalance <- getWalletUtxoBalance w

    let prefilteredTxs = prefilterTxs (w ^. walletESK) (rbTxs b)
        (utxo', balanceDelta) = updateUtxo prefilteredTxs utxo
        pending'              = updatePending prefilteredTxs pending
        balance'              = balanceDelta + utxoBalance

    updateWalletUtxo        w $ utxo'
    updateWalletPending     w $ pending'
    updateWalletUtxoBalance w $ balance'

applyBlocks :: HasConfiguration
              => PassiveWallet
              -> OldestFirst NE ResolvedBlock
              -> IO ()
applyBlocks w bs = do
    mapM_ (applyBlock w) bs
    return ()

updateUtxo :: [ResolvedTx]  -- ^ Prefiltered [(inputs, outputsUtxo)]
            -> Utxo -> (Utxo, Balance)
updateUtxo prefilteredTxs currentUtxo
    = (utxo', balanceDelta)
    where
        prefilteredIns = txIns prefilteredTxs
        unionUtxo = Map.union (txOuts prefilteredTxs) currentUtxo
        utxo' = utxoRemoveInputs unionUtxo prefilteredIns

        unionUtxoRestricted  = utxoRestrictToInputs unionUtxo prefilteredIns
        balanceDelta = balance unionUtxo - balance unionUtxoRestricted


updatePending :: [ResolvedTx]  -- ^ Prefiltered [(inputs, outputsUtxo)]
              -> Pending -> Pending
updatePending prefilteredTxs
    = Set.filter (\t -> disjoint (txAuxInputSet t) (txIns prefilteredTxs))

txAuxInputSet :: TxAux -> Set TxIn
txAuxInputSet = Set.fromList . NE.toList . _txInputs . taTx

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys` s = m `Map.difference` Map.fromSet (const ()) s

restrictKeys :: Ord k => Map k a -> Set k -> Map k a
m `restrictKeys` s = m `Map.intersection` Map.fromSet (const ()) s

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = Set.null (a `Set.intersection` b)

txIns :: [ResolvedTx] -> Set TxIn
txIns = unionTxIns . map rtxInputs

txOuts :: [ResolvedTx] -> Utxo
txOuts = unionTxOuts . map rtxOutputs

unionTxIns :: [[ResolvedTxPair]] -> Set TxIn
unionTxIns allTxIns = Set.fromList $ map fst $ concatMap toList allTxIns

unionTxOuts :: [Utxo] -> Utxo
unionTxOuts = Map.unions

utxoRemoveInputs :: Utxo -> Set TxIn -> Utxo
utxoRemoveInputs = withoutKeys

utxoRestrictToInputs :: Utxo -> Set TxIn -> Utxo
utxoRestrictToInputs = restrictKeys

utxoInputs :: Utxo -> Set TxIn
utxoInputs = Map.keysSet

utxoOutputs :: Utxo -> [TxOut]
utxoOutputs = map toaOut . Map.elems

{-------------------------------------------------------------------------------
  Available, Change, Total + balances
-------------------------------------------------------------------------------}

available :: PassiveWallet -> IO Utxo
available w = do
    utxo' <- getWalletUtxo w
    pending' <- getWalletPending w

    return $ utxoRemoveInputs utxo' (txIns' pending')

    where
        txIns' :: Set TxAux -> Set TxIn
        txIns' txs = Set.fromList . concatMap (NE.toList . _txInputs . taTx) $ txs

change :: PassiveWallet -> IO Utxo
change w = do
    pending' <- getWalletPending w
    let pendingUtxo = unionTxOuts $ map (txUtxo . taTx) $ Set.toList pending'

    return $ ourUtxo (_walletESK w) pendingUtxo

total :: PassiveWallet -> IO Utxo
total w = Map.union <$> available w <*> change w

balance :: Utxo -> Balance
balance = sumCoins . map txOutValue . utxoOutputs

availableBalance :: PassiveWallet -> IO Balance
availableBalance w = balance <$> available w

totalBalance :: PassiveWallet -> IO Balance
totalBalance w = balance <$> total w

{-------------------------------------------------------------------------------
  New Pending
-------------------------------------------------------------------------------}

-- | Return True if there are pending transactions
hasPending :: ActiveWallet -> IO Bool
hasPending ActiveWallet{..} = do
    pending' <- getWalletPending walletPassive
    return $ Set.size pending' > 0

-- | Submit a new pending transaction
newPending :: ActiveWallet -> TxAux -> IO Bool
newPending activeWallet@ActiveWallet{..} tx = do
    availableInputs <- utxoInputs <$> available walletPassive

    let isValid = txAuxInputSet tx `Set.isSubsetOf` availableInputs
    if isValid
        then insertWalletPending activeWallet tx >> return True
        else return False
