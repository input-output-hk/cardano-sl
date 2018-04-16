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
  , getPassiveWallet
  , newPending
  , hasPending
  , updateUtxo
  , applyBlock
  , applyBlocks
  , Pending
  ) where

import Universum hiding (State)
import           Control.Lens.TH
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

import Pos.Crypto (EncryptedSecretKey) -- TODO Hash, PublicKey
import Pos.Txp.Toil.Types (Utxo)
import Pos.Util.Chrono (OldestFirst, NE)

import Data.Maybe (fromJust)

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

type Pending = Set TxAux
type Balance = Integer

data State = State {
      _stateUtxo        :: Utxo
    , _statePending     :: Pending
    , _stateUtxoBalance :: Balance
    }

-- | Wallet
--
-- Contains the Wallet EncryptedSecretKey and State DB handle

data Wallet = WalletHdRnd {
      -- | Wallet master key
      --
      -- TODO: We may need to rethink having this in-memory
      -- ESK should _not_ end up in the wallet's acid-state log
      -- (for some reason..)
    _walletESK :: EncryptedSecretKey

    -- | Wallet state
    --
    -- TODO: will become an acid-state handle
    , _walletState :: MVar State
    }

-- | Wallet Id
--
-- A Wallet Id can take several forms, the simplest of which is a hash
-- of the Wallet public key

data WalletId =
    -- | HD wallet with randomly generated addresses
    WalletIdHdRnd Int -- TODO @@@ (Hash PublicKey)

    {- potential future kinds of wallet IDs:
    -- | HD wallet with sequentially generated addresses
    | WalletIdHdSeq ...

    -- | External wallet (all crypto done off-site, like hardware wallets)
    | WalletIdExt ...
    -}
    deriving (Eq, Ord)

-- | Passive Wallet
--
-- Contains multiple PassiveWalletBase, indexed by WalletId

-- | Wallets
type Wallets = Map WalletId Wallet

-- | Passive Wallet
--
-- Contains multiple Wallet indexed by WalletId
data PassiveWallet = PassiveWallet {
      _walletLogMessage :: Severity -> Text -> IO ()
    , _wallets :: MVar Wallets
    }

makeLenses ''Wallet
makeLenses ''PassiveWallet
makeLenses ''State

-- | Get Map of wallets in PassiveWallet
--
getWallets :: PassiveWallet -> IO Wallets
getWallets pw = modifyWallets pw (\x -> x) -- TODO

-- | Modify PassiveWallet Wallet collection
--
modifyWallets :: PassiveWallet -> (Wallets -> Wallets) -> IO Wallets
modifyWallets pw modifyF = do
    ws <- takeMVar mvar

    let ws' = modifyF ws
    putMVar mvar ws'
    return ws'
    where mvar = pw ^. wallets

-- | Insert a new {WalletId -> Wallet} into PassiveWallet wallets
--
insertWallet :: PassiveWallet -> WalletId -> Wallet -> IO ()
insertWallet pw wid w
    = void $ modifyWallets pw (Map.insert wid w)

-- | Lookup Wallet in PassiveWallet, using WalletId
--
findWallet :: PassiveWallet -> WalletId -> IO (Maybe Wallet)
findWallet pw wid = do
    wallets' <- getWallets pw
    return $ Map.lookup wid wallets'

getWalletState :: PassiveWallet -> WalletId -> IO State
getWalletState pw wid = modifyWalletState pw wid (\x -> x) -- TODO id

getWalletUtxo :: PassiveWallet -> WalletId -> IO Utxo
getWalletUtxo pw wid = _stateUtxo <$> getWalletState pw wid

getWalletPending :: PassiveWallet -> WalletId -> IO Pending
getWalletPending pw wid = _statePending <$> getWalletState pw wid

-- | Modify Wallet State with given modifier function
--
modifyWalletState :: PassiveWallet -> WalletId -> (State -> State) -> IO State
modifyWalletState pw wid modifyF = do
    w <- fromJust <$> findWallet pw wid
    let mvar = w ^. walletState
    s <- takeMVar mvar

    let s' = modifyF s
    putMVar mvar s'
    return s'

-- | Replace Wallet State
--
updateWalletState :: PassiveWallet -> WalletId -> State -> IO ()
updateWalletState pw wid s' = void $ modifyWalletState pw wid (const s')

-- | Insert a new pending transaction to Pending set of the Wallet given
--   by WalletId.
--
insertWalletPending :: ActiveWallet -> WalletId -> TxAux -> IO ()
insertWalletPending ActiveWallet{..} wid tx
    = void $ modifyWalletState walletPassive wid modifyF
    where modifyF = over statePending (Set.insert tx)

-- TODO doc
initState :: State
initState = State {_stateUtxo = Map.empty
                  , _statePending = Set.empty
                  , _stateUtxoBalance = 0
                  }

-- TODO doc
initWalletHdRnd :: EncryptedSecretKey -> IO Wallet
initWalletHdRnd esk = do
    state' <- Universum.newMVar initState
    return $ WalletHdRnd esk state'

initPassiveWallet :: (Severity -> Text -> IO ())
                  -> IO PassiveWallet
initPassiveWallet logMessage = do
    ws <- Universum.newMVar Map.empty
    return $ PassiveWallet logMessage ws

-- TODO doc
newWalletHdRndWithESK :: PassiveWallet -> EncryptedSecretKey -> IO WalletId
newWalletHdRndWithESK pw esk = do
    w <- initWalletHdRnd esk
    let wid = todoEskToWalletID esk

    insertWallet pw wid w
    return wid
    where
        todoEskToWalletID _esk' = WalletIdHdRnd 0 -- TODO @@@

-- TODO `newWalletHdRnd :: PassiveWallet -> Utxo -> IO WalletId` (which will _construct_ a new secret key)

-- | Allocate wallet resources
--
-- NOTE: See also 'init'.
--
-- TODO: Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet :: (MonadMask m, MonadIO m)
                     => (Severity -> Text -> IO ())
                     -> EncryptedSecretKey
                     -> (PassiveWallet -> m a) -> m a
bracketPassiveWallet _walletLogMessage esk =
    bracket
      (do
          pw <- liftIO $ initPassiveWallet _walletLogMessage
          _wid <- liftIO $ newWalletHdRndWithESK pw esk -- TODO ?esdko -> discarding wid!
          return pw)
      (\_ -> return ())

-- | Initialize the Active wallet (specified by WalletId) with the given Utxo
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).

init :: PassiveWallet -> WalletId -> Utxo -> IO ()
init pw@PassiveWallet{..} wid utxo' = do
    _walletLogMessage Info "Wallet kernel initialized"

    when (utxo' /= Map.empty) $
        void $ modifyWalletState pw wid modifyF
    where modifyF = over stateUtxo (const utxo')

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

getPassiveWallet :: ActiveWallet -> PassiveWallet
getPassiveWallet = walletPassive

{-------------------------------------------------------------------------------
  Apply Block - updateUtxo, updatePending
-------------------------------------------------------------------------------}

-- | Apply the ResolvedBlock to all the wallets in the PassiveWallet
applyBlock :: HasConfiguration
          => PassiveWallet
          -> ResolvedBlock
          -> IO ()
applyBlock pw b
    = do
        ws <- getWallets pw
        let wids = map fst $ Map.toList ws
        mapM_ (applyBlock' pw b) wids

-- | Apply the ResolvedBlock to the PassiveWallet indexed by WalletID @wid@
applyBlock' :: HasConfiguration
            => PassiveWallet
            -> ResolvedBlock
            -> WalletId
            -> IO ()
applyBlock' pw b wid = do
    (State utxo' pending' balance') <- getWalletState pw wid
    w <- fromJust <$> findWallet pw wid

    let prefilteredTxs = prefilterTxs (w ^. walletESK) (rbTxs b)
        (utxo'', balanceDelta) = updateUtxo prefilteredTxs utxo'
        pending''              = updatePending prefilteredTxs pending'
        balance''              = balanceDelta + balance'

    updateWalletState pw wid $ State utxo'' pending'' balance''

-- | Apply the ResolvedBlocks, one at a time, to all wallets in the PassiveWallet
applyBlocks :: HasConfiguration
              => PassiveWallet
              -> OldestFirst NE ResolvedBlock
              -> IO ()
applyBlocks pw = mapM_ (applyBlock pw)

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
-- TODO WalletId

available :: PassiveWallet -> WalletId -> IO Utxo
available pw wid = do
    State utxo pending _ <- getWalletState pw wid

    return $ utxoRemoveInputs utxo (txIns' pending)

    where
        txIns' :: Set TxAux -> Set TxIn
        txIns' = Set.fromList . concatMap (NE.toList . _txInputs . taTx)

change :: PassiveWallet -> WalletId -> IO Utxo
change pw wid = do
    State _ pending _ <- getWalletState pw wid
    let pendingUtxo = unionTxOuts $ map (txUtxo . taTx) $ Set.toList pending

    w <- fromJust <$> findWallet pw wid
    return $ ourUtxo (w ^. walletESK) pendingUtxo

total :: PassiveWallet -> WalletId -> IO Utxo
total pw wid = Map.union <$> available pw wid <*> change pw wid

balance :: Utxo -> Balance
balance = sumCoins . map txOutValue . utxoOutputs

availableBalance :: PassiveWallet -> WalletId -> IO Balance
availableBalance pw wid = balance <$> available pw wid

totalBalance :: PassiveWallet -> WalletId -> IO Balance
totalBalance pw wid = balance <$> total pw wid

{-------------------------------------------------------------------------------
  New Pending
-------------------------------------------------------------------------------}

-- | Return True if there are pending transactions
hasPending :: ActiveWallet -> WalletId -> IO Bool
hasPending ActiveWallet{..} wid = do
    s <- getWalletState walletPassive wid
    return $ Set.size (s ^. statePending) > 0

-- | Submit a new pending transaction
newPending :: ActiveWallet -> WalletId -> TxAux -> IO Bool
newPending activeWallet@ActiveWallet{..} wid tx = do
    availableInputs <- utxoInputs <$> available walletPassive wid

    let isValid = txAuxInputSet tx `Set.isSubsetOf` availableInputs
    if isValid
        then insertWalletPending activeWallet wid tx >> return True
        else return False
