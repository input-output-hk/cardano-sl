-- | Wallet state as mandated by the wallet specification
module Cardano.Wallet.Kernel.DB.Spec (
    -- * Wallet state as mandated by the spec
    Pending(..)
  , Checkpoint(..)
  , Checkpoints
  , genPending
  , emptyPending
    -- ** Lenses
  , pendingTransactions
  , checkpointUtxo
  , checkpointUtxoBalance
  , checkpointExpected
  , checkpointPending
  , checkpointBlockMeta
    -- ** Lenses into the current checkpoint
  , currentCheckpoint
  , currentUtxo
  , currentUtxoBalance
  , currentExpected
  , currentPending
  , currentBlockMeta
  ) where

import           Universum

import           Control.Lens (to)
import           Control.Lens.TH (makeLenses)
import qualified Data.Map.Strict as M
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.Text.Buildable (build)
import qualified Data.Vector as V
import           Formatting (bprint)
import qualified Formatting as F
import           Serokell.Util.Text (mapBuilder)
import           Test.QuickCheck (Gen, listOf)

import qualified Pos.Arbitrary.Txp as Core
import qualified Pos.Core as Core
import           Pos.Crypto.Hashing (hash)
import qualified Pos.Txp as Core

import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb

{-------------------------------------------------------------------------------
  Wallet state as mandated by the spec
-------------------------------------------------------------------------------}

-- | Pending transactions
data Pending = Pending {
      _pendingTransactions :: InDb (Map Core.TxId Core.TxAux)
     } deriving Eq

instance Buildable Pending where
    build (Pending p) = bprint (F.later mapBuilder) (p ^. fromDb . to M.toList)

genPending :: Core.ProtocolMagic -> Gen Pending
genPending pMagic = do
    elems <- listOf (do tx  <- Core.genTx
                        wit <- (V.fromList <$> listOf (Core.genTxInWitness pMagic))
                        aux <- Core.TxAux <$> pure tx <*> pure wit
                        pure (hash tx, aux)
                    )
    return . Pending . InDb . M.fromList $ elems

-- | Returns a new, empty 'Pending' set.
emptyPending :: Pending
emptyPending = Pending . InDb $ mempty

-- | Per-wallet state
--
-- This is the same across all wallet types.
data Checkpoint = Checkpoint {
      _checkpointUtxo        :: InDb Core.Utxo
    , _checkpointUtxoBalance :: InDb Core.Coin
    , _checkpointExpected    :: InDb Core.Utxo
    , _checkpointPending     :: Pending
    , _checkpointBlockMeta   :: BlockMeta
    }

-- | List of checkpoints
type Checkpoints = NonEmpty Checkpoint

makeLenses ''Pending
makeLenses ''Checkpoint

deriveSafeCopy 1 'base ''Pending
deriveSafeCopy 1 'base ''Checkpoint

{-------------------------------------------------------------------------------
  Lenses for accessing current checkpoint
-------------------------------------------------------------------------------}

currentCheckpoint :: Lens' Checkpoints Checkpoint
currentCheckpoint = neHead

currentUtxo        :: Lens' Checkpoints Core.Utxo
currentUtxoBalance :: Lens' Checkpoints Core.Coin
currentExpected    :: Lens' Checkpoints Core.Utxo
currentPending     :: Lens' Checkpoints Pending
currentBlockMeta   :: Lens' Checkpoints BlockMeta

currentUtxo        = currentCheckpoint . checkpointUtxo        . fromDb
currentUtxoBalance = currentCheckpoint . checkpointUtxoBalance . fromDb
currentExpected    = currentCheckpoint . checkpointExpected    . fromDb
currentPending     = currentCheckpoint . checkpointPending
currentBlockMeta   = currentCheckpoint . checkpointBlockMeta

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

neHead :: Lens' (NonEmpty a) a
neHead f (x :| xs) = (:| xs) <$> f x
