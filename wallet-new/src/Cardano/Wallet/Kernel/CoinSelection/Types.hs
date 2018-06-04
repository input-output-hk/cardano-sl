{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
module Cardano.Wallet.Kernel.CoinSelection.Types (
    -- * Types and lenses
      ExpenseRegulation (..)
    , InputGrouping (..)
    , CoinSelectionOptions
    , csoDustThreshold
    , csoEstimateFee
    , csoInputGrouping
    , csoExpenseRegulation
    , csoMakeSigner
    , newOptions
    , CoinSelectionFailure (..)
    , CoinPolicyState
    , initCoinPolicyState
    , mergeCoinPolicyState
    , cpsUtxo
    , cpsSelectedInputs
    , cpsChangeOutputs
    , CoinSelectionPolicy
    , RunPolicy (..)
    , RunPolicyResult

    -- * Helpers for transient types
    , Addr (Treasury)
    , Output (..)
    , TotalOutput (..)

    -- * Helper functions
    , fromTxOut
    , fromTxOutAux
    , toTxOutAux

    ) where

import           Universum

import           Control.Lens.TH (makeLenses)

import qualified Data.Set as Set

import qualified Pos.Core as Core
import qualified Pos.Crypto as Core
import qualified Pos.Txp as Core

{-------------------------------------------------------------------------------
  Expense regulation (i.e. "who pays for the fee"?)
-------------------------------------------------------------------------------}

data ExpenseRegulation =
      SenderPaysFee
    -- ^ The sender pays for the fee. This is the typical case.
    | ReceiverPaysFee
    -- ^ The receiver pays for the fee. This is useful for cases
    -- where users wants to transfer funds between wallets owned by them,
    -- and they wish to trasfer an @exact@ amount (or, for example, the max
    -- amount).

{-------------------------------------------------------------------------------
  Input grouping

  Input grouping is the process of requiring additional (\"unsolicited\")
  inputs as part of the coin selection if they all output to the same 'Address'.
  This is useful in some circumstances as a security measure to make sure that,
  if the target 'Address' is leaked, then the remaining funds associated to it
  would be 0.
-------------------------------------------------------------------------------}

data InputGrouping =
      RequireGrouping
      -- ^ Requires that grouping is enforced throughout the coin selection process.
      -- Failure to do so so would result in an 'CoinSelectionFailure'.
    | PreferGrouping
      -- ^ If possible, try to group the inputs. If not possible, fallback to
      -- 'IgnoreGrouping' without failing.
    | IgnoreGrouping
      -- ^ Ignore input grouping. This achieves the best troughput as avoid
      -- unnecessary traversal of the Utxo, at the expense of the security.

{-------------------------------------------------------------------------------
  Selection context
-------------------------------------------------------------------------------}

data CoinSelectionOptions = CoinSelectionOptions {
      _csoEstimateFee       :: Int -> NonEmpty Core.Coin -> Core.Coin
    -- ^ A function to estimate the fees.
    , _csoInputGrouping     :: InputGrouping
    -- ^ A preference regarding input grouping.
    , _csoExpenseRegulation :: ExpenseRegulation
    -- ^ A preference regarding expense regulation
    , _csoDustThreshold     :: Maybe Core.Coin
    -- ^ If set, change addresses below the given threshold will be evicted
    -- from the created transaction.
    , _csoMakeSigner        :: forall e. Core.Address -> Either e Core.SafeSigner
    -- ^ How to sign an 'Address'.
    }

makeLenses ''CoinSelectionOptions

-- | Creates new 'CoinSelectionOptions' using 'PreferGrouping' as default
-- 'InputGrouping' and 'SenderPaysFee' as default 'ExpenseRegulation'.
newOptions :: (Int -> NonEmpty Core.Coin -> Core.Coin)
           -> (forall e. Core.Address -> Either e Core.SafeSigner)
           -> CoinSelectionOptions
newOptions estimateFee mkSigner = CoinSelectionOptions {
      _csoEstimateFee       = estimateFee
    , _csoInputGrouping     = PreferGrouping
    , _csoExpenseRegulation = SenderPaysFee
    , _csoDustThreshold = Nothing
    , _csoMakeSigner = mkSigner
    }

{-------------------------------------------------------------------------------
  Domain-specific types
-------------------------------------------------------------------------------}

-- | An ephimeral type used to work in an address schema which doesn't
-- contemplate treasury addresses.
-- In the context of coin selection, the fees are treated as outputs to
-- a treasury address, but we don't have this notion in Cardano. Using 'Addr'
-- and converting everything back to a Cardano Address at the very last minute
-- (i.e. when we forge the final 'Tx') solves the problem.
data Addr a = Addr a | Treasury deriving (Eq, Ord)

-- | An ephimeral type used to work with transaction outputs parametrised over
-- an address 'a'.
-- contemplate treasury addresses.
data Output a = Output {
      outAddr :: Addr a
    , outVal  :: Core.Coin
    }
    deriving (Eq, Ord)

-- | Converts a Cardano's 'TxOut' into an 'Output', specialised over
-- 'Addr Core.Address'.
fromTxOut :: Core.TxOut -> Output Core.Address
fromTxOut txOut = Output (Addr (Core.txOutAddress txOut)) (Core.txOutValue txOut)

fromTxOutAux :: Core.TxOutAux -> Output Core.Address
fromTxOutAux = fromTxOut . Core.toaOut

-- | Converts an 'Output' back into a Cardano-specific 'TxOut'. Users of this
-- function should use this one only when they are sure the input 'Output' is
-- not a 'Treasury' address, to prevent errors, as this function is effectively
-- partial.
toTxOutAux :: Output Core.Address -> Core.TxOutAux
toTxOutAux (Output (Addr a) coin) = Core.TxOutAux (Core.TxOut a coin)
toTxOutAux (Output Treasury _) =
  error $ "toTxOut was called on a 'Treasury' address, which shouldn't have " <>
          "leaked outside the coin selection policy algorithm. This indicates " <>
          "a bug in the runCoinPolicyT function."

{-------------------------------------------------------------------------------
  Policy
-------------------------------------------------------------------------------}

-- | Monads in which we can run input selection policies
class Monad m => RunPolicy m a | m -> a where
  -- | Generate change address
  genChangeAddr :: m (Addr a)


-- | The possible errors encountered when performing coin selection
data CoinSelectionFailure a =
      CoinSelectionFailure
      -- ^ We failed to select funds to cover the outputs to pay.
    | InsufficientFundsToCoverFee ExpenseRegulation Core.Coin (Output a)
      -- ^ We need extra funds to cover the fee.
    | OutputIsReedeemAddress a
      -- ^ This output


type CoinSelectionPolicy a m =
      CoinSelectionOptions
      -- ^ User-provided options
   -> Core.Utxo
      -- ^ The initial UTXO
   -> NonEmpty Core.TxOut
      -- ^ The outputs we need to pay.
   -> m (Either [CoinSelectionFailure a] Core.TxAux)

{-------------------------------------------------------------------------------
  Coin selection combinator
-------------------------------------------------------------------------------}

data CoinPolicyState a = CoinPolicyState {
      -- | Available entries in the UTxO
      _cpsUtxo           :: Core.Utxo

      -- | Selected inputs
    , _cpsSelectedInputs :: Set Core.TxIn

      -- | Generated change outputs
    , _cpsChangeOutputs  :: [Output a]
    }

-- | Initialises the 'CoinPolicyState'.
initCoinPolicyState :: Core.Utxo -> CoinPolicyState a
initCoinPolicyState utxo = CoinPolicyState {
      _cpsUtxo             = utxo
    , _cpsSelectedInputs   = Set.empty
    , _cpsChangeOutputs = []
    }


-- | Merges two 'CoinPolicyState' together.
-- The final 'Utxo' is taken from the last passed as input, as we need to
-- pick the \"most recent\" one.
mergeCoinPolicyState :: CoinPolicyState a
                     -> CoinPolicyState a
                     -> CoinPolicyState a
mergeCoinPolicyState (CoinPolicyState _ s1 c1) (CoinPolicyState u2 s2 c2) =
    CoinPolicyState u2 (s1 `Set.union` s2) (c1 `mappend` c2)

makeLenses ''CoinPolicyState

-- | A top-level, specialised type alias used to handle errors at the boundaries
-- of the policy mechanism, when dealing with concrete Cardano types.
type RunPolicyResult a r = Either [CoinSelectionFailure a] r

newtype TotalOutput = TotalOutput { getTotal :: Core.Coin }

