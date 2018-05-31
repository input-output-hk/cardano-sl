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
    , CoinPolicyError (..)
    , CoinPolicyState
    , initCoinPolicyState
    , mergeCoinPolicyState
    , cpsUtxo
    , cpsSelectedInputs
    , cpsChangeOutputs
    , RunPolicy (..)
    , RunPolicyResult

    -- * Helpers for transient types
    , Addr (..)
    , Output (..)
    , TotalOutput (..)

    -- * Unsafe coercions to get rid of the 'Addr' indirection
    , coerceState
    , coerceError
    , coerceOutput

    -- * Helper functions
    , fromTxOut
    , fromTxOutAux
    , toTxOutAux

    ) where

import           Universum

import           Control.Lens.TH (makeLenses)

import qualified Data.Set as Set
import           Data.Text.Buildable (Buildable (..))
import           Formatting (bprint, (%))
import qualified Formatting as F

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

instance Buildable ExpenseRegulation where
    build SenderPaysFee   = bprint "SenderPaysFee"
    build ReceiverPaysFee = bprint "ReceiverPaysFee"

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

instance Buildable a => Buildable (Addr a) where
    build (Addr a) = build a
    build Treasury = bprint "Treasury"

-- | An ephimeral type used to work with transaction outputs parametrised over
-- an address 'a'.
-- contemplate treasury addresses.
data Output a = Output {
      outAddr :: a
    , outVal  :: Core.Coin
    }
    deriving (Eq, Ord)

instance Buildable a => Buildable (Output a) where
    build (Output addr val) = bprint ("Output { " %
                                      "  addr = " % F.build %
                                      ", val  = " % F.build) addr val

-- | Converts a Cardano's 'TxOut' into an 'Output', specialised over
-- 'Addr Core.Address'.
fromTxOut :: Core.TxOut -> Output Core.Address
fromTxOut txOut = Output (Core.txOutAddress txOut) (Core.txOutValue txOut)

fromTxOutAux :: Core.TxOutAux -> Output Core.Address
fromTxOutAux = fromTxOut . Core.toaOut

-- | Converts an 'Output' back into a Cardano-specific 'TxOut'. Users of this
-- function should use this one only when they are sure the input 'Output' is
-- not a 'Treasury' address, to prevent errors, as this function is effectively
-- partial.
toTxOutAux :: Output Core.Address -> Core.TxOutAux
toTxOutAux (Output a coin) = Core.TxOutAux (Core.TxOut a coin)

{-------------------------------------------------------------------------------
  Policy
-------------------------------------------------------------------------------}

-- | Monads in which we can run input selection policies
class Monad m => RunPolicy m a | m -> a where
  -- | Generate change address
  genChangeAddr :: m (Addr a)

newtype TotalOutput = TotalOutput { getTotal :: Core.Coin } deriving Show

-- | The possible errors encountered when performing coin selection
data CoinSelectionFailure a =
      CoinSelectionFailure CoinPolicyError
      -- ^ We failed to select funds to cover the outputs to pay.
    | InsufficientFundsToCoverFee ExpenseRegulation Core.Coin (Output a)
      -- ^ We need extra funds to cover the fee.
    | OutputIsReedeemAddress a
      -- ^ This output

data CoinPolicyError =
      UtxoExhausted Core.Coin TotalOutput
      -- ^ The Utxo (whose balance is given as the first argument)
      -- was exhaustively searched and it failed to satisfy the
      -- import of the payment, reported as the second argument.
    | CouldntFindCandidates

instance Buildable TotalOutput where
    build (TotalOutput o) = bprint F.build (Core.getCoin o)

instance Buildable CoinPolicyError where
    build (UtxoExhausted utxoBal amount) =
        bprint ("UtxoExhausted { utxo = " % F.build %
                " , payment_amount = " % F.build %
                " }") utxoBal amount
    build CouldntFindCandidates = bprint "CouldntFindCandidates"

instance Buildable a => Buildable (CoinSelectionFailure a) where
    build (CoinSelectionFailure err) =
        bprint ("CoinSelectionFailure " % F.build) err
    build (InsufficientFundsToCoverFee er c o) =
        bprint ("InsufficientFundsToCoverFee { " %
                "  expenseRegulation = " % F.build %
                ", coins = " % F.build %
                ", output = " % F.build) er c o
    build (OutputIsReedeemAddress a) = bprint ("OutputIsReedeemAddress " % F.build) a

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

coerceState :: CoinPolicyState (Addr a) -> CoinPolicyState a
coerceState = over cpsChangeOutputs (map coerceOutput)

coerceOutput :: Output (Addr a) -> Output a
coerceOutput (Output (Addr a) o) = Output a o
coerceOutput (Output Treasury _) =
  error $ "coerceOutput failed. This likely indicate an invariant violation " <>
          "and possibly a bug inside CoinSelection.Policies."

coerceError :: CoinSelectionFailure (Addr a) -> CoinSelectionFailure a
coerceError (CoinSelectionFailure policyErr) = CoinSelectionFailure policyErr
coerceError (InsufficientFundsToCoverFee e c (Output (Addr a) o)) =
    InsufficientFundsToCoverFee e c (Output a o)
coerceError (InsufficientFundsToCoverFee _ _ (Output Treasury _)) =
    error "coerceError: invariant violation for InsufficientFundsToCoverFee"
coerceError (OutputIsReedeemAddress (Addr a)) = OutputIsReedeemAddress a
coerceError (OutputIsReedeemAddress Treasury) =
    error "coerceError: invariant violation for OutputIsReedeemAddress"

-- | A top-level, specialised type alias used to handle errors at the boundaries
-- of the policy mechanism, when dealing with concrete Cardano types.
type RunPolicyResult a r = Either [CoinSelectionFailure a] r

