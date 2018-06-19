{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Kernel.CoinSelection.FromGeneric (
    -- * Instantiation of the generic framework
    Cardano
    -- * Coin selection options
  , ExpenseRegulation(..)
  , InputGrouping(..)
  , CoinSelectionOptions(..)
  , newOptions
    -- * Transaction building
  , MkTx
  , mkStdTx
    -- * Coin selection policies
  , random
  , largestFirst
  ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import qualified Pos.Client.Txp.Util as Core
import qualified Pos.Core as Core
import qualified Pos.Crypto as Core
import qualified Pos.Txp as Core

import           Cardano.Wallet.Kernel.CoinSelection.Generic
import           Cardano.Wallet.Kernel.CoinSelection.Generic.Fees
import qualified Cardano.Wallet.Kernel.CoinSelection.Generic.LargestFirst as LargestFirst
import qualified Cardano.Wallet.Kernel.CoinSelection.Generic.Random as Random

{-------------------------------------------------------------------------------
  Type class instances
-------------------------------------------------------------------------------}

data Cardano

instance CoinSelDom Cardano where
  type Input  Cardano = Core.TxIn
  type Output Cardano = Core.TxOutAux
  type Value  Cardano = Core.Coin

  outVal    = Core.txOutValue . Core.toaOut
  outSetVal = \v o -> o {Core.toaOut = (Core.toaOut o) {Core.txOutValue = v}}

  valueZero   = Core.mkCoin 0
  valueAdd    = Core.addCoin
  valueSub    = Core.subCoin
  valueMult   = Core.mulCoin
  valueDist   = \  a b -> if a < b then b `Core.unsafeSubCoin` a
                                   else a `Core.unsafeSubCoin` b
  valueRatio  = \  a b -> coinToDouble a / coinToDouble b
  valueAdjust = \r d a -> coinFromDouble r (d * coinToDouble a)

coinToDouble :: Core.Coin -> Double
coinToDouble = fromIntegral . Core.getCoin

coinFromDouble :: Rounding -> Double -> Maybe Core.Coin
coinFromDouble _ d         | d < 0 = Nothing
coinFromDouble RoundUp   d = safeMkCoin (ceiling d)
coinFromDouble RoundDown d = safeMkCoin (floor   d)

safeMkCoin :: Word64 -> Maybe Core.Coin
safeMkCoin w = let coin = Core.Coin w in
               case Core.checkCoin coin of
                 Left _err -> Nothing
                 Right ()  -> Just coin

instance PickFromUtxo Core.Utxo where
  type Dom Core.Utxo = Cardano

{-------------------------------------------------------------------------------
  Coin selection options
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

data CoinSelectionOptions = CoinSelectionOptions {
      csoEstimateFee       :: Int -> NonEmpty Core.Coin -> Core.Coin
    -- ^ A function to estimate the fees.
    , csoInputGrouping     :: InputGrouping
    -- ^ A preference regarding input grouping.
    , csoExpenseRegulation :: ExpenseRegulation
    -- ^ A preference regarding expense regulation
    , csoDustThreshold     :: Core.Coin
    -- ^ Change addresses below the given threshold will be evicted
    -- from the created transaction. If you only want to remove change
    -- outputs equal to 0, set 'csoDustThreshold' to 0.
    }

-- | Creates new 'CoinSelectionOptions' using 'PreferGrouping' as default
-- 'InputGrouping' and 'SenderPaysFee' as default 'ExpenseRegulation'.
newOptions :: (Int -> NonEmpty Core.Coin -> Core.Coin)
           -> CoinSelectionOptions
newOptions estimateFee = CoinSelectionOptions {
      csoEstimateFee       = estimateFee
    , csoInputGrouping     = PreferGrouping
    , csoExpenseRegulation = SenderPaysFee
    , csoDustThreshold     = Core.mkCoin 0
    }

feeOptions :: CoinSelectionOptions -> FeeOptions Cardano
feeOptions CoinSelectionOptions{..} = FeeOptions{
      foExpenseRegulation = csoExpenseRegulation
    , foEstimate          = \numInputs outputs ->
                               case outputs of
                                 []   -> error "feeOptions: empty list"
                                 o:os -> csoEstimateFee numInputs (o :| os)
    }

{-------------------------------------------------------------------------------
  Building transactions
-------------------------------------------------------------------------------}

-- | Build a transaction
type MkTx m = NonEmpty (Core.TxIn, Core.TxOutAux) -- ^ Transaction inputs
           -> NonEmpty Core.TxOutAux              -- ^ Transaction outputs
           -> m (Either (CoinSelHardErr Cardano) Core.TxAux)

-- | Construct a standard transaction
--
-- " Standard " here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
mkStdTx :: (Monad m, Core.HasProtocolMagic)
        => (Core.Address -> Either (CoinSelHardErr Cardano) Core.SafeSigner)
        -> MkTx m
mkStdTx hdwSigners inps outs =
    return $ Core.makeMPubKeyTxAddrs hdwSigners (fmap repack inps) outs
  where
    -- Repack a utxo-derived tuple into a format suitable for
    -- 'TxOwnedInputs'.
    repack :: (Core.TxIn, Core.TxOutAux) -> (Core.TxOut, Core.TxIn)
    repack (txIn, aux) = (Core.toaOut aux, txIn)

{-------------------------------------------------------------------------------
  Coin selection policy top-level entry point
-------------------------------------------------------------------------------}

-- | Pick an element from the UTxO to cover any remaining fee
type PickUtxo m = Core.Coin  -- ^ Fee to still cover
               -> CoinSelT Core.Utxo (CoinSelHardErr Cardano) m
                    (Core.TxIn, Core.TxOutAux)

-- | Run coin selection
--
-- NOTE: Final UTxO is /not/ returned: coin selection runs /outside/ any wallet
-- database transactions. The right way to invoke this is to get a snapshot
-- of the wallet database, extract the UTxO, run coin selection, then submit
-- the resulting transaction. If a second transaction should be generated,
-- this whole process should be repeated. It is possible that by time the
-- transaction is submitted the wallet's UTxO has changed and the transaction
-- is no longer valid. This is by design; if it happens, coin selection should
-- just be run again on a new snapshot of the wallet DB.
runCoinSelT :: forall m. Monad m
            => CoinSelectionOptions
            -> m Core.Address
            -> PickUtxo m
            -> MkTx m
            -> CoinSelT Core.Utxo (CoinSelHardErr Cardano) m
                 [CoinSelResult Cardano]
            -> Core.Utxo
            -> m (Either (CoinSelHardErr Cardano) Core.TxAux)
runCoinSelT opts genChangeAddr pickUtxo mkTx policy utxo = do
    mSelection <- unwrapCoinSelT policy' utxo
    case mSelection of
      Left err -> return (Left err)
      Right ((cssWithDust, additionalUtxo), _utxo') -> do
        let css  = map (coinSelRemoveDust (csoDustThreshold opts)) cssWithDust
            inps = concatMap selectedEntries
                     (additionalUtxo : map coinSelInputs css)
            outs = map coinSelOutput css
        changeOuts <- forM (concatMap coinSelChange css) $ \change -> do
                        changeAddr <- genChangeAddr
                        return Core.TxOutAux {
                            Core.toaOut = Core.TxOut {
                                Core.txOutAddress = changeAddr
                              , Core.txOutValue   = change
                              }
                          }
        let allInps = case inps of
                        []   -> error "runCoinSelT: empty list of inputs"
                        i:is -> i :| is
            allOuts = case outs ++ changeOuts of
                        []   -> error "runCoinSelT: empty list of outputs"
                        o:os -> o :| os
        -- TODO: We should shuffle allOuts
        mkTx allInps allOuts
  where
    policy' :: CoinSelT Core.Utxo (CoinSelHardErr Cardano) m
                 ([CoinSelResult Cardano], SelectedUtxo Cardano)
    policy' = do
        css <- policy
        mapM_ validateOutput css
        adjustForFees (feeOptions opts) pickUtxo css

validateOutput :: Monad m
               => CoinSelResult Cardano
               -> CoinSelT utxo (CoinSelHardErr Cardano) m ()
validateOutput cs =
    when (Core.isRedeemAddress . Core.txOutAddress . Core.toaOut . coinSelOutput $ cs) $
      throwError $ CoinSelHardErrOutputIsRedeemAddress (coinSelOutput cs)

{-------------------------------------------------------------------------------
  Top-level entry points
-------------------------------------------------------------------------------}

-- | Random input selection policy
random :: forall m. MonadRandom m
       => CoinSelectionOptions
       -> m Core.Address
       -> MkTx m
       -> Int
       -> CoinSelPolicy Core.Utxo m Core.TxAux
random opts changeAddr mkTx maxInps =
      runCoinSelT opts changeAddr pickUtxo mkTx
    . Random.random Random.PrivacyModeOn maxInps
    . NE.toList
  where
    -- We ignore the size of the fee, and just pick randomly
    pickUtxo :: PickUtxo m
    pickUtxo _val = Random.findRandomOutput

-- | Largest-first input selection policy
--
-- NOTE: Not for production use.
largestFirst :: forall m. Monad m
             => CoinSelectionOptions
             -> m Core.Address
             -> MkTx m
             -> Int
             -> CoinSelPolicy Core.Utxo m Core.TxAux
largestFirst opts changeAddr mkTx maxInps =
      runCoinSelT opts changeAddr pickUtxo mkTx
    . LargestFirst.largestFirst maxInps
    . NE.toList
  where
    pickUtxo :: PickUtxo m
    pickUtxo val = search . Map.toList =<< get
      where
        search :: [(Core.TxIn, Core.TxOutAux)]
               -> CoinSelT Core.Utxo (CoinSelHardErr Cardano) m
                    (Core.TxIn, Core.TxOutAux)
        search [] = throwError CoinSelHardErrCannotCoverFee
        search ((i, o):ios)
          | Core.txOutValue (Core.toaOut o) >= val = return (i, o)
          | otherwise                              = search ios
