{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Kernel.CoinSelection.FromGeneric (
    -- * Instantiation of the generic framework
    Cardano
  , Size(..)
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

import           Universum hiding (Sum (..))

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import qualified Pos.Client.Txp.Util as Core
import qualified Pos.Core as Core
import qualified Pos.Crypto as Core
import qualified Pos.Txp as Core

import           Cardano.Wallet.Kernel.CoinSelection.Generic
import           Cardano.Wallet.Kernel.CoinSelection.Generic.Fees
import           Cardano.Wallet.Kernel.CoinSelection.Generic.Grouped
import qualified Cardano.Wallet.Kernel.CoinSelection.Generic.LargestFirst as LargestFirst
import qualified Cardano.Wallet.Kernel.CoinSelection.Generic.Random as Random

{-------------------------------------------------------------------------------
  Type class instances
-------------------------------------------------------------------------------}

data Cardano

instance IsValue Core.Coin where
  valueZero   = Core.mkCoin 0
  valueAdd    = Core.addCoin
  valueSub    = Core.subCoin
  valueDist   = \  a b -> if a < b then b `Core.unsafeSubCoin` a
                                   else a `Core.unsafeSubCoin` b
  valueRatio  = \  a b -> coinToDouble a / coinToDouble b
  valueAdjust = \r d a -> coinFromDouble r (d * coinToDouble a)

instance CoinSelDom Cardano where
  type    Input     Cardano = Core.TxIn
  type    Output    Cardano = Core.TxOutAux
  type    UtxoEntry Cardano = (Core.TxIn, Core.TxOutAux)
  type    Value     Cardano = Core.Coin
  newtype Size      Cardano = Size Word64

  outVal    = Core.txOutValue   . Core.toaOut
  outSubFee = \(Fee v) o -> outSetVal o <$> valueSub (outVal o) v
     where
       outSetVal o v = o {Core.toaOut = (Core.toaOut o) {Core.txOutValue = v}}

instance HasAddress Cardano where
  type Address Cardano = Core.Address

  outAddr = Core.txOutAddress . Core.toaOut

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

instance StandardDom Cardano
instance StandardUtxo Core.Utxo

instance PickFromUtxo Core.Utxo where
  type Dom Core.Utxo = Cardano
  -- Use default implementations

instance CanGroup Core.Utxo where
  -- Use default implementations

{-------------------------------------------------------------------------------
  Coin selection options
-------------------------------------------------------------------------------}

-- | Grouping policy
--
-- Grouping refers to the fact that when an output to a certain address is spent,
-- /all/ outputs to that same address in the UTxO must be spent.
--
-- NOTE: Grouping is /NOT/ suitable for exchange nodes, which often have many
-- outputs to the same address. It should only be used for end users. For
-- this reason we don't bother caching the grouped UTxO, but reconstruct it
-- on each call to coin selection; the cost of this reconstruction is @O(n)@
-- in the size of the UTxO.

-- See also 'GroupedUtxo'
data InputGrouping =
      -- | Require grouping
      --
      -- We cannot recover from coin selection failures due to grouping.
      RequireGrouping

      -- | Ignore grouping
      --
      -- NOTE: MUST BE USED FOR EXCHANGE NODES.
    | IgnoreGrouping

      -- | Prefer grouping if possible, but retry coin selection without
      -- grouping when needed.
    | PreferGrouping

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

-- | Creates new 'CoinSelectionOptions' using 'NoGrouping' as default
-- 'InputGrouping' and 'SenderPaysFee' as default 'ExpenseRegulation'.
newOptions :: (Int -> NonEmpty Core.Coin -> Core.Coin)
           -> CoinSelectionOptions
newOptions estimateFee = CoinSelectionOptions {
      csoEstimateFee       = estimateFee
    , csoInputGrouping     = IgnoreGrouping
    , csoExpenseRegulation = SenderPaysFee
    , csoDustThreshold     = Core.mkCoin 0
    }

feeOptions :: CoinSelectionOptions -> FeeOptions Cardano
feeOptions CoinSelectionOptions{..} = FeeOptions{
      foExpenseRegulation = csoExpenseRegulation
    , foEstimate = \numInputs outputs ->
                      case outputs of
                        []   -> error "feeOptions: empty list"
                        o:os -> Fee $ csoEstimateFee numInputs (o :| os)
    }

{-------------------------------------------------------------------------------
  Building transactions
-------------------------------------------------------------------------------}

-- | Build a transaction
type MkTx m = NonEmpty (Core.TxIn, Core.TxOutAux) -- ^ Transaction inputs
           -> NonEmpty Core.TxOutAux              -- ^ Transaction outputs
           -> m (Either CoinSelHardErr Core.TxAux)

-- | Construct a standard transaction
--
-- " Standard " here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
mkStdTx :: Monad m
        => Core.ProtocolMagic
        -> (Core.Address -> Either CoinSelHardErr Core.SafeSigner)
        -> MkTx m
mkStdTx pm hdwSigners inps outs =
    return $ Core.makeMPubKeyTxAddrs pm hdwSigners (fmap repack inps) outs
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
               -> CoinSelT Core.Utxo CoinSelHardErr m (Core.TxIn, Core.TxOutAux)

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
            -> (forall utxo. PickFromUtxo utxo
                  => NonEmpty (Output (Dom utxo))
                  -> CoinSelT utxo CoinSelHardErr m [CoinSelResult (Dom utxo)])
            -> CoinSelPolicy Core.Utxo m Core.TxAux
runCoinSelT opts genChangeAddr pickUtxo mkTx policy request utxo = do
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
    policy' :: CoinSelT Core.Utxo CoinSelHardErr m
                 ([CoinSelResult Cardano], SelectedUtxo Cardano)
    policy' = do
        mapM_ validateOutput request
        css <- intInputGrouping (csoInputGrouping opts)
        -- We adjust for fees /after/ potentially dealing with grouping
        -- Since grouping only affects the inputs we select, this makes no
        -- difference.
        adjustForFees (feeOptions opts) pickUtxo css

    intInputGrouping :: InputGrouping
                     -> CoinSelT Core.Utxo CoinSelHardErr m [CoinSelResult Cardano]
    intInputGrouping RequireGrouping = grouped
    intInputGrouping IgnoreGrouping  = ungrouped
    intInputGrouping PreferGrouping  = grouped `catchError` \_ -> ungrouped

    ungrouped :: CoinSelT Core.Utxo CoinSelHardErr m [CoinSelResult Cardano]
    ungrouped = policy request

    grouped :: CoinSelT Core.Utxo CoinSelHardErr m [CoinSelResult Cardano]
    grouped = mapCoinSelUtxo groupUtxo underlyingUtxo $
                map fromGroupedResult <$> policy request'
      where
        -- We construct a single group for each output of the request
        -- This means that the coin selection policy will get the opportunity
        -- to create change outputs for each output, rather than a single
        -- change output for the entire transaction.
        request' :: NonEmpty (Grouped Core.TxOutAux)
        request' = fmap (Group . (:[])) request

-- | Translate out of the result of calling the policy on the grouped UTxO
--
-- * Since the request we submit is always a singleton output, we can be sure
--   that request as listed must be a singleton group; since the policy itself
--   sets 'coinSelOutput == coinSelRequest', this holds also for the output.
--   (TODO: It would be nice to express this more clearly in the types.)
-- * If we have one or more change outputs, they are of a size equal to that
--   single request output; the number here is determined by the policy
--   (which created one or more change outputs), and unrelated to the grouping.
--   We just leave the number of change outputs the same.
-- * The only part where grouping really plays a role is in the inputs we
--   selected; we just flatten this list.
fromGroupedResult :: forall dom.
                     CoinSelResult (Grouped dom) -> CoinSelResult dom
fromGroupedResult CoinSelResult{ coinSelRequest = Group [req]
                               , coinSelOutput  = Group [out]
                               , ..
                               } = CoinSelResult {
      coinSelRequest = req
    , coinSelOutput  = out
    , coinSelChange  = map getSum coinSelChange
    , coinSelInputs  = flatten coinSelInputs
    }
  where
    flatten :: SelectedUtxo (Grouped dom) -> SelectedUtxo dom
    flatten SelectedUtxo{..} = SelectedUtxo{
          selectedEntries = concatMap getGroup selectedEntries
        , selectedBalance = getSum selectedBalance
        , selectedSize    = groupSize selectedSize
        }
fromGroupedResult _ = error "fromGroupedResult: unexpected input"

validateOutput :: Monad m
               => Core.TxOutAux
               -> CoinSelT utxo CoinSelHardErr m ()
validateOutput out =
    when (Core.isRedeemAddress . Core.txOutAddress . Core.toaOut $ out) $
      throwError $ CoinSelHardErrOutputIsRedeemAddress out

{-------------------------------------------------------------------------------
  Top-level entry points
-------------------------------------------------------------------------------}

-- | Random input selection policy
random :: forall m. MonadRandom m
       => CoinSelectionOptions
       -> m Core.Address  -- ^ Generate change address
       -> MkTx m          -- ^ Build and sign transaction
       -> Word64          -- ^ Maximum number of inputs
       -> CoinSelPolicy Core.Utxo m Core.TxAux
random opts changeAddr mkTx maxInps =
      runCoinSelT opts changeAddr pickUtxo mkTx
    $ Random.random Random.PrivacyModeOn maxInps . NE.toList
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
             -> Word64
             -> CoinSelPolicy Core.Utxo m Core.TxAux
largestFirst opts changeAddr mkTx maxInps =
      runCoinSelT opts changeAddr pickUtxo mkTx
    $ LargestFirst.largestFirst maxInps . NE.toList
  where
    pickUtxo :: PickUtxo m
    pickUtxo val = search . Map.toList =<< get
      where
        search :: [(Core.TxIn, Core.TxOutAux)]
               -> CoinSelT Core.Utxo CoinSelHardErr m (Core.TxIn, Core.TxOutAux)
        search [] = throwError CoinSelHardErrCannotCoverFee
        search ((i, o):ios)
          | Core.txOutValue (Core.toaOut o) >= val = return (i, o)
          | otherwise                              = search ios
