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
  , CoinSelFinalResult(..)
  , mkStdTx
    -- * Coin selection policies
  , random
  , largestFirst
    -- * Estimating fees
  , estimateCardanoFee
  , dummyAddrAttrSize
  , dummyTxAttrSize
    -- * Estimating transaction limits
  , estimateMaxTxInputs
  , estimateHardMaxTxInputs
    -- * Testing & internal use only
  , estimateSize
  ) where

import           Universum hiding (Sum (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Typeable (TypeRep, typeRep)

import           Pos.Binary.Class (LengthOf, Range (..), SizeOverride (..),
                     encode, szSimplify, szWithCtx, toLazyByteString)
import qualified Pos.Chain.Txp as Core
import qualified Pos.Client.Txp.Util as Core
import           Pos.Core (AddrAttributes, Coin (..), TxSizeLinear,
                     calculateTxSizeLinear)
import qualified Pos.Core as Core
import           Pos.Core.Attributes (Attributes)
import           Pos.Core.Txp (TxAux, TxIn, TxInWitness, TxOut, TxSigData)
import           Pos.Crypto (Signature)
import qualified Pos.Crypto as Core
import           Serokell.Data.Memory.Units (Byte, toBytes)

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

-- | Construct a standard transaction
--
-- " Standard " here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
mkStdTx :: Monad m
        => Core.ProtocolMagic
        -> (Core.Address -> Either CoinSelHardErr Core.SafeSigner)
        -> NonEmpty (Core.TxIn, Core.TxOutAux)
        -- ^ Selected inputs
        -> NonEmpty Core.TxOutAux
        -- ^ Selected outputs
        -> [Core.TxOutAux]
        -- ^ A list of change addresess, in the form of 'TxOutAux'(s).
        -> m (Either CoinSelHardErr Core.TxAux)
mkStdTx pm hdwSigners inps outs change = do
    let allOuts = foldl' (flip NE.cons) outs change
    return $ Core.makeMPubKeyTxAddrs pm hdwSigners (fmap repack inps) allOuts
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

data CoinSelFinalResult = CoinSelFinalResult {
      csrInputs  :: NonEmpty (Core.TxIn, Core.TxOutAux)
      -- ^ Picked inputs
    , csrOutputs :: NonEmpty Core.TxOutAux
      -- ^ Picked outputs
    , csrChange  :: [Core.Coin]
    }

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
            -> PickUtxo m
            -> (forall utxo. PickFromUtxo utxo
                  => NonEmpty (Output (Dom utxo))
                  -> CoinSelT utxo CoinSelHardErr m [CoinSelResult (Dom utxo)])
            -> CoinSelPolicy Core.Utxo m CoinSelFinalResult
runCoinSelT opts pickUtxo policy request utxo = do
    mSelection <- unwrapCoinSelT policy' utxo
    case mSelection of
      Left err -> return (Left err)
      Right ((cssWithDust, additionalUtxo), _utxo') -> do
        let css  = map (coinSelRemoveDust (csoDustThreshold opts)) cssWithDust
            inps = concatMap selectedEntries
                     (additionalUtxo : map coinSelInputs css)
            outs = map coinSelOutput css
        let allInps = case inps of
                        []   -> error "runCoinSelT: empty list of inputs"
                        i:is -> i :| is
            originalOuts = case outs of
                               []   -> error "runCoinSelT: empty list of outputs"
                               o:os -> o :| os
        -- TODO: We should shuffle allOuts
        return . Right $ CoinSelFinalResult allInps
                                            originalOuts
                                            (concatMap coinSelChange css)
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
       -> Word64          -- ^ Maximum number of inputs
       -> CoinSelPolicy Core.Utxo m CoinSelFinalResult
random opts maxInps =
      runCoinSelT opts pickUtxo
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
             -> Word64
             -> CoinSelPolicy Core.Utxo m CoinSelFinalResult
largestFirst opts maxInps =
      runCoinSelT opts pickUtxo
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


{-------------------------------------------------------------------------------
  Cardano-specific fee-estimation.
-------------------------------------------------------------------------------}

-- | Estimate the size of a transaction, in bytes.
estimateSize :: Byte     -- ^ Average size of @Attributes AddrAttributes@.
             -> Byte     -- ^ Size of transaction's @Attributes ()@.
             -> Int      -- ^ Number of inputs to the transaction.
             -> [Word64] -- ^ Coin value of each output to the transaction.
             -> Range Byte -- ^ Estimated size bounds of the resulting transaction.
estimateSize saa sta ins outs =
    -- This error case should not be possible unless the structure of `TxAux` changes
    -- in such a way that it depends on a new type with no override for `Bi`'s
    -- `encodedSizeExpr`. In that case, the size expression for `TxAux` will be symbolic,
    -- and cannot be simplified to a concrete size range. However, the `Bi` unit tests
    -- in `core` include a test that `TxAux`'s size reduces to a concrete range, and the
    -- range encloses the correct encoded size.
    --
    -- In other words, either the unit test in `core` gives a concrete range and this
    -- always yields a `Right`, or the unit test gives fails due to a symbolic range
    -- and this always yields a `Left`.
    case szSimplify (szWithCtx ctx (Proxy @TxAux)) of
        Left  sz    -> error ("Size estimate failed to simplify: " <> pretty sz)
        Right range -> range

  where

    ctx = sizeEstimateCtx
        -- Number of outputs
        & insert (Proxy @(LengthOf [TxOut])) (toSize (length outs))
        -- Average size of an encoded Coin for this transaction.
        & insert (Proxy @Coin) (toSize (avgCoinSize outs))
        -- Number of inputs.
        & insert (Proxy @(LengthOf [TxIn]))               (toSize ins)
        & insert (Proxy @(LengthOf (Vector TxInWitness))) (toSize ins)
        -- Set attribute sizes to reasonable dummy values.
        & insert (Proxy @(Attributes AddrAttributes)) (toSize (toBytes saa))
        & insert (Proxy @(Attributes ()))             (toSize (toBytes sta))

    insert k = Map.insert (typeRep k)

    avgCoinSize [] = 0
    avgCoinSize cs = case sum (map encodedCoinSize cs) `quotRem` length cs of
        (avg, 0) -> avg
        (avg, _) -> avg + 1

    encodedCoinSize = fromIntegral . LBS.length . toLazyByteString . encode . Coin

-- | Estimate the fee for a transaction that has @ins@ inputs
--   and @length outs@ outputs. The @outs@ lists holds the coin value
--   of each output.
--
--   NOTE: The average size of @Attributes AddrAttributes@ and
--         the transaction attributes @Attributes ()@ are both hard-coded
--         here with some (hopefully) realistic values.
estimateCardanoFee :: TxSizeLinear -> Int -> [Word64] -> Word64
estimateCardanoFee linearFeePolicy ins outs
    = round $ calculateTxSizeLinear linearFeePolicy
            $ hi $ estimateSize dummyAddrAttrSize dummyTxAttrSize ins outs

-- | Size to use for a value of type @Attributes AddrAttributes@ when estimating
--   encoded transaction sizes. The minimum possible value is 2.
dummyAddrAttrSize :: Byte
dummyAddrAttrSize = 16

-- | Size to use for a value of type @Attributes ()@ when estimating
--   encoded transaction sizes. The minimum possible value is 2.
dummyTxAttrSize :: Byte
dummyTxAttrSize = 2

-- | For a given transaction size, and sizes for @Attributes AddrAttributes@ and
--   @Attributes ()@, compute the maximum possible number of inputs a transaction
--   can have. The formula for this value is not linear, so we just do a binary
--   search here. A decent first guess makes this search relatively quick.
--
--   We use a conservative over-estimate for the encoded transaction sizes, so the
--   number of transaction inputs computed here is a lower bound on the true
--   maximum.
estimateMaxTxInputs
  :: Byte -- ^ Size of @Attributes AddrAttributes@
  -> Byte -- ^ Size of @Attributes ()@
  -> Byte -- ^ Maximum size of a transaction
  -> Word64
estimateMaxTxInputs addrAttrSize txAttrSize maxSize =
    fromIntegral (searchUp estSize maxSize 7)

  where
    estSize txins = hi $ estimateSize addrAttrSize txAttrSize txins [Core.maxCoinVal]

-- | For a given transaction size, and sizes for @Attributes AddrAttributes@ and
--   @Attributes ()@, compute the maximum possible number of inputs a transaction
--   can have, as in @estimateMaxTxInputs@. The difference is that this function
--   tries to find the absolute highest number of possible inputs, by minimizing the
--   size of the transaction. This gives a hard upper bound on the number of
--   possible inputs a transaction can have; by comparison, @estimateMaxTxInputs@
--   gives an upper bound on the number of inputs you can put into a transaction,
--   if you do not have /a priori/ control over the size of those inputs.
estimateHardMaxTxInputs
  :: Byte -- ^ Size of @Attributes AddrAttributes@
  -> Byte -- ^ Size of @Attributes ()@
  -> Byte -- ^ Maximum size of a transaction
  -> Word64
estimateHardMaxTxInputs addrAttrSize txAttrSize maxSize =
    fromIntegral (searchUp estSize maxSize 7)

  where
    estSize txins = lo $ estimateSize addrAttrSize txAttrSize txins [minBound]

-- | Substitutions for certain sizes and lengths in the size estimates.
sizeEstimateCtx :: Map TypeRep SizeOverride
sizeEstimateCtx = Map.fromList
      -- For this estimate, assume all input witnesses use the `PkWitness` constructor.
    [ (typeRep (Proxy @TxInWitness)           , SelectCases ["PkWitness"])

    -- The magic number 66 is the encoded size of a `TxSigData` signature:
    -- 64 bytes for the payload, plus two bytes of ByteString overhead.
    , (typeRep (Proxy @(Signature TxSigData)) , SizeConstant 66)
    ]

-- | Helper function for creating size constants.
toSize :: Integral a => a -> SizeOverride
toSize = SizeConstant . fromIntegral

-- | For a monotonic @f@ and a value @y@, @searchUp f y k@ will find the
--   largest @x@ such that @f x <= y@ by walking up from zero using steps
--   of size @2^k@, reduced by a factor of 2 whenever we overshoot the target.
searchUp :: (Integral a, Ord b) => (a -> b) -> b -> a -> a
searchUp f y k = go 0 (2^k)
  where
    go x step = case compare (f x) y of
        LT -> go (x + step) step
        EQ -> x
        GT | step == 1 -> x - 1
           | otherwise -> let step' = step `div` 2
                          in go (x - step') step'
