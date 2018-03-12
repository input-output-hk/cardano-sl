{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Pure functions for operations with transactions

module Pos.Client.Txp.Util
       (
       -- * Tx creation params
         InputSelectionPolicy (..)
       , defaultInputSelectionPolicy
       , PendingAddresses (..)

       -- * Tx creation
       , TxCreateMode
       , makeAbstractTx
       , runTxCreator
       , makePubKeyTx
       , makeMPubKeyTx
       , makeMPubKeyTxAddrs
       , makeMOfNTx
       , makeRedemptionTx
       , createGenericTx
       , createTx
       , createMTx
       , createMOfNTx
       , createRedemptionTx

       -- * Fees logic
       , txToLinearFee
       , computeTxFee

       -- * Additional datatypes
       , TxError (..)
       , isCheckedTxError
       , isNotEnoughMoneyTxError

       , TxOutputs
       , TxWithSpendings
       ) where

import           Universum

import           Control.Lens (makeLenses, (%=), (.=))
import           Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import           Data.Default (Default (..))
import           Data.Fixed (Fixed, HasResolution)
import qualified Data.HashSet as HS
import           Data.List (partition, tail)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Semigroup as S
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Data.Traversable (for)
import qualified Data.Vector as V
import           Formatting (bprint, build, sformat, stext, (%))
import           Serokell.Util (listJson)

import           Pos.Binary (biSize)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Core (Address, Coin, StakeholderId, TxFeePolicy (..), TxSizeLinear (..),
                           bvdTxFeePolicy, calculateTxSizeLinear, coinToInteger, integerToCoin,
                           isRedeemAddress, mkCoin, sumCoins, txSizeLinearMinValue,
                           unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Crypto (RedeemSecretKey, SafeSigner, SignTag (SignRedeemTx, SignTx),
                             deterministicKeyGen, fakeSigner, hash, redeemSign, redeemToPublic,
                             safeSign, safeToPublic)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.DB (MonadGState, gsAdoptedBVData)
import           Pos.Script (Script)
import           Pos.Script.Examples (multisigRedeemer, multisigValidator)
import           Pos.Txp (Tx (..), TxAux (..), TxFee (..), TxIn (..), TxInWitness (..), TxOut (..),
                          TxOutAux (..), TxSigData (..), Utxo)
import           Pos.Util.LogSafe (SecureLog, buildUnsecure)
import           Test.QuickCheck (Arbitrary (..), elements)

type TxInputs = NonEmpty TxIn
type TxOwnedInputs owner = NonEmpty (owner, TxIn)
type TxOutputs = NonEmpty TxOutAux
type TxWithSpendings = (TxAux, NonEmpty TxOut)

-- | List of addresses which are refered by at least one output of transaction
-- which is not yet confirmed i.e. detected in block.
newtype PendingAddresses = PendingAddresses (Set Address)
    deriving (Show, Monoid)

instance Buildable TxWithSpendings where
    build (txAux, neTxOut) =
        bprint ("("%build%", "%listJson%")") txAux neTxOut

-- This datatype corresponds to raw transaction.
data TxRaw = TxRaw
    { trInputs         :: !(TxOwnedInputs TxOut)
    -- ^ Selected inputs from Utxo
    , trOutputs        :: !TxOutputs
    -- ^ Output addresses of tx (without remaining output)
    , trRemainingMoney :: !Coin
    -- ^ Remaining money
    } deriving (Show)

data TxError =
      NotEnoughMoney !Coin
      -- ^ Parameter: how much more money is needed
    | NotEnoughAllowedMoney !Coin
      -- ^ Parameter: how much more money is needed and which available input addresses
      -- are present in output addresses set
    | FailedToStabilize
      -- ^ Parameter: how many attempts were performed
    | OutputIsRedeem !Address
      -- ^ One of the tx outputs is a redemption address
    | RedemptionDepleted
      -- ^ Redemption address has already been used
    | SafeSignerNotFound !Address
      -- ^ The safe signer at the specified address was not found
    | GeneralTxError !Text
      -- ^ Parameter: description of the problem
    deriving (Show, Generic)

isNotEnoughMoneyTxError :: TxError -> Bool
isNotEnoughMoneyTxError = \case
    NotEnoughMoney{}        -> True
    NotEnoughAllowedMoney{} -> True
    _                       -> False

instance Exception TxError

instance Buildable TxError where
    build (NotEnoughMoney coin) =
        bprint ("Transaction creation error: not enough money, need "%build%" more") coin
    build (NotEnoughAllowedMoney coin) =
        bprint ("Transaction creation error: not enough money on addresses which are not included \
                \in output addresses set, need "%build%" more") coin
    build FailedToStabilize =
        "Transaction creation error: failed to stabilize fee"
    build (OutputIsRedeem addr) =
        bprint ("Output address "%build%" is a redemption address") addr
    build RedemptionDepleted =
        bprint "Redemption address balance is 0"
    build (SafeSignerNotFound addr) =
        bprint ("Address "%build%" has no associated safe signer") addr
    build (GeneralTxError msg) =
        bprint ("Transaction creation error: "%stext) msg

isCheckedTxError :: TxError -> Bool
isCheckedTxError = \case
    NotEnoughMoney{}        -> True
    NotEnoughAllowedMoney{} -> True
    FailedToStabilize{}     -> False
    OutputIsRedeem{}        -> True
    RedemptionDepleted{}    -> True
    SafeSignerNotFound{}    -> True
    GeneralTxError{}        -> True

-----------------------------------------------------------------------------
-- Tx creation
-----------------------------------------------------------------------------

-- | Specifies the way Utxos are going to be grouped.
data InputSelectionPolicy
    = OptimizeForSecurity       -- ^ Spend everything from the address
    | OptimizeForHighThroughput -- ^ No grouping, prefer confirmed addresses
    deriving (Show, Eq, Enum, Bounded, Generic)

instance Buildable InputSelectionPolicy where
    build = \case
        OptimizeForSecurity       -> "securely"
        OptimizeForHighThroughput -> "high throughput"

instance Buildable (SecureLog InputSelectionPolicy) where
    build = buildUnsecure

instance Default InputSelectionPolicy where
    def = defaultInputSelectionPolicy

defaultInputSelectionPolicy :: InputSelectionPolicy
defaultInputSelectionPolicy = OptimizeForSecurity

instance Arbitrary InputSelectionPolicy where
    arbitrary = elements [minBound .. maxBound]

-- | Mode for creating transactions. We need to know fee policy.
type TxDistrMode m
     = ( MonadGState m
       , HasConfiguration
       )

type TxCreateMode m
    = ( TxDistrMode m
      , MonadAddresses m
      )

-- | Generic function to create a transaction, given desired inputs,
-- outputs and a way to construct witness from signature data
makeAbstractTx :: (owner -> TxSigData -> Either e TxInWitness)
               -> TxOwnedInputs owner
               -> TxOutputs
               -> Either e TxAux
makeAbstractTx mkWit txInputs outputs = do
  let
    tx = UncheckedTx (map snd txInputs) txOutputs txAttributes
    txOutputs = map toaOut outputs
    txAttributes = mkAttributes ()
    txSigData = TxSigData
        { txSigTxHash = hash tx
        }
  txWitness <- V.fromList . toList <$>
      for txInputs (\(addr, _) -> mkWit addr txSigData)
  pure $ TxAux tx txWitness

-- | Datatype which contains all data from DB which is necessary
-- to create transactions
data TxCreatorData = TxCreatorData
    { _tcdFeePolicy            :: !TxFeePolicy
    , _tcdInputSelectionPolicy :: !InputSelectionPolicy
    }

makeLenses ''TxCreatorData

-- | Transformer which holds data necessary for creating transactions
type TxCreator m = ReaderT TxCreatorData (ExceptT TxError m)

runTxCreator
    :: TxDistrMode m
    => InputSelectionPolicy
    -> TxCreator m a
    -> m (Either TxError a)
runTxCreator inputSelectionPolicy action = runExceptT $ do
    _tcdFeePolicy <- bvdTxFeePolicy <$> gsAdoptedBVData
    let _tcdInputSelectionPolicy = inputSelectionPolicy
    runReaderT action TxCreatorData{..}

-- | Like 'makePubKeyTx', but allows usage of different signers
makeMPubKeyTx
    :: (HasConfiguration)
    => (owner -> Either e SafeSigner)
    -> TxOwnedInputs owner
    -> TxOutputs
    -> Either e TxAux
makeMPubKeyTx getSs = makeAbstractTx mkWit
  where mkWit addr sigData =
          getSs addr <&> \ss ->
              PkWitness
              { twKey = safeToPublic ss
              , twSig = safeSign SignTx ss sigData
              }

-- | More specific version of 'makeMPubKeyTx' for convenience
makeMPubKeyTxAddrs
    :: (HasConfiguration)
    => (Address -> Either e SafeSigner)
    -> TxOwnedInputs TxOut
    -> TxOutputs
    -> Either e TxAux
makeMPubKeyTxAddrs hdwSigners = makeMPubKeyTx getSigner
  where
    getSigner (TxOut addr _) = hdwSigners addr

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx :: HasConfiguration => SafeSigner -> TxInputs -> TxOutputs -> TxAux
makePubKeyTx ss txInputs txOutputs = either absurd identity $
    makeMPubKeyTx (\_ -> Right ss) (map ((), ) txInputs) txOutputs

makeMOfNTx :: HasConfiguration => Script -> [Maybe SafeSigner] -> TxInputs -> TxOutputs -> TxAux
makeMOfNTx validator sks txInputs txOutputs = either absurd identity $
    makeAbstractTx mkWit (map ((), ) txInputs) txOutputs
  where
    mkWit _ sigData = Right $ ScriptWitness
            { twValidator = validator
            , twRedeemer = multisigRedeemer sigData sks
            }

makeRedemptionTx :: HasConfiguration => RedeemSecretKey -> TxInputs -> TxOutputs -> TxAux
makeRedemptionTx rsk txInputs txOutputs = either absurd identity $
    makeAbstractTx mkWit (map ((), ) txInputs) txOutputs
  where rpk = redeemToPublic rsk
        mkWit _ sigData = Right $ RedeemWitness
            { twRedeemKey = rpk
            , twRedeemSig = redeemSign SignRedeemTx rsk sigData
            }

-- | Helper for summing values of `TxOutAux`s
sumTxOutCoins :: NonEmpty TxOutAux -> Integer
sumTxOutCoins = sumCoins . map (txOutValue . toaOut)

integerToFee :: MonadError TxError m => Integer -> m TxFee
integerToFee =
    either (throwError . invalidFee) (pure . TxFee) . integerToCoin
  where
    invalidFee reason = GeneralTxError ("Invalid fee: " <> reason)

fixedToFee :: (MonadError TxError m, HasResolution a) => Fixed a -> m TxFee
fixedToFee = integerToFee . ceiling

type FlatUtxo = [(TxIn, TxOutAux)]
type InputPickingWay = Utxo -> TxOutputs -> Coin -> Either TxError FlatUtxo

-- TODO [CSM-526] Scatter on submodules

-------------------------------------------------------------------------
-- Simple inputs picking
-------------------------------------------------------------------------

data InputPickerState = InputPickerState
    { _ipsMoneyLeft        :: !Coin
    , _ipsAvailableOutputs :: !FlatUtxo
    }

makeLenses ''InputPickerState

type InputPicker = StateT InputPickerState (Either TxError)

plainInputPicker :: PendingAddresses -> InputPickingWay
plainInputPicker (PendingAddresses pendingAddrs) utxo _outputs moneyToSpent =
    evalStateT (pickInputs []) (InputPickerState moneyToSpent sortedUnspent)
  where
    onlyConfirmedInputs :: Set.Set Address -> (TxIn, TxOutAux) -> Bool
    onlyConfirmedInputs addrs (_, (TxOutAux (TxOut addr _))) = not (addr `Set.member` addrs)
    --
    -- NOTE (adinapoli, kantp) Under certain circumstances, it's still possible for the `confirmed` set
    -- to be exhausted and for the utxo to be picked from the `unconfirmed`, effectively allowing for the
    -- old "slow" behaviour which could create linear chains of dependent transactions which can then be
    -- submitted to relays and possibly fail to be accepted if they arrive in an out-of-order fashion,
    -- effectively piling up in the mempool of the edgenode and in need to be resubmitted.
    -- However, this policy significantly reduce the likelyhood of such edge case to happen, as for exchanges
    -- the `confirmed` set would tend to be quite big anyway.
    -- We should revisit such policy and its implications during a proper rewrite.
    --
    -- NOTE (adinapoli, kantp) There is another subtle corner case which involves such partitioning; it's now
    -- in theory (by absurd reasoning) for the `confirmed` set to contain only dust, which would yes involve a
    -- "high throughput" Tx but also a quite large one, bringing it closely to the "Toil too large" error
    -- (The same malady the @OptimiseForSecurity@ policy was affected by).
    sortedUnspent = confirmed ++ unconfirmed

    (confirmed, unconfirmed) =
      -- Give precedence to "confirmed" addresses.
      partition (onlyConfirmedInputs pendingAddrs)
                (sortOn (Down . txOutValue . toaOut . snd) (M.toList utxo))

    pickInputs :: FlatUtxo -> InputPicker FlatUtxo
    pickInputs inps = do
        moneyLeft <- use ipsMoneyLeft
        if moneyLeft == mkCoin 0
            then return inps
            else do
            mNextOut <- head <$> use ipsAvailableOutputs
            case mNextOut of
                Nothing -> throwError $ NotEnoughMoney moneyLeft
                Just inp@(_, (TxOutAux (TxOut {..}))) -> do
                    ipsMoneyLeft .= unsafeSubCoin moneyLeft (min txOutValue moneyLeft)
                    ipsAvailableOutputs %= tail
                    pickInputs (inp : inps)

-------------------------------------------------------------------------
-- Grouped inputs picking
-------------------------------------------------------------------------

-- | Group of unspent transaction outputs which belongs
-- to one address
data UtxoGroup = UtxoGroup
    { ugAddr       :: !Address
    , ugTotalMoney :: !Coin
    , ugUtxo       :: !(NonEmpty (TxIn, TxOutAux))
    } deriving (Show)

-- | Group unspent outputs by addresses
groupUtxo :: Utxo -> [UtxoGroup]
groupUtxo utxo =
    map mkUtxoGroup preUtxoGroups
  where
    futxo = M.toList utxo
    preUtxoGroups = NE.groupAllWith (txOutAddress . toaOut . snd) futxo
    mkUtxoGroup ugUtxo@(sample :| _) =
        let ugAddr = txOutAddress . toaOut . snd $ sample
            ugTotalMoney = unsafeIntegerToCoin . sumTxOutCoins $
                map snd ugUtxo
        in UtxoGroup {..}

data GroupedInputPickerState = GroupedInputPickerState
    { _gipsMoneyLeft             :: !Coin
    , _gipsAvailableOutputGroups :: ![UtxoGroup]
    }

makeLenses ''GroupedInputPickerState

type GroupedInputPicker = StateT GroupedInputPickerState (Either TxError)

groupedInputPicker :: InputPickingWay
groupedInputPicker utxo outputs moneyToSpent =
    evalStateT (pickInputs []) (GroupedInputPickerState moneyToSpent sortedGroups)
  where
    gUtxo = groupUtxo utxo
    outputAddrsSet = foldl' (flip HS.insert) mempty $
        map (txOutAddress . toaOut) outputs
    isOutputAddr = flip HS.member outputAddrsSet
    sortedGroups = sortOn (Down . ugTotalMoney) $
        filter (not . isOutputAddr . ugAddr) gUtxo
    disallowedInputGroups = filter (isOutputAddr . ugAddr) gUtxo
    disallowedMoney = sumCoins $ map ugTotalMoney disallowedInputGroups

    pickInputs :: FlatUtxo -> GroupedInputPicker FlatUtxo
    pickInputs inps = do
        moneyLeft <- use gipsMoneyLeft
        if moneyLeft == mkCoin 0
            then return inps
            else do
                mNextOutGroup <- head <$> use gipsAvailableOutputGroups
                case mNextOutGroup of
                    Nothing -> if disallowedMoney >= coinToInteger moneyLeft
                        then throwError $ NotEnoughAllowedMoney moneyLeft
                        else throwError $ NotEnoughMoney moneyLeft
                    Just UtxoGroup {..} -> do
                        gipsMoneyLeft .= unsafeSubCoin moneyLeft (min ugTotalMoney moneyLeft)
                        gipsAvailableOutputGroups %= tail
                        pickInputs (toList ugUtxo ++ inps)

-------------------------------------------------------------------------
-- Further logic
-------------------------------------------------------------------------

-- | Given filtered Utxo, desired outputs and fee size,
-- prepare correct inputs and outputs for transaction
-- (and tell how much to send to remaining address)
prepareTxRawWithPicker
    :: Monad m
    => InputPickingWay
    -> Utxo
    -> TxOutputs
    -> TxFee
    -> TxCreator m TxRaw
prepareTxRawWithPicker inputPicker utxo outputs (TxFee fee) = do
    mapM_ (checkIsNotRedeemAddr . txOutAddress . toaOut) outputs

    totalMoney <- sumTxOuts outputs
    when (totalMoney == mkCoin 0) $
        throwError $ GeneralTxError "Attempted to send 0 money"

    moneyToSpent <- case integerToCoin (sumCoins [totalMoney, fee]) of
        -- we don't care about exact number if user desires all money in the world
        Left _  -> throwError $ NotEnoughMoney maxBound
        Right c -> pure c

    futxo <- either throwError pure $ inputPicker utxo outputs moneyToSpent
    case nonEmpty futxo of
        Nothing       -> throwError $ GeneralTxError "Failed to prepare inputs!"
        Just inputsNE -> do
            totalTxAmount <- sumTxOuts $ map snd inputsNE
            let trInputs = map formTxInputs inputsNE
                trRemainingMoney = totalTxAmount `unsafeSubCoin` moneyToSpent
            let trOutputs = outputs
            pure TxRaw {..}
  where
    sumTxOuts = either (throwError . GeneralTxError) pure .
        integerToCoin . sumTxOutCoins
    formTxInputs (inp, TxOutAux txOut) = (txOut, inp)
    checkIsNotRedeemAddr outAddr =
        when (isRedeemAddress outAddr) $
            throwError $ OutputIsRedeem outAddr

prepareTxRaw
    :: Monad m
    => PendingAddresses
    -> Utxo
    -> TxOutputs
    -> TxFee
    -> TxCreator m TxRaw
prepareTxRaw pendingTx utxo outputs fee = do
    inputSelectionPolicy <- view tcdInputSelectionPolicy
    let inputPicker =
          case inputSelectionPolicy of
            OptimizeForHighThroughput -> plainInputPicker pendingTx
            OptimizeForSecurity       -> groupedInputPicker
    prepareTxRawWithPicker inputPicker utxo outputs fee

-- Returns set of tx outputs including change output (if it's necessary)
mkOutputsWithRem
    :: TxCreateMode m
    => AddrData m
    -> TxRaw
    -> TxCreator m TxOutputs
mkOutputsWithRem addrData TxRaw {..}
    | trRemainingMoney == mkCoin 0 = pure trOutputs
    | otherwise = do
        changeAddr <- lift . lift $ getNewAddress addrData
        let txOut = TxOut changeAddr trRemainingMoney
        pure $ TxOutAux txOut :| toList trOutputs

prepareInpsOuts
    :: TxCreateMode m
    => PendingAddresses
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOuts pendingTx utxo outputs addrData = do
    txRaw@TxRaw {..} <- prepareTxWithFee pendingTx utxo outputs
    outputsWithRem <- mkOutputsWithRem addrData txRaw
    pure (trInputs, outputsWithRem)

createGenericTx
    :: TxCreateMode m
    => PendingAddresses
    -> (TxOwnedInputs TxOut -> TxOutputs -> Either TxError TxAux)
    -> InputSelectionPolicy
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createGenericTx pendingTx creator inputSelectionPolicy utxo outputs addrData =
    runTxCreator inputSelectionPolicy $ do
        (inps, outs) <- prepareInpsOuts pendingTx utxo outputs addrData
        txAux <- either throwError return $ creator inps outs
        pure (txAux, map fst inps)

createGenericTxSingle
    :: TxCreateMode m
    => PendingAddresses
    -> (TxInputs -> TxOutputs -> Either TxError TxAux)
    -> InputSelectionPolicy
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createGenericTxSingle pendingTx creator = createGenericTx pendingTx (creator . map snd)

-- | Make a multi-transaction using given secret key and info for outputs.
-- Currently used for HD wallets only, thus `HDAddressPayload` is required
createMTx
    :: TxCreateMode m
    => PendingAddresses
    -> InputSelectionPolicy
    -> Utxo
    -> (Address -> Maybe SafeSigner)
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createMTx pendingTx groupInputs utxo hdwSigners outputs addrData =
    createGenericTx pendingTx (makeMPubKeyTxAddrs getSigner)
        groupInputs utxo outputs addrData
  where
    getSigner address =
        note (SafeSignerNotFound address) $
        hdwSigners address

-- | Make a multi-transaction using given secret key and info for
-- outputs.
createTx
    :: TxCreateMode m
    => PendingAddresses
    -> Utxo
    -> SafeSigner
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createTx pendingTx utxo ss outputs addrData =
    createGenericTxSingle pendingTx (\i o -> Right $ makePubKeyTx ss i o)
    OptimizeForSecurity utxo outputs addrData

-- | Make a transaction, using M-of-N script as a source
createMOfNTx
    :: TxCreateMode m
    => PendingAddresses
    -> Utxo
    -> [(StakeholderId, Maybe SafeSigner)]
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createMOfNTx pendingTx utxo keys outputs addrData =
    createGenericTxSingle pendingTx (\i o -> Right $ makeMOfNTx validator sks i o)
    OptimizeForSecurity utxo outputs addrData
  where
    ids = map fst keys
    sks = map snd keys
    m = length $ filter isJust sks
    validator = multisigValidator m ids

-- | Make a transaction for retrieving money from redemption address
createRedemptionTx
    :: TxCreateMode m
    => Utxo
    -> RedeemSecretKey
    -> TxOutputs
    -> m (Either TxError TxAux)
createRedemptionTx utxo rsk outputs =
    runTxCreator whetherGroupedInputs $ do
        TxRaw {..} <- prepareTxRaw mempty utxo outputs (TxFee $ mkCoin 0)
        let bareInputs = snd <$> trInputs
        pure $ makeRedemptionTx rsk bareInputs trOutputs
  where
    -- always spend redeem address fully
    whetherGroupedInputs = OptimizeForSecurity

-----------------------------------------------------------------------------
-- Fees logic
-----------------------------------------------------------------------------

-- | Helper function to reduce code duplication
withLinearFeePolicy
    :: Monad m
    => (TxSizeLinear -> TxCreator m a)
    -> TxCreator m a
withLinearFeePolicy action = view tcdFeePolicy >>= \case
    TxFeePolicyUnknown w _ -> throwError $ GeneralTxError $
        sformat ("Unknown fee policy, tag: "%build) w
    TxFeePolicyTxSizeLinear linearPolicy ->
        action linearPolicy

-- | Prepare transaction considering fees
prepareTxWithFee ::
       (HasConfiguration, MonadAddresses m)
    => PendingAddresses
    -> Utxo
    -> TxOutputs
    -> TxCreator m TxRaw
prepareTxWithFee pendingTx utxo outputs = withLinearFeePolicy $ \linearPolicy ->
    stabilizeTxFee pendingTx linearPolicy utxo outputs

-- | Compute, how much fees we should pay to send money to given
-- outputs
computeTxFee
    :: (HasConfiguration, MonadAddresses m)
    => PendingAddresses
    -> Utxo
    -> TxOutputs
    -> TxCreator m TxFee
computeTxFee pendingTx utxo outputs = do
    TxRaw {..} <- prepareTxWithFee pendingTx utxo outputs
    let outAmount = sumTxOutCoins trOutputs
        inAmount = sumCoins $ map (txOutValue . fst) trInputs
        remaining = coinToInteger trRemainingMoney
    integerToFee $ inAmount - outAmount - remaining

-- | Search such spendings that transaction's fee would be stable.
--
-- Stabilisation is simple iterative algorithm which performs
-- @ fee <- minFee( tx(fee) ) @ per iteration step.
-- It does *not* guarantee to find minimal possible fee, but is expected
-- to converge in O(|utxo|) steps, where @ utxo @ a set of addresses
-- encountered in utxo.
--
-- Alogrithm consists of two stages:
--
-- 1. Iterate until @ fee_{i+1} <= fee_i @.
-- It can last for no more than @ ~2 * |utxo| @ iterations. Really, let's
-- consider following cases:
--
--     * Number of used input addresses increased at i-th iteration, i.e.
--       @ |inputs(tx(fee_i))| > |inputs(tx(fee_{i-1}))| @,
--       which can happen no more than |utxo| times.
--
--     * Number of tx input addresses stayed the same, i.e.
--       @ |inputs(tx(fee_i))| = |inputs(tx(fee_{i-1}))| @.
--
--       If @ fee_i <= fee_{i-1} @ then stage 1 has already finished. Otherwise,
--       since inputs and outputs are picked deterministically, inputs and
--       outputs for @ tx(fee_i) @ and @ tx(fee_{i-1}) @ are the same and they
--       differ only in remainder amount. Since we assume @ fee_i > fee_{i-1} @,
--       then @ rem(tx(fee_i)) < rem(tx(fee_{i-1})) @ by evaluation method.
--       It leads to @ size(tx(fee_i)) <= size(tx(fee_{i-1})) @ and thus
--       @ minFee(tx(fee_i)) <= minFee(tx(fee_{i-1})) @,
--       i.e. @ fee_{i+1} <= fee_{i} @.
--
--     * Number if input addresses decreased.
--       Is may occur when fee increases more than on current remainder.
--       Is this case fee on next iteration would indeed decrease, because
--       size of single input is much greater than any fluctuations of
--       remainder size (in bytes).
--
-- In total, case (1) occurs no more than |utxo| times, case (2) is always
-- followed by case (1), and case (3) terminates current stage immediatelly,
-- thus stage 1 takes no more than, approximatelly, @ 2 * |utxo| @ iterations.
--
-- 2. Once we find such @ i @ for which @ fee_{i+1} <= fee_i @, we can return
-- @ tx(fee_i) @ as answer, but it may contain overestimated fee (which is still
-- valid).
-- To possibly find better solutions we iterate for several times more.
stabilizeTxFee
    :: forall m. (HasConfiguration, MonadAddresses m)
    => PendingAddresses
    -> TxSizeLinear
    -> Utxo
    -> TxOutputs
    -> TxCreator m TxRaw
stabilizeTxFee pendingTx linearPolicy utxo outputs = do
    minFee <- fixedToFee (txSizeLinearMinValue linearPolicy)
    mtx <- stabilizeTxFeeDo (False, firstStageAttempts) minFee
    case mtx of
        Nothing -> throwError FailedToStabilize
        Just tx -> pure $ tx & \(S.Min (S.Arg _ txRaw)) -> txRaw
  where
    firstStageAttempts = 2 * length utxo + 5
    secondStageAttempts = 10

    stabilizeTxFeeDo :: (Bool, Int)
                     -> TxFee
                     -> TxCreator m $ Maybe (S.ArgMin TxFee TxRaw)
    stabilizeTxFeeDo (_, 0) _ = pure Nothing
    stabilizeTxFeeDo (isSecondStage, attempt) expectedFee = do
        txRaw <- prepareTxRaw pendingTx utxo outputs expectedFee
        fakeChangeAddr <- lift . lift $ getFakeChangeAddress
        txMinFee <- txToLinearFee linearPolicy $
                    createFakeTxFromRawTx fakeChangeAddr txRaw

        let txRawWithFee = S.Min $ S.Arg expectedFee txRaw
        let iterateDo step = stabilizeTxFeeDo step txMinFee
        case expectedFee `compare` txMinFee of
            LT -> iterateDo (isSecondStage, attempt - 1)
            EQ -> pure (Just txRawWithFee)
            GT -> do
                let nextStep = (True, if isSecondStage then attempt - 1 else secondStageAttempts)
                futureRes <- iterateDo nextStep
                return $! Just txRawWithFee S.<> futureRes

-- | Calcucate linear fee from transaction's size
txToLinearFee
    :: MonadError TxError m
    => TxSizeLinear -> TxAux -> m TxFee
txToLinearFee linearPolicy =
    fixedToFee .
    calculateTxSizeLinear linearPolicy .
    biSize @TxAux

-- | Function is used to calculate intermediate fee amounts
-- when forming a transaction
createFakeTxFromRawTx :: HasConfiguration => Address -> TxRaw -> TxAux
createFakeTxFromRawTx fakeAddr TxRaw{..} =
    let fakeOutMB
            | trRemainingMoney == mkCoin 0 = Nothing
            | otherwise =
                Just $
                TxOutAux (TxOut fakeAddr trRemainingMoney)
        txOutsWithRem = maybe trOutputs (\remTx -> remTx :| toList trOutputs) fakeOutMB

        -- We create fake signers instead of safe signers,
        -- because safe signer requires passphrase
        -- but we don't want to reveal our passphrase to compute fee.
        -- Fee depends on size of tx in bytes, sign of a tx has the fixed size
        -- so we can use arbitrary signer.
        (_, fakeSK) = deterministicKeyGen "patakbardaqskovoroda228pva1488kk"
    in either absurd identity $ makeMPubKeyTxAddrs
           (\_ -> Right $ fakeSigner fakeSK)
           trInputs
           txOutsWithRem
