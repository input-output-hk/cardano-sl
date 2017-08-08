{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure functions for operations with transactions

module Pos.Client.Txp.Util
       (
       -- * Tx creation
         TxCreateMode
       , makeAbstractTx
       , overrideTxOutDistrBoot
       , overrideTxDistrBoot
       , makePubKeyTx
       , makeMPubKeyTx
       , makeMOfNTx
       , makeRedemptionTx
       , createTx
       , createMTx
       , createMOfNTx
       , createRedemptionTx

       -- * Fees logic
       , computeTxFee

       -- * Additional datatypes
       , TxError (..)
       ) where

import           Control.Lens             (makeLenses, (%=), (.=))
import           Control.Monad.Except     (ExceptT, MonadError (throwError), runExceptT)
import           Control.Monad.State      (StateT (..), evalStateT)
import qualified Data.HashMap.Strict      as HM
import           Data.List                (tail)
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map                 as M
import qualified Data.Text.Buildable
import qualified Data.Vector              as V
import           Ether.Internal           (HasLens (..))
import           Formatting               (bprint, build, sformat, stext, (%))
import           Universum

import           Pos.Binary               (biSize)
import           Pos.Context              (GenesisStakeholders, genesisStakeholdersM)
import           Pos.Core                 (AddressIgnoringAttributes (AddressIA),
                                           TxFeePolicy (..), TxSizeLinear, bvdTxFeePolicy,
                                           calculateTxSizeLinear, integerToCoin,
                                           integerToCoin, siEpoch, unsafeAddCoin,
                                           unsafeGetCoin, unsafeSubCoin)
import           Pos.Crypto               (PublicKey, RedeemSecretKey, SafeSigner,
                                           SignTag (SignTxIn), deterministicKeyGen,
                                           fakeSigner, hash, redeemSign, redeemToPublic,
                                           safeSign, safeToPublic)
import           Pos.Data.Attributes      (mkAttributes)
import           Pos.DB                   (MonadGState, gsAdoptedBVData, gsIsBootstrapEra)
import           Pos.Genesis              (genesisSplitBoot)
import           Pos.Script               (Script)
import           Pos.Script.Examples      (multisigRedeemer, multisigValidator)
import           Pos.Slotting.Class       (MonadSlots (getCurrentSlotBlocking))
import           Pos.Txp                  (Tx (..), TxAux (..), TxDistribution (..),
                                           TxFee (..), TxIn (..), TxInWitness (..),
                                           TxOut (..), TxOutAux (..), TxOutDistribution,
                                           TxSigData (..), Utxo)
import           Pos.Types                (Address, Coin, mkCoin, sumCoins)

import           Pos.Client.Txp.Addresses (MonadAddresses (..))

type TxInputs = NonEmpty TxIn
type TxOwnedInputs owner = NonEmpty (owner, TxIn)
type TxOutputs = NonEmpty TxOutAux
type TxWithSpendings = (TxAux, NonEmpty TxOut)

-- This datatype corresponds to raw transaction.
data TxRaw = TxRaw
    { trInputs    :: !(TxOwnedInputs TxOut)
    -- ^ Selected inputs from Utxo
    , trOutputs   :: !TxOutputs
    -- ^ Output addresses of tx (without remaing output)
    , trRemaining :: !Coin
    -- ^ Remaining money
    }

data TxError
    = TxError { unTxError :: !Text }
    deriving (Show, Generic)

instance Exception TxError

instance Buildable TxError where
    build (TxError msg) = bprint ("Transaction creation error ("%stext%")") msg

throwTxError
    :: MonadError TxError m
    => Text -> m a
throwTxError = throwError . TxError

-----------------------------------------------------------------------------
-- Tx creation
-----------------------------------------------------------------------------

-- | Mode for creating transactions. We need to know the bootstrap era
-- status and have access to generic stakeholders to distribute
-- txdistr accordingly.
type TxDistrMode ctx m
     = ( MonadGState m
       , MonadReader ctx m
       , MonadSlots m
       , HasLens GenesisStakeholders ctx GenesisStakeholders
       )

type TxCreateMode ctx m
    = ( TxDistrMode ctx m
      , MonadAddresses m
      )

-- | Generic function to create a transaction, given desired inputs,
-- outputs and a way to construct witness from signature data
makeAbstractTx :: (owner -> TxSigData -> TxInWitness)
               -> TxOwnedInputs owner
               -> TxOutputs
               -> TxAux
makeAbstractTx mkWit txInputs outputs =
    TxAux
    { taTx = UnsafeTx (map snd txInputs) txOutputs txAttributes
    , taWitness = txWitness
    , taDistribution = txDist
    }
  where
    txOutputs = map toaOut outputs
    txAttributes = mkAttributes ()
    txOutHash = hash txOutputs
    txDist = TxDistribution (map toaDistr outputs)
    txDistHash = hash txDist
    txWitness = V.fromList $ toList $ txInputs <&>
        \(addr, txIn) -> mkWit addr $ makeTxSigData txIn
    makeTxSigData txIn =
        TxSigData
        { txSigInput = txIn
        , txSigOutsHash = txOutHash
        , txSigDistrHash = txDistHash
        }

-- | Overrides 'txDistr' with correct ones (according to the boot era
-- stake distribution) or leaves it as it is if in post-boot era.
overrideTxOutDistrBoot
    :: TxDistrMode ctx m
    => Coin
    -> TxOutDistribution
    -> ExceptT TxError m TxOutDistribution
overrideTxOutDistrBoot c oldDistr = do
    -- Blocking here should be fine for now (@volhovm)
    -- 1. Code in tx generator must have current slot.
    -- 2. Code in wallet will block on "synchronizing" on the
    --    frontend so it's fine too.
    epoch <- siEpoch <$> lift getCurrentSlotBlocking
    bootEra <- lift $ gsIsBootstrapEra epoch
    genStakeholders <- toList <$> genesisStakeholdersM
    if not bootEra
      then pure oldDistr
      else do
          when (unsafeGetCoin c < fromIntegral (length genStakeholders)) $
               throwTxError $
               sformat ("Can't spend "%build%" coins: amount is too small for boot "%
                        " era and can't be distributed among genStakeholders") c
          -- TODO CSL-1351 boot stakeholders' weights are not used
          pure $ genesisSplitBoot (HM.fromList $ map (,1) genStakeholders) c

-- | Same as 'overrideTxOutDistrBoot' but changes 'TxOutputs' all at once
overrideTxDistrBoot
    :: TxDistrMode ctx m
    => TxOutputs -> ExceptT TxError m TxOutputs
overrideTxDistrBoot outputs = do
    forM outputs $ \TxOutAux{..} -> do
        newStakeDistr <- overrideTxOutDistrBoot (txOutValue toaOut) toaDistr
        pure $ TxOutAux toaOut newStakeDistr

-- | Like 'makePubKeyTx', but allows usage of different signers
makeMPubKeyTx
    :: (owner -> SafeSigner)
    -> TxOwnedInputs owner
    -> TxOutputs
    -> TxAux
makeMPubKeyTx getSs = makeAbstractTx mkWit
  where mkWit addr sigData =
          let ss = getSs addr
          in PkWitness
              { twKey = safeToPublic ss
              , twSig = safeSign SignTxIn ss sigData
              }

-- | More specific version of 'makeMPubKeyTx' for convenience
makeMPubKeyTxAddrs
    :: NonEmpty (SafeSigner, Address)
    -> TxOwnedInputs TxOut
    -> TxOutputs
    -> TxAux
makeMPubKeyTxAddrs hdwSigners = makeMPubKeyTx getSigner
  where
    signers = HM.fromList . toList $
        map (swap . second AddressIA) hdwSigners
    getSigner (TxOut addr _) =
        fromMaybe (error "Requested signer for unknown address") $
        HM.lookup (AddressIA addr) signers

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx :: SafeSigner -> TxInputs -> TxOutputs -> TxAux
makePubKeyTx ss txInputs =
    makeMPubKeyTx (const ss) (map ((), ) txInputs)

makeMOfNTx :: Script -> [Maybe SafeSigner] -> TxInputs -> TxOutputs -> TxAux
makeMOfNTx validator sks txInputs = makeAbstractTx mkWit (map ((), ) txInputs)
  where mkWit _ sigData = ScriptWitness
            { twValidator = validator
            , twRedeemer = multisigRedeemer sigData sks
            }

makeRedemptionTx :: RedeemSecretKey -> TxInputs -> TxOutputs -> TxAux
makeRedemptionTx rsk txInputs = makeAbstractTx mkWit (map ((), ) txInputs)
  where rpk = redeemToPublic rsk
        mkWit _ sigData = RedeemWitness
            { twRedeemKey = rpk
            , twRedeemSig = redeemSign rsk sigData
            }

type FlatUtxo = [(TxIn, TxOutAux)]

data InputPickerState = InputPickerState
    { _ipsMoneyLeft        :: !Coin
    , _ipsAvailableOutputs :: !FlatUtxo
    }

makeLenses ''InputPickerState

type InputPicker = StateT InputPickerState (Either TxError)

-- | Given filtered Utxo, desired outputs and fee size,
-- prepare correct inputs and outputs for transaction
-- (and tell how much to send to remaining address)
prepareTxRaw
    :: MonadError TxError m
    => Utxo
    -> TxOutputs
    -> TxFee
    -> m TxRaw
prepareTxRaw utxo outputs (TxFee fee) = do
    totalMoney <- sumTxOuts outputs
    when (totalMoney == mkCoin 0) $
        throwTxError "Attempted to send 0 money"

    let totalMoneyWithFee = totalMoney `unsafeAddCoin` fee
    futxo <- either throwError pure $
        evalStateT (pickInputs []) (InputPickerState totalMoneyWithFee sortedUnspent)
    case nonEmpty futxo of
        Nothing       -> throwTxError "Failed to prepare inputs!"
        Just inputsNE -> do
            totalTxAmount <- sumTxOuts $ map snd inputsNE
            let trInputs = map formTxInputs inputsNE
                trOutputs = outputs
                trRemaining = totalTxAmount `unsafeSubCoin` totalMoneyWithFee
            pure TxRaw {..}
  where
    sumTxOuts = either throwTxError pure .
        integerToCoin . sumCoins . map (txOutValue . toaOut)
    allUnspent = M.toList utxo
    sortedUnspent =
        sortOn (Down . txOutValue . toaOut . snd) allUnspent

    pickInputs :: FlatUtxo -> InputPicker FlatUtxo
    pickInputs inps = do
        moneyLeft <- use ipsMoneyLeft
        if moneyLeft == mkCoin 0
            then return inps
            else do
                mNextOut <- head <$> use ipsAvailableOutputs
                case mNextOut of
                    Nothing -> throwTxError $
                        sformat ("Not enough money to send (need "%build%" coins more)") moneyLeft
                    Just inp@(_, (TxOutAux (TxOut {..}) _)) -> do
                        ipsMoneyLeft .= unsafeSubCoin moneyLeft (min txOutValue moneyLeft)
                        ipsAvailableOutputs %= tail
                        pickInputs (inp : inps)

    formTxInputs (inp, TxOutAux txOut _) = (txOut, inp)

-- | Returns set of tx outputs including change output (if it's necessary)
mkOutputsWithRem
    :: MonadAddresses m
    => AddrData m
    -> TxRaw
    -> m TxOutputs
mkOutputsWithRem addrData TxRaw {..}
    | trRemaining == mkCoin 0 = pure trOutputs
    | otherwise = do
          changeAddr <- getNewAddress addrData
          pure $ (TxOutAux (TxOut changeAddr trRemaining) []) :| toList trOutputs

prepareInpsOuts
    :: TxCreateMode ctx m
    => Utxo
    -> TxOutputs
    -> AddrData m
    -> ExceptT TxError m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOuts utxo outputs addrData = do
    txRaw@TxRaw {..} <- prepareTxWithFee utxo outputs
    outputsWithRem <- lift $ mkOutputsWithRem addrData txRaw
    properOutputs <- overrideTxDistrBoot outputsWithRem
    pure (trInputs, properOutputs)

createGenericTx
    :: TxCreateMode ctx m
    => (TxOwnedInputs TxOut -> TxOutputs -> TxAux)
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> ExceptT TxError m TxWithSpendings
createGenericTx creator utxo outputs addrData = do
    (inps, outs) <- prepareInpsOuts utxo outputs addrData
    pure (creator inps outs, map fst inps)

createGenericTxSingle
    :: TxCreateMode ctx m
    => (TxInputs -> TxOutputs -> TxAux)
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> ExceptT TxError m TxWithSpendings
createGenericTxSingle creator = createGenericTx (creator . map snd)

-- | Make a multi-transaction using given secret key and info for outputs.
-- Currently used for HD wallets only, thus `HDAddressPayload` is required
createMTx
    :: TxCreateMode ctx m
    => Utxo
    -> NonEmpty (SafeSigner, Address)
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createMTx utxo hdwSigners outputs addrData = runExceptT $
    createGenericTx (makeMPubKeyTxAddrs hdwSigners)
    utxo outputs addrData

-- | Make a multi-transaction using given secret key and info for
-- outputs.
createTx
    :: TxCreateMode ctx m
    => Utxo
    -> SafeSigner
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createTx utxo ss outputs addrData = runExceptT $
    createGenericTxSingle (makePubKeyTx ss)
    utxo outputs addrData

-- | Make a transaction, using M-of-N script as a source
createMOfNTx
    :: TxCreateMode ctx m
    => Utxo
    -> [(PublicKey, Maybe SafeSigner)]
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createMOfNTx utxo keys outputs addrData = runExceptT $
    createGenericTxSingle (makeMOfNTx validator sks)
    utxo outputs addrData
  where
    pks = map fst keys
    sks = map snd keys
    m = length $ filter isJust sks
    validator = multisigValidator m pks

-- | Make a transaction for retrieving money from redemption address
createRedemptionTx
    :: TxDistrMode ctx m
    => Utxo
    -> RedeemSecretKey
    -> TxOutputs
    -> m (Either TxError TxAux)
createRedemptionTx utxo rsk outputs = runExceptT $ do
    TxRaw {..} <- prepareTxRaw utxo outputs (TxFee $ mkCoin 0)
    let bareInputs = snd <$> trInputs
    properOutputs <- overrideTxDistrBoot trOutputs
    pure $ makeRedemptionTx rsk bareInputs properOutputs

-----------------------------------------------------------------------------
-- Fees logic
-----------------------------------------------------------------------------

-- | Helper function to reduce code duplication
withLinearFeePolicy
    :: (TxDistrMode ctx m, MonadError TxError m)
    => (TxSizeLinear -> m a)
    -> m a
withLinearFeePolicy action = bvdTxFeePolicy <$> gsAdoptedBVData >>= \case
    TxFeePolicyUnknown w _ -> throwTxError $
        sformat ("Unknown fee policy, tag: "%build) w
    TxFeePolicyTxSizeLinear linearPolicy ->
        action linearPolicy

-- | Prepare transaction considering fees
prepareTxWithFee
    :: TxDistrMode ctx m
    => Utxo
    -> TxOutputs
    -> ExceptT TxError m TxRaw
prepareTxWithFee utxo outputs = withLinearFeePolicy $ \linearPolicy ->
    stabilizeTxFee linearPolicy utxo outputs

-- | Compute, how much fees we should pay to send money to given
-- outputs
computeTxFee
    :: TxDistrMode ctx m
    => Utxo
    -> TxOutputs
    -> ExceptT TxError m TxFee
computeTxFee utxo outputs = withLinearFeePolicy $ \linearPolicy -> do
    txAux <- createFakeTxFromRawTx <$>
             stabilizeTxFee linearPolicy utxo outputs
    txToLinearFee linearPolicy txAux

-- | Search such spendings that transaction's fee would be stable.
stabilizeTxFee
    :: forall m. MonadError TxError m
    => TxSizeLinear
    -> Utxo
    -> TxOutputs
    -> m TxRaw
stabilizeTxFee linearPolicy utxo outputs =
    stabilizeTxFeeDo 5 (TxFee $ mkCoin 0)
  where
    stabilizeTxFeeDo :: Int -> TxFee -> m TxRaw
    stabilizeTxFeeDo 0 _ = throwTxError "Couldn't stabilize tx fee after 5 attempts"
    stabilizeTxFeeDo attempt expectedFee = do
        txRaw <- prepareTxRaw utxo outputs expectedFee
        txFee <- txToLinearFee linearPolicy $
                 createFakeTxFromRawTx txRaw
        if expectedFee == txFee
            then pure txRaw
            else stabilizeTxFeeDo (attempt - 1) txFee

-- | Calcucate linear fee from transaction's size
txToLinearFee
    :: MonadError TxError m
    => TxSizeLinear -> TxAux -> m TxFee
txToLinearFee linearPolicy =
    either throwError pure .
    bimap invalidFee TxFee .
    integerToCoin .
    ceiling .
    calculateTxSizeLinear linearPolicy .
    biSize @TxAux
  where
    invalidFee reason = TxError ("Invalid fee: " <> reason)

-- | Function is used to calculate intermediate fee amounts
-- when forming a transaction
createFakeTxFromRawTx :: TxRaw -> TxAux
createFakeTxFromRawTx TxRaw{..} =
    let fakeAddr = txOutAddress . toaOut . NE.head $ trOutputs
        fakeOutMB
            | trRemaining == mkCoin 0 = Nothing
            | otherwise = Just $ TxOutAux (TxOut fakeAddr trRemaining) []
        txOutsWithRem = maybe trOutputs (\remTx -> remTx :| toList trOutputs) fakeOutMB

        -- We create fake signers instead of safe signers,
        -- because safe signer requires passphrase
        -- but we don't want to reveal our passphrase to compute fee.
        -- Fee depends on size of tx in bytes, sign of a tx has the fixed size
        -- so we can use arbitrary signer.
        srcAddrs = NE.map (txOutAddress . fst) trInputs
        (_, fakeSK) = deterministicKeyGen "patakbardaqskovoroda228pva1488kk"
        hdwSigners = NE.zip (NE.repeat $ fakeSigner fakeSK) srcAddrs
    in makeMPubKeyTxAddrs hdwSigners trInputs txOutsWithRem

    -- buildDistribution :: TxRaw -> Text
    -- buildDistribution TxRaw{..} =
    --     let entries =
    --             trSpendings <&> \(CWAddressMeta {..}, c) ->
    --                 F.bprint (build % ": " %build) c cwamId
    --         remains = F.bprint ("Remaining: " %build) (fst trRemaining)
    --     in sformat
    --            ("Transaction input distribution:\n" %listF "\n" build %
    --             "\n" %build)
    --            (toList entries)
    --            remains
    -- listF separator formatter =
    --     F.later $ fold . intersperse separator . fmap (F.bprint formatter)

