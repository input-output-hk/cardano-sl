
-- | Pure functions for operations with transactions

module Pos.Client.Txp.Util
       (
       -- * Tx creation
         TxCreateMode
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
       ) where

import           Universum

import           Control.Lens             (makeLenses, (%=), (.=))
import           Control.Monad.Except     (ExceptT, MonadError (throwError), runExceptT)
import           Control.Monad.State      (StateT (..), evalStateT)
import qualified Data.HashMap.Strict      as HM
import           Data.List                (tail)
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map                 as M
import qualified Data.Text.Buildable
import qualified Data.Vector              as V
import           Formatting               (bprint, build, sformat, stext, (%))

import           Pos.Binary               (biSize)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Core                 (TxFeePolicy (..), TxSizeLinear, bvdTxFeePolicy,
                                           calculateTxSizeLinear, integerToCoin,
                                           integerToCoin, isRedeemAddress, unsafeAddCoin,
                                           unsafeSubCoin)
import           Pos.Crypto               (RedeemSecretKey, SafeSigner,
                                           SignTag (SignRedeemTx, SignTx),
                                           deterministicKeyGen, fakeSigner, hash,
                                           redeemSign, redeemToPublic, safeSign,
                                           safeToPublic)
import           Pos.Data.Attributes      (mkAttributes)
import           Pos.DB                   (MonadGState, gsAdoptedBVData)
import           Pos.Script               (Script)
import           Pos.Script.Examples      (multisigRedeemer, multisigValidator)
import           Pos.Txp                  (Tx (..), TxAux (..), TxFee (..), TxIn (..),
                                           TxInWitness (..), TxOut (..), TxOutAux (..),
                                           TxSigData (..), Utxo)
import           Pos.Types                (Address, Coin, StakeholderId, mkCoin, sumCoins)

type TxInputs = NonEmpty TxIn
type TxOwnedInputs owner = NonEmpty (owner, TxIn)
type TxOutputs = NonEmpty TxOutAux
type TxWithSpendings = (TxAux, NonEmpty TxOut)

-- This datatype corresponds to raw transaction.
data TxRaw = TxRaw
    { trInputs         :: !(TxOwnedInputs TxOut)
    -- ^ Selected inputs from Utxo
    , trOutputs        :: !TxOutputs
    -- ^ Output addresses of tx (without remaing output)
    , trRemainingMoney :: !Coin
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

-- | Mode for creating transactions. We need to know fee policy.
type TxDistrMode m
     = ( MonadGState m
       )

type TxCreateMode m
    = ( TxDistrMode m
      , MonadAddresses m
      )

-- | Generic function to create a transaction, given desired inputs,
-- outputs and a way to construct witness from signature data
makeAbstractTx :: (owner -> TxSigData -> TxInWitness)
               -> TxOwnedInputs owner
               -> TxOutputs
               -> TxAux
makeAbstractTx mkWit txInputs outputs = TxAux tx txWitness
  where
    tx = UnsafeTx (map snd txInputs) txOutputs txAttributes
    txOutputs = map toaOut outputs
    txAttributes = mkAttributes ()
    txWitness = V.fromList $ toList $ txInputs <&>
        \(addr, _) -> mkWit addr txSigData
    txSigData = TxSigData
        { txSigTxHash = hash tx
        }

-- | Datatype which contains all data from DB which is necessary
-- to create transactions
data TxCreatorData = TxCreatorData
    { _tcdFeePolicy    :: !TxFeePolicy
    }

makeLenses ''TxCreatorData

-- | Transformer which holds data necessary for creating transactions
type TxCreator m = ReaderT TxCreatorData (ExceptT TxError m)

runTxCreator
    :: TxDistrMode m
    => TxCreator m a
    -> m (Either TxError a)
runTxCreator action = runExceptT $ do
    _tcdFeePolicy <- bvdTxFeePolicy <$> gsAdoptedBVData
    runReaderT action TxCreatorData{..}

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
              , twSig = safeSign SignTx ss sigData
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
        map swap hdwSigners
    getSigner (TxOut addr _) =
        fromMaybe (error "Requested signer for unknown address") $
        HM.lookup addr signers

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
            , twRedeemSig = redeemSign SignRedeemTx rsk sigData
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
    :: Monad m
    => Utxo
    -> TxOutputs
    -> TxFee
    -> TxCreator m TxRaw
prepareTxRaw utxo outputs (TxFee fee) = do
    mapM_ (checkIsNotRedeemAddr . txOutAddress . toaOut) outputs

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
                trRemainingMoney = totalTxAmount `unsafeSubCoin` totalMoneyWithFee
            let trOutputs = outputs
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
                    Just inp@(_, (TxOutAux (TxOut {..}))) -> do
                        ipsMoneyLeft .= unsafeSubCoin moneyLeft (min txOutValue moneyLeft)
                        ipsAvailableOutputs %= tail
                        pickInputs (inp : inps)

    formTxInputs (inp, TxOutAux txOut) = (txOut, inp)

    checkIsNotRedeemAddr outAddr =
        when (isRedeemAddress outAddr) $
            throwTxError "Destination address can't be redeem address"

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
    => Utxo
    -> TxOutputs
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOuts utxo outputs addrData = do
    txRaw@TxRaw {..} <- prepareTxWithFee utxo outputs
    outputsWithRem <- mkOutputsWithRem addrData txRaw
    pure (trInputs, outputsWithRem)

createGenericTx
    :: TxCreateMode m
    => (TxOwnedInputs TxOut -> TxOutputs -> TxAux)
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createGenericTx creator utxo outputs addrData = runTxCreator $ do
    (inps, outs) <- prepareInpsOuts utxo outputs addrData
    pure (creator inps outs, map fst inps)

createGenericTxSingle
    :: TxCreateMode m
    => (TxInputs -> TxOutputs -> TxAux)
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createGenericTxSingle creator = createGenericTx (creator . map snd)

-- | Make a multi-transaction using given secret key and info for outputs.
-- Currently used for HD wallets only, thus `HDAddressPayload` is required
createMTx
    :: TxCreateMode m
    => Utxo
    -> NonEmpty (SafeSigner, Address)
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createMTx utxo hdwSigners outputs addrData =
    createGenericTx (makeMPubKeyTxAddrs hdwSigners)
    utxo outputs addrData

-- | Make a multi-transaction using given secret key and info for
-- outputs.
createTx
    :: TxCreateMode m
    => Utxo
    -> SafeSigner
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createTx utxo ss outputs addrData =
    createGenericTxSingle (makePubKeyTx ss)
    utxo outputs addrData

-- | Make a transaction, using M-of-N script as a source
createMOfNTx
    :: TxCreateMode m
    => Utxo
    -> [(StakeholderId, Maybe SafeSigner)]
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createMOfNTx utxo keys outputs addrData =
    createGenericTxSingle (makeMOfNTx validator sks)
    utxo outputs addrData
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
createRedemptionTx utxo rsk outputs = runTxCreator $ do
    TxRaw {..} <- prepareTxRaw utxo outputs (TxFee $ mkCoin 0)
    let bareInputs = snd <$> trInputs
    pure $ makeRedemptionTx rsk bareInputs trOutputs

-----------------------------------------------------------------------------
-- Fees logic
-----------------------------------------------------------------------------

-- | Helper function to reduce code duplication
withLinearFeePolicy
    :: Monad m
    => (TxSizeLinear -> TxCreator m a)
    -> TxCreator m a
withLinearFeePolicy action = view tcdFeePolicy >>= \case
    TxFeePolicyUnknown w _ -> throwTxError $
        sformat ("Unknown fee policy, tag: "%build) w
    TxFeePolicyTxSizeLinear linearPolicy ->
        action linearPolicy

-- | Prepare transaction considering fees
prepareTxWithFee
    :: Monad m
    => Utxo
    -> TxOutputs
    -> TxCreator m TxRaw
prepareTxWithFee utxo outputs = withLinearFeePolicy $ \linearPolicy ->
    stabilizeTxFee linearPolicy utxo outputs

-- | Compute, how much fees we should pay to send money to given
-- outputs
computeTxFee
    :: Monad m
    => Utxo
    -> TxOutputs
    -> TxCreator m TxFee
computeTxFee utxo outputs = withLinearFeePolicy $ \linearPolicy -> do
    txAux <- createFakeTxFromRawTx <$>
             stabilizeTxFee linearPolicy utxo outputs
    txToLinearFee linearPolicy txAux

-- | Search such spendings that transaction's fee would be stable.
stabilizeTxFee
    :: forall m. Monad m
    => TxSizeLinear
    -> Utxo
    -> TxOutputs
    -> TxCreator m TxRaw
stabilizeTxFee linearPolicy utxo outputs =
    stabilizeTxFeeDo 5 (TxFee $ mkCoin 0)
  where
    stabilizeTxFeeDo :: Int -> TxFee -> TxCreator m TxRaw
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
        srcAddrs = NE.map (txOutAddress . fst) trInputs
        (_, fakeSK) = deterministicKeyGen "patakbardaqskovoroda228pva1488kk"
        hdwSigners = NE.zip (NE.repeat $ fakeSigner fakeSK) srcAddrs
    in makeMPubKeyTxAddrs hdwSigners trInputs txOutsWithRem
