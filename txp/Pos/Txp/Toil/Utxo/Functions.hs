-- | Functions operating on UTXO.

module Pos.Txp.Toil.Utxo.Functions
       ( VTxContext (..)
       , verifyTxUtxo
       , applyTxToUtxo
       , rollbackTxUtxo
       ) where

import           Universum

import           Control.Lens (_Left)
import           Control.Monad.Error.Class (MonadError (..))
import qualified Data.List.NonEmpty as NE
import           Formatting (int, sformat, (%))
import           Serokell.Util (VerificationRes, allDistinct, enumerate, formatFirstError,
                                verResToMonadError, verifyGeneric)

import           Pos.Binary.Core ()
import           Pos.Core (AddrType (..), Address (..), HasConfiguration, addressF, integerToCoin,
                           isRedeemAddress, isUnknownAddressType, sumCoins)
import           Pos.Core.Common (checkPubKeyAddress, checkRedeemAddress, checkScriptAddress)
import           Pos.Core.Txp (Tx (..), TxAttributes, TxAux (..), TxIn (..), TxInWitness (..),
                               TxOut (..), TxOutAux (..), TxSigData (..), TxUndo, TxWitness,
                               isTxInUnknown)
import           Pos.Crypto (SignTag (SignRedeemTx, SignTx), WithHash (..), checkSig, hash,
                             redeemCheckSig)
import           Pos.Data.Attributes (Attributes (attrRemain), areAttributesKnown)
import           Pos.Script (Script (..), isKnownScriptVersion, txScriptCheck)
import           Pos.Txp.Toil.Class (MonadUtxo (..), MonadUtxoRead (..), utxoDel, utxoPut)
import           Pos.Txp.Toil.Failure (ToilVerFailure (..), WitnessVerFailure (..))
import           Pos.Txp.Toil.Types (TxFee (..))

----------------------------------------------------------------------------
-- Verification
----------------------------------------------------------------------------

-- CSL-366 Add context-dependent variables to scripts
-- Postponed for now, should be done in near future.
-- Maybe these datatypes should be moved to Types.
-- | Global context data needed for tx verification. VT stands for
-- "Verify Tx". To be populated in further versions.
data VTxContext = VTxContext
    { -- | Verify that script versions in tx are known, addresses' and
      -- witnesses' types are known, attributes are known too.
      vtcVerifyAllIsKnown :: !Bool
--    , vtcSlotId   :: !SlotId         -- ^ Slot id of block transaction is checked in
--    , vtcLeaderId :: !StakeholderId  -- ^ Leader id of block transaction is checked in
    } deriving (Show)

-- | CHECK: Verify Tx correctness using 'MonadUtxoRead'.
-- Specifically there are the following checks:
--
-- * every input is a known unspent output;
-- * sum of inputs >= sum of outputs;
-- * every input has a proper witness verifying that input;
-- * script witnesses have matching script versions;
-- * if 'vtcVerifyAllIsKnown' is 'True', addresses' versions are verified
--   to be known, as well as witnesses, attributes, script versions.
--
-- Note that 'verifyTxUtxo' doesn't attempt to verify scripts with
-- versions higher than maximum script version we can handle. That's
-- because we want blocks with such transactions to be accepted (to
-- avoid hard forks). However, we won't include such transactions into
-- blocks when we're creating a block (because transactions for
-- inclusion into blocks are verified with 'vtcVerifyAllIsKnown'
-- set to 'True', so unknown script versions are rejected).
--
-- Returned fee can be 'Nothing' if there are inputs of unknown types.
verifyTxUtxo
    :: (MonadUtxoRead m, MonadError ToilVerFailure m)
    => VTxContext
    -> TxAux
    -> m (TxUndo, Maybe TxFee)
verifyTxUtxo ctx@VTxContext {..} ta@(TxAux UncheckedTx {..} witnesses) = do
    let unknownTxInMB = find (isTxInUnknown . snd) $ zip [0..] (toList _txInputs)
    case (vtcVerifyAllIsKnown, unknownTxInMB) of
        (True, Just (inpId, txIn)) -> throwError $
            ToilUnknownInput inpId txIn
        (False, Just _) -> do
            -- Case when at least one input isn't known
            minimalReasonableChecks
            resolvedInputs <- mapM (fmap rightToMaybe . runExceptT . resolveInput) _txInputs
            pure (map (fmap snd) resolvedInputs, Nothing)
        _               -> do
            -- Case when all inputs are known
            minimalReasonableChecks
            resolvedInputs <- mapM resolveInput _txInputs
            txFee <- verifySums resolvedInputs _txOutputs
            verifyKnownInputs ctx resolvedInputs ta
            when vtcVerifyAllIsKnown $ verifyAttributesAreKnown _txAttributes
            pure (map (Just . snd) resolvedInputs, Just txFee)
  where
    minimalReasonableChecks = do
        verifyConsistency _txInputs witnesses
        verResToMonadError (ToilInvalidOutputs . formatFirstError) $
            verifyOutputs ctx ta

resolveInput
    :: (MonadUtxoRead m, MonadError ToilVerFailure m)
    => TxIn -> m (TxIn, TxOutAux)
resolveInput txIn = (txIn, ) <$> (note (ToilNotUnspent txIn) =<< utxoGet txIn)

verifySums
    :: MonadError ToilVerFailure m
    => NonEmpty (TxIn, TxOutAux) -> NonEmpty TxOut -> m TxFee
verifySums resolvedInputs outputs =
  case mTxFee of
      Nothing -> throwError $
          ToilOutGreaterThanIn {tInputSum = inpSum, tOutputSum = outSum}
      Just txFee ->
          return txFee
  where
    -- It will be 'Nothing' if value exceeds 'maxBound @Coin' (can't
    -- happen because 'inpSum' doesn't exceed it and 'outSum' is not
    -- negative) or if 'outSum > inpSum' (which can happen and should
    -- be rejected).
    mTxFee = TxFee <$> rightToMaybe (integerToCoin (inpSum - outSum))
    outSum = sumCoins $ map txOutValue outputs
    inpSum = sumCoins $ map (txOutValue . toaOut . snd) resolvedInputs

verifyConsistency :: MonadError ToilVerFailure m => NonEmpty TxIn -> TxWitness -> m ()
verifyConsistency inputs witnesses
    | length inputs == length witnesses = pass
    | otherwise = throwError $ ToilInconsistentTxAux errMsg
  where
    errFmt = ("length of inputs != length of witnesses "%"("%int%" != "%int%")")
    errMsg = sformat errFmt (length inputs) (length witnesses)

verifyOutputs :: VTxContext -> TxAux -> VerificationRes
verifyOutputs VTxContext {..} (TxAux UncheckedTx {..} _) =
    verifyGeneric $
    concatMap verifyOutput (enumerate $ toList _txOutputs)
  where
    verifyOutput :: (Int, TxOut) -> [(Bool, Text)]
    verifyOutput (i, (TxOut {txOutAddress = addr@Address {..}, ..})) =
        [ ( not vtcVerifyAllIsKnown || areAttributesKnown addrAttributes
          , sformat
                ("output #"%int%" with address "%addressF%
                 " has unknown attributes")
                i addr
          )
        , ( not $ vtcVerifyAllIsKnown && isUnknownAddressType addr
          , sformat ("output #"%int%" sends money to an address with unknown "
                    %"type ("%addressF%"), this is prohibited") i addr
          )
        , ( not (isRedeemAddress addr)
          , sformat ("output #"%int%" sends money to a redeem address ("
                    %addressF%"), this is prohibited") i addr
          )
        ]

-- Verify inputs of a transaction after they have been resolved
-- (implies that they are known).
verifyKnownInputs ::
       (HasConfiguration, MonadError ToilVerFailure m)
    => VTxContext
    -> NonEmpty (TxIn, TxOutAux)
    -> TxAux
    -> m ()
verifyKnownInputs VTxContext {..} resolvedInputs TxAux {..} = do
    unless allInputsDifferent $ throwError ToilRepeatedInput
    mapM_ (uncurry3 checkInput) $
        zip3 [0 ..] (toList resolvedInputs) (toList witnesses)
  where
    uncurry3 f (a, b, c) = f a b c
    witnesses = taWitness
    txHash = hash taTx
    txSigData = TxSigData txHash

    allInputsDifferent :: Bool
    allInputsDifferent = allDistinct (toList (map fst resolvedInputs))

    checkInput
        :: (HasConfiguration, MonadError ToilVerFailure m)
        => Word32           -- ^ Input index
        -> (TxIn, TxOutAux) -- ^ Input and corresponding output data
        -> TxInWitness
        -> m ()
    checkInput i (txIn, toa@(TxOutAux txOut@TxOut{..})) witness = do
        unless (checkSpendingData txOutAddress witness) $
            throwError $ ToilWitnessDoesntMatch i txIn txOut witness
        whenLeft (checkWitness toa witness) $ \err ->
            throwError $ ToilInvalidWitness i witness err

    checkSpendingData addr wit = case wit of
        PkWitness{..}            -> checkPubKeyAddress twKey addr
        ScriptWitness{..}        -> checkScriptAddress twValidator addr
        RedeemWitness{..}        -> checkRedeemAddress twRedeemKey addr
        UnknownWitnessType witTag _ -> case addrType addr of
            ATUnknown addrTag -> addrTag == witTag
            _                 -> False

    -- the first argument here includes local context, can be used for scripts
    checkWitness :: HasConfiguration => TxOutAux -> TxInWitness -> Either WitnessVerFailure ()
    checkWitness _txOutAux witness = case witness of
        PkWitness{..} ->
            unless (checkSig SignTx twKey txSigData twSig) $
                throwError WitnessWrongSignature
        RedeemWitness{..} ->
            unless (redeemCheckSig SignRedeemTx twRedeemKey txSigData twRedeemSig) $
                throwError WitnessWrongSignature
        ScriptWitness{..} -> do
            let valVer = scrVersion twValidator
                redVer = scrVersion twRedeemer
            when (valVer /= redVer) $
                throwError $ WitnessScriptVerMismatch valVer redVer
            when (vtcVerifyAllIsKnown && not (isKnownScriptVersion valVer)) $
                throwError $ WitnessUnknownScriptVer valVer
            over _Left WitnessScriptError $
                txScriptCheck txSigData twValidator twRedeemer
        UnknownWitnessType t _ ->
            when vtcVerifyAllIsKnown $
                throwError $ WitnessUnknownType t

verifyAttributesAreKnown
    :: (MonadError ToilVerFailure m)
    => TxAttributes -> m ()
verifyAttributesAreKnown attrs =
    unless (areAttributesKnown attrs) $
    throwError $ ToilUnknownAttributes (attrRemain attrs)

-- | Remove unspent outputs used in given transaction, add new unspent
-- outputs.
applyTxToUtxo :: MonadUtxo m => WithHash Tx -> m ()
applyTxToUtxo (WithHash UncheckedTx {..} txid) = do
    mapM_ utxoDel $ filter (not . isTxInUnknown) (toList _txInputs)
    mapM_ applyOutput . zip [0 ..] . toList . map TxOutAux $ _txOutputs
  where
    applyOutput (idx, toa) = utxoPut (TxInUtxo  txid idx) toa

-- | Rollback application of given transaction to Utxo using Undo
-- data.  This function assumes that transaction has been really
-- applied and doesn't check anything.
rollbackTxUtxo
    :: (MonadUtxo m)
    => (TxAux, TxUndo) -> m ()
rollbackTxUtxo (txAux, undo) = do
    let tx@UncheckedTx {..} = taTx txAux
    let txid = hash tx
    mapM_ utxoDel $ take (length _txOutputs) $ map (TxInUtxo txid) [0..]
    mapM_ (uncurry utxoPut) $ mapMaybe knownInputAndUndo $ toList $ NE.zip _txInputs undo
  where
    knownInputAndUndo (_,         Nothing) = Nothing
    knownInputAndUndo (TxInUnknown _ _, _) = Nothing
    knownInputAndUndo (inp, Just u)        = Just (inp, u)
