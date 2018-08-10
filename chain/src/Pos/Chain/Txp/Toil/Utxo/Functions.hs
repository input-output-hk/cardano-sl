{-# LANGUAGE RecordWildCards #-}

-- | Functions operating on UTXO (in 'UtxoM' monad).

module Pos.Chain.Txp.Toil.Utxo.Functions
       ( VTxContext (..)
       , VerifyTxUtxoRes (..)
       , verifyTxUtxo
       , applyTxToUtxo
       , rollbackTxUtxo
       ) where

import           Universum

import           Control.Lens (_Left)
import           Control.Monad.Except (throwError)
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as Set
import           Formatting (int, sformat, (%))
import           Serokell.Util (allDistinct, enumerate)

import           Pos.Chain.Script (Script (..), isKnownScriptVersion,
                     txScriptCheck)
import           Pos.Chain.Txp.Toil.Failure (ToilVerFailure (..),
                     TxOutVerFailure (..), WitnessVerFailure (..))
import           Pos.Chain.Txp.Toil.Monad (UtxoM, utxoDel, utxoGet, utxoPut)
import           Pos.Chain.Txp.Toil.Types (TxFee (..))
import           Pos.Core (AddrType (..), Address (..), integerToCoin,
                     isRedeemAddress, isUnknownAddressType, sumCoins)
import           Pos.Core.Attributes (Attributes (attrRemain),
                     areAttributesKnown)
import           Pos.Core.Common (checkPubKeyAddress, checkRedeemAddress,
                     checkScriptAddress)
import           Pos.Core.Txp (Tx (..), TxAttributes, TxAux (..), TxIn (..),
                     TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxSigData (..), TxUndo, TxWitness, isTxInUnknown)
import           Pos.Crypto (SignTag (SignRedeemTx, SignTx), WithHash (..),
                     checkSig, hash, redeemCheckSig)
import           Pos.Crypto.Configuration (ProtocolMagic)
import           Pos.Util (liftEither)

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
    }

-- | Result of successful 'Tx' verification based on Utxo.
data VerifyTxUtxoRes = VerifyTxUtxoRes
    { vturUndo :: !TxUndo
    -- ^ 'TxUndo' for the verified transaction.
    , vturFee  :: !(Maybe TxFee)
    -- ^ Fee of the verified transaction. Can be 'Nothing' if there
    -- are inputs of unknown types.
    } deriving (Show)

-- | CHECK: Verify Tx correctness using 'UtxoLookup'.
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
verifyTxUtxo
    :: ProtocolMagic
    -> VTxContext
    -> Set Address
    -> TxAux
    -> ExceptT ToilVerFailure UtxoM VerifyTxUtxoRes
verifyTxUtxo protocolMagic ctx@VTxContext {..} lockedAssets ta@(TxAux UnsafeTx {..} witnesses) = do
    let unknownTxInMB = find (isTxInUnknown . snd) $ zip [0..] (toList _txInputs)
    case (vtcVerifyAllIsKnown, unknownTxInMB) of
        (True, Just (inpId, txIn)) -> throwError $
            ToilUnknownInput inpId txIn
        (False, Just _) -> do
            -- Case when at least one input isn't known
            minimalReasonableChecks
            resolvedInputs :: NonEmpty (Maybe (TxIn, TxOutAux)) <-
                mapM
                    (lift . fmap rightToMaybe . runExceptT . resolveInput)
                    _txInputs
            pure VerifyTxUtxoRes
                 { vturUndo = map (fmap snd) resolvedInputs
                 , vturFee = Nothing
                 }
        (_, Nothing) -> do
            -- Case when all inputs are known
            minimalReasonableChecks
            resolvedInputs <- filterAssetLocked =<< mapM resolveInput _txInputs
            liftEither $ do
                txFee <- verifySums resolvedInputs _txOutputs
                verifyKnownInputs protocolMagic ctx resolvedInputs ta
                when vtcVerifyAllIsKnown $ verifyAttributesAreKnown _txAttributes
                pure VerifyTxUtxoRes
                    { vturUndo = map (Just . snd) resolvedInputs
                    , vturFee = Just txFee
                    }
  where
    minimalReasonableChecks :: ExceptT ToilVerFailure UtxoM ()
    minimalReasonableChecks = liftEither $ do
        verifyConsistency _txInputs witnesses
        verifyOutputs ctx ta

    -- Until multisig addresses are available (which is supposed to be before decentralisation)
    -- all Ada are secured by single cryptographic signatures. In addition, before
    -- decentralisation, all blocks are being mined/minted by IOHK, Emurgo and the Cardano
    -- Foundation. These machines are run by the IOHK devops team and all run the same version
    -- of the software. Before decentralisation, the lockedAssets can provide extra protection for
    -- the Ada held by these entities. It is intended that this lockedAssets functionality will be
    -- removed after multisig addresses are available but before decentralisation.
    --
    -- After decentralisation, anyone can modify the code to add or remove similar functionality,
    -- but the decentralised nature of the network should make any such action irrelevant.
    filterAssetLocked
        :: NonEmpty (TxIn, TxOutAux)
        -> ExceptT ToilVerFailure UtxoM (NonEmpty (TxIn, TxOutAux))
    filterAssetLocked xs =
        case NE.filter notAssetLockedSrcAddr xs of
            []     -> throwError ToilEmptyAfterFilter
            (y:ys) -> pure (y :| ys)

    -- Return `True` iff none of the source addresses are in the lockedAssets set.
    notAssetLockedSrcAddr :: (txin, TxOutAux) -> Bool
    notAssetLockedSrcAddr (_, tao) =
        txOutAddress (toaOut tao) `Set.notMember` lockedAssets


-- | For a given TxIn, look up the TxOutAux that it is spending.
resolveInput :: TxIn -> ExceptT ToilVerFailure UtxoM (TxIn, TxOutAux)
resolveInput txIn =
    (txIn, ) <$> (note (ToilNotUnspent txIn) =<< lift (utxoGet txIn))

verifySums ::
       NonEmpty (TxIn, TxOutAux)
    -> NonEmpty TxOut
    -> Either ToilVerFailure TxFee
verifySums resolvedInputs outputs =
  case mTxFee of
      Nothing -> throwError $
          ToilOutGreaterThanIn inpSum outSum
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

verifyConsistency :: NonEmpty TxIn -> TxWitness -> Either ToilVerFailure ()
verifyConsistency inputs witnesses
    | length inputs == length witnesses = pass
    | otherwise = throwError $ ToilInconsistentTxAux errMsg
  where
    errFmt = ("length of inputs != length of witnesses "%"("%int%" != "%int%")")
    errMsg = sformat errFmt (length inputs) (length witnesses)

verifyOutputs :: VTxContext -> TxAux -> Either ToilVerFailure ()
verifyOutputs VTxContext {..} (TxAux UnsafeTx {..} _) =
    mapM_ verifyOutput . enumerate $ toList _txOutputs
  where
    verifyOutput :: (Word32, TxOut) -> Either ToilVerFailure ()
    verifyOutput (i, (TxOut {txOutAddress = addr@Address {..}, ..})) = do
        when (vtcVerifyAllIsKnown && not (areAttributesKnown addrAttributes)) $
            throwError $ ToilInvalidOutput i (TxOutUnknownAttributes addr)
        when (vtcVerifyAllIsKnown && isUnknownAddressType addr) $
            throwError $ ToilInvalidOutput i (TxOutUnknownAddressType addr)
        when (isRedeemAddress addr) $
            throwError $ ToilInvalidOutput i (TxOutRedeemAddressProhibited addr)

-- Verify inputs of a transaction after they have been resolved
-- (implies that they are known).
verifyKnownInputs ::
       ProtocolMagic
    -> VTxContext
    -> NonEmpty (TxIn, TxOutAux)
    -> TxAux
    -> Either ToilVerFailure ()
verifyKnownInputs protocolMagic VTxContext {..} resolvedInputs TxAux {..} = do
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
        :: Word32           -- ^ Input index
        -> (TxIn, TxOutAux) -- ^ Input and corresponding output data
        -> TxInWitness
        -> Either ToilVerFailure ()
    checkInput i (txIn, toa@(TxOutAux txOut@TxOut{..})) witness = do
        unless (checkSpendingData txOutAddress witness) $
            throwError $ ToilWitnessDoesntMatch i txIn txOut witness
        whenLeft (checkWitness toa witness) $ \err ->
            throwError $ ToilInvalidWitness i witness err

    checkSpendingData addr wit = case wit of
        PkWitness twKey _            -> checkPubKeyAddress twKey addr
        ScriptWitness twValidator _  -> checkScriptAddress twValidator addr
        RedeemWitness twRedeemKey _  -> checkRedeemAddress twRedeemKey addr
        UnknownWitnessType witTag _  -> case addrType addr of
            ATUnknown addrTag -> addrTag == witTag
            _                 -> False

    -- the first argument here includes local context, can be used for scripts
    checkWitness :: TxOutAux -> TxInWitness -> Either WitnessVerFailure ()
    checkWitness _txOutAux witness = case witness of
        PkWitness twKey twSig ->
            unless (checkSig protocolMagic SignTx twKey txSigData twSig) $
                throwError WitnessWrongSignature
        RedeemWitness twRedeemKey twRedeemSig ->
            unless (redeemCheckSig protocolMagic SignRedeemTx twRedeemKey txSigData twRedeemSig) $
                throwError WitnessWrongSignature
        ScriptWitness twValidator twRedeemer -> do
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
    :: TxAttributes -> Either ToilVerFailure ()
verifyAttributesAreKnown attrs =
    unless (areAttributesKnown attrs) $
    throwError $ ToilUnknownAttributes (attrRemain attrs)

-- | Remove unspent outputs used in given transaction, add new unspent
-- outputs.
applyTxToUtxo :: WithHash Tx -> UtxoM ()
applyTxToUtxo (WithHash UnsafeTx {..} txid) = do
    mapM_ utxoDel $ filter (not . isTxInUnknown) (toList _txInputs)
    mapM_ applyOutput . zip [0 ..] . toList . map TxOutAux $ _txOutputs
  where
    applyOutput (idx, toa) = utxoPut (TxInUtxo txid idx) toa

-- | Rollback application of given transaction to Utxo using Undo
-- data.  This function assumes that transaction has been really
-- applied and doesn't check anything.
rollbackTxUtxo :: (TxAux, TxUndo) -> UtxoM ()
rollbackTxUtxo (txAux, undo) = do
    let tx@UnsafeTx {..} = taTx txAux
    let txid = hash tx
    mapM_ utxoDel $ take (length _txOutputs) $ map (TxInUtxo txid) [0..]
    mapM_ (uncurry utxoPut) $ mapMaybe knownInputAndUndo $ toList $ NE.zip _txInputs undo
  where
    knownInputAndUndo (_,         Nothing) = Nothing
    knownInputAndUndo (TxInUnknown _ _, _) = Nothing
    knownInputAndUndo (inp, Just u)        = Just (inp, u)
