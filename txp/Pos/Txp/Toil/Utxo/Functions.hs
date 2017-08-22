-- | Functions operating on UTXO.

module Pos.Txp.Toil.Utxo.Functions
       ( VTxContext (..)
       , verifyTxUtxo
       , applyTxToUtxo
       , rollbackTxUtxo
       ) where

import           Universum

import           Control.Lens              (_Left)
import           Control.Monad.Error.Class (MonadError (..))
import qualified Data.List.NonEmpty        as NE
import           Formatting                (int, sformat, (%))
import           Serokell.Util             (VerificationRes, allDistinct, enumerate,
                                            formatFirstError, verResToMonadError,
                                            verifyGeneric)

import           Pos.Binary.Txp.Core       ()
import           Pos.Core                  (AddrType (..), Address (..), StakeholderId,
                                            addressF, coinF, coinToInteger, integerToCoin,
                                            isRedeemAddress, isUnknownAddressType, mkCoin,
                                            sumCoins)
import           Pos.Core.Address          (checkPubKeyAddress, checkRedeemAddress,
                                            checkScriptAddress)
import           Pos.Crypto                (SignTag (SignTx), WithHash (..), checkSig,
                                            hash, redeemCheckSig)
import           Pos.Data.Attributes       (Attributes (attrRemain), areAttributesKnown)
import           Pos.Script                (Script (..), isKnownScriptVersion,
                                            txScriptCheck)
import           Pos.Txp.Core              (Tx (..), TxAttributes, TxAux (..),
                                            TxDistribution (..), TxIn (..),
                                            TxInWitness (..), TxOut (..), TxOutAux (..),
                                            TxOutDistribution, TxSigData (..), TxUndo,
                                            TxWitness)
import           Pos.Txp.Toil.Class        (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Failure      (ToilVerFailure (..), WitnessVerFailure (..))
import           Pos.Txp.Toil.Types        (TxFee (..))

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
verifyTxUtxo
    :: (MonadUtxoRead m, MonadError ToilVerFailure m)
    => VTxContext
    -> TxAux
    -> m (TxUndo, TxFee)
verifyTxUtxo ctx@VTxContext {..} ta@(TxAux UnsafeTx {..} witnesses _) = do
    verifyConsistency _txInputs witnesses
    verResToMonadError (ToilInvalidOutputs . formatFirstError) $
        verifyOutputs ctx ta
    resolvedInputs <- mapM resolveInput _txInputs
    txFee <- verifySums resolvedInputs _txOutputs
    verifyInputs ctx resolvedInputs ta
    when vtcVerifyAllIsKnown $ verifyAttributesAreKnown _txAttributes
    return (map snd resolvedInputs, txFee)

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
          ToilOutGTIn {tInputSum = inpSum, tOutputSum = outSum}
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
verifyOutputs VTxContext {..} (TxAux UnsafeTx {..} _ distrs) =
    verifyGeneric $
    ( length _txOutputs == length (getTxDistribution distrs)
    , "length of outputs != length of tx distribution") :
    concatMap
        verifyOutput
        (enumerate $ toList (NE.zip _txOutputs (getTxDistribution distrs)))
  where
    verifyOutput :: (Int, (TxOut, TxOutDistribution)) -> [(Bool, Text)]
    verifyOutput (i, (TxOut {txOutAddress = addr@Address {..}, ..}, d)) =
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
        ] ++ checkDist i d txOutValue
    checkDist i d txOutValue =
        let sumDist = sumCoins (map snd d)
        in [ ( sumDist <= coinToInteger txOutValue
             , sformat
                   ("output #"%int%" has distribution "%"sum("%int%
                    ") > txOutValue("%coinF%")")
                   i sumDist txOutValue)
           , ( allDistinct (map fst d :: [StakeholderId])
             , sformat
                   ("output #"%int%"'s distribution "%
                    "has duplicated addresses") i)
           , ( all (> mkCoin 0) (map snd d)
             , sformat
                   ("output #"%int%"'s distribution "%
                    "assigns 0 coins to some addresses") i)
           ]

verifyInputs ::
       MonadError ToilVerFailure m
    => VTxContext
    -> NonEmpty (TxIn, TxOutAux)
    -> TxAux
    -> m ()
verifyInputs VTxContext {..} resolvedInputs TxAux {..} = do
    unless allInputsDifferent $ throwError ToilRepeatedInput
    mapM_ (uncurry3 checkInput) $
        zip3 [0 ..] (toList resolvedInputs) (toList witnesses)
  where
    uncurry3 f (a, b, c) = f a b c
    witnesses = taWitness
    distrs = taDistribution
    txHash = hash taTx
    distrHash = hash distrs
    txSigData = TxSigData txHash distrHash

    allInputsDifferent :: Bool
    allInputsDifferent = allDistinct (toList (map fst resolvedInputs))

    checkInput
        :: MonadError ToilVerFailure m
        => Word32           -- ^ Input index
        -> (TxIn, TxOutAux) -- ^ Input and corresponding output data
        -> TxInWitness
        -> m ()
    checkInput i (txIn@TxIn{..}, toa@(TxOutAux txOut@TxOut{..} _)) witness = do
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
    checkWitness :: TxOutAux -> TxInWitness -> Either WitnessVerFailure ()
    checkWitness _txOutAux witness = case witness of
        PkWitness{..} ->
            unless (checkSig SignTx twKey txSigData twSig) $
                throwError WitnessWrongSignature
        RedeemWitness{..} ->
            unless (redeemCheckSig twRedeemKey txSigData twRedeemSig) $
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
applyTxToUtxo :: MonadUtxo m => WithHash Tx -> TxDistribution -> m ()
applyTxToUtxo (WithHash UnsafeTx {..} txid) distr = do
    mapM_ utxoDel _txInputs
    mapM_ applyOutput . zip [0 ..] . toList . NE.zipWith TxOutAux _txOutputs $
        getTxDistribution distr
  where
    applyOutput (idx, toa) = utxoPut (TxIn txid idx) toa

-- | Rollback application of given transaction to Utxo using Undo
-- data.  This function assumes that transaction has been really
-- applied and doesn't check anything.
rollbackTxUtxo
    :: (MonadUtxo m)
    => (TxAux, TxUndo) -> m ()
rollbackTxUtxo (txAux, undo) = do
    let tx@UnsafeTx {..} = taTx txAux
    let txid = hash tx
    mapM_ utxoDel $ take (length _txOutputs) $ map (TxIn txid) [0..]
    mapM_ (uncurry utxoPut) $ NE.zip _txInputs undo
