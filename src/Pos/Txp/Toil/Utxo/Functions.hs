-- | Functions operating on UTXO.

module Pos.Txp.Toil.Utxo.Functions
       ( VTxContext (..)
       , verifyTxUtxo
       , applyTxToUtxo
       , rollbackTxUtxo
       ) where

import           Control.Monad.Error.Class (MonadError (..))
import           Data.List                 (zipWith3)
import qualified Data.List.NonEmpty        as NE
import           Formatting                (build, int, sformat, (%))
import           Serokell.Util             (VerificationRes, allDistinct,
                                            formatFirstError, verResToMonadError,
                                            verifyGeneric)
import           Universum

import           Pos.Binary.Core           ()
import           Pos.Binary.Txp            ()
import           Pos.Core                  (Address (..), StakeholderId, coinF, mkCoin)
import           Pos.Core.Address          (addressDetailedF, checkPubKeyAddress,
                                            checkRedeemAddress, checkScriptAddress,
                                            checkUnknownAddressType)
import           Pos.Core.Coin             (coinToInteger, sumCoins)
import           Pos.Crypto                (WithHash (..), checkSig, hash, redeemCheckSig)
import           Pos.Script                (Script (..), isKnownScriptVersion,
                                            txScriptCheck)
import           Pos.Txp.Core              (Tx (..), TxAux, TxDistribution (..),
                                            TxIn (..), TxInWitness (..), TxOut (..),
                                            TxOutAux (..), TxUndo, TxWitness, txOutputs)
import           Pos.Txp.Toil.Class        (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Failure      (ToilVerFailure (..))

----------------------------------------------------------------------------
-- Verification
----------------------------------------------------------------------------

-- CSL-366 Add context-dependent variables to scripts
-- Postponed for now, should be done in near future.
-- Maybe these datatypes should be moved to Types.
-- | Global context data needed for tx verification. VT stands for
-- "Verify Tx". To be populated in further versions.
data VTxContext = VTxContext
    { -- | Verify that script & address versions in tx are known
      vtcVerifyVersions :: !Bool
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
-- * if 'vtcVerifyVersions' is 'True', addresses' versions are verified
--   to be known.
--
-- Note that 'verifyTxUtxo' doesn't attempt to verify scripts with versions
-- higher than maximum script version we can handle. That's because we want
-- blocks with such transactions to be accepted (to avoid hard
-- forks). However, we won't include such transactions into blocks when we're
-- creating a block.
verifyTxUtxo
    :: (MonadUtxoRead m, MonadError ToilVerFailure m)
    => VTxContext
    -> TxAux
    -> m TxUndo
verifyTxUtxo ctx ta@(UnsafeTx {..}, witnesses, _) = do
    verifyConsistency _txInputs witnesses
    verResToMonadError (ToilInvalidOutputs . formatFirstError) $
        verifyOutputs ctx ta
    resolvedInputs <- mapM resolveInput _txInputs
    verifySums resolvedInputs _txOutputs
    verResToMonadError ToilInvalidInputs $
        verifyInputs ctx resolvedInputs ta
    return $ map snd resolvedInputs

resolveInput
    :: (MonadUtxoRead m, MonadError ToilVerFailure m)
    => TxIn -> m (TxIn, TxOutAux)
resolveInput txIn = (txIn, ) <$> (note (ToilNotUnspent txIn) =<< utxoGet txIn)

verifySums
    :: MonadError ToilVerFailure m
    => NonEmpty (TxIn, TxOutAux) -> NonEmpty TxOut -> m ()
verifySums resolvedInputs outputs =
    when (outSum > inpSum) $
    throwError $ ToilOutGTIn {tInputSum = inpSum, tOutputSum = outSum}
  where
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
verifyOutputs VTxContext {..} (UnsafeTx {..}, _, distrs)=
    verifyGeneric $
        [ ( length _txOutputs == length (getTxDistribution distrs)
          , "length of outputs != length of tx distribution")
        ]
        ++
        do (i, (TxOut{..}, d)) <-
               zip [0 :: Int ..] $ toList (NE.zip _txOutputs (getTxDistribution distrs))
           case txOutAddress of
               PubKeyAddress{} ->
                   [ ( null d
                     , sformat ("output #"%int%" with pubkey address "%
                                "has non-empty distribution") i)
                   ]
               ScriptAddress{} -> checkDist i d txOutValue
               RedeemAddress{} ->
                   [ ( null d
                     , sformat ("output #"%int%" with redeem address "%
                                "has non-empty distribution") i)
                   ]
               UnknownAddressType t _
                   | vtcVerifyVersions ->
                         [ (False, sformat ("output #"%int%" has "%
                                            "unknown address type: "%int)
                                           i t) ]
                   | otherwise ->
                         checkDist i d txOutValue
  where
    checkDist i d txOutValue =
        let sumDist = sumCoins (map snd d)
        in [ (sumDist <= coinToInteger txOutValue,
              sformat ("output #"%int%" has distribution "%
                       "sum("%int%") > txOutValue("%coinF%")")
                      i sumDist txOutValue)
           , (allDistinct (map fst d :: [StakeholderId]),
              sformat ("output #"%int%"'s distribution "%
                       "has duplicated addresses")
                      i)
           , (all (> mkCoin 0) (map snd d),
              sformat ("output #"%int%"'s distribution "%
                       "assigns 0 coins to some addresses")
                      i)
           ]

verifyInputs :: VTxContext
             -> NonEmpty (TxIn, TxOutAux)
             -> TxAux
             -> VerificationRes
verifyInputs VTxContext {..} resolvedInputs (view txOutputs -> outs, witnesses, distrs) =
    verifyGeneric . concat $
    zipWith3 inputPredicates [0 ..] (toList resolvedInputs) (toList witnesses)
  where
    txOutHash = hash outs
    distrsHash = hash distrs
    inputPredicates
        :: Word32           -- ^ Input index
        -> (TxIn, TxOutAux) -- ^ Input and corresponding output data
        -> TxInWitness
        -> [(Bool, Text)]
    inputPredicates i (txIn@TxIn{..}, toa@(TxOutAux txOut@TxOut{..} _)) witness =
        [ ( checkAddrHash txOutAddress witness
          , sformat ("input #"%int%"'s witness doesn't match address "%
                     "of corresponding output:\n"%
                     "  input: "%build%"\n"%
                     "  output spent by this input: "%build%"\n"%
                     "  address details: "%addressDetailedF%"\n"%
                     "  witness: "%build)
                i txIn txOut txOutAddress witness
          )
        , case validateTxIn txIn toa witness of
              Right _ -> (True, error "can't happen")
              Left err -> (False, sformat
                  ("input #"%int%" isn't validated by its witness:\n"%
                   "  reason: "%build%"\n"%
                   "  input: "%build%"\n"%
                   "  output spent by this input: "%build%"\n"%
                   "  witness: "%build)
                  i err txIn txOut witness)
        ]

    checkAddrHash addr wit = case wit of
        PkWitness{..}          -> checkPubKeyAddress twKey addr
        ScriptWitness{..}      -> checkScriptAddress twValidator addr
        RedeemWitness{..}      -> checkRedeemAddress twRedeemKey addr
        UnknownWitnessType t _ -> checkUnknownAddressType t addr

    -- third argument here is local context, can be used for scripts
    validateTxIn :: TxIn -> TxOutAux -> TxInWitness -> Either String ()
    validateTxIn TxIn {..} _ PkWitness {..} =
        unless (checkSig twKey (txInHash, txInIndex, txOutHash, distrsHash) twSig) $
        Left "signature check failed"
    validateTxIn TxIn{..} _txOutAux ScriptWitness{..}
        | scrVersion twValidator /= scrVersion twRedeemer =
            Left "validator and redeemer have different versions"
        | not (isKnownScriptVersion (scrVersion twValidator)) =
            Right ()
        | otherwise =
                let txSigData = (txInHash, txInIndex, txOutHash, distrsHash)
                in txScriptCheck txSigData twValidator twRedeemer
    validateTxIn TxIn{..} _ RedeemWitness{..}
        | redeemCheckSig twRedeemKey (txInHash, txInIndex, txOutHash, distrsHash) twRedeemSig =
            Right ()
        | otherwise =
            Left "signature check failed"
    validateTxIn _ _ (UnknownWitnessType t _)
        | vtcVerifyVersions = Left ("unknown witness type: " <> show t)
        | otherwise         = Right ()

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
rollbackTxUtxo ((tx@UnsafeTx{..}, _, _), undo) = do
    let txid = hash tx
    mapM_ utxoDel $ take (length _txOutputs) $ zipWith TxIn (repeat txid) [0..]
    mapM_ (uncurry utxoPut) $ NE.zip _txInputs undo
