-- | Specification of Pos.Txp.Toil.Utxo

module Test.Pos.Txp.Toil.UtxoSpec
       ( spec
       ) where

import           Universum

import           Control.Monad.Except (runExceptT)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text.Buildable as B
import qualified Data.Vector as V (fromList)
import           Fmt (blockListF', genericF, nameF, (+|), (|+))
import           Serokell.Util (allDistinct)
import           Test.Hspec (Expectation, Spec, describe, expectationFailure, it)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Property, arbitrary, counterexample, (==>))

import           Pos.Arbitrary.Txp (BadSigsTx (..), DoubleInputTx (..), GoodTx (..))
import           Pos.Core (HasConfiguration, addressHash, checkPubKeyAddress, makePubKeyAddressBoot,
                           makeScriptAddress, mkCoin, sumCoins)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxIn (..), TxInWitness (..), TxOut (..),
                               TxOutAux (..), TxSigData (..), TxWitness, isTxInUnknown)
import           Pos.Crypto (SignTag (SignTx), checkSig, fakeSigner, hash, toPublic, unsafeHash,
                             withHash)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Script (PlutusError (..), Script)
import           Pos.Script.Examples (alwaysSuccessValidator, badIntRedeemer, goodIntRedeemer,
                                      goodIntRedeemerWithBlah, goodStdlibRedeemer, idValidator,
                                      intValidator, intValidatorWithBlah, multisigRedeemer,
                                      multisigValidator, shaStressRedeemer, sigStressRedeemer,
                                      stdlibValidator)
import           Pos.Txp (MonadUtxoRead (utxoGet), ToilVerFailure (..), Utxo, VTxContext (..),
                          WitnessVerFailure (..), applyTxToUtxoPure, verifyTxUtxo, verifyTxUtxoPure)
import           Pos.Util (SmallGenerator (..), nonrepeating, runGen)
import           Pos.Util.QuickCheck.Property (qcIsLeft, qcIsRight)

import           Test.Pos.Configuration (withDefConfiguration)

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

spec :: Spec
spec = withDefConfiguration $ describe "Txp.Toil.Utxo" $ do
    describe "utxoGet @((->) Utxo)" $ do
        it "returns Nothing when given empty Utxo" $
            isNothing (utxoGet myTx (mempty @Utxo))
        prop description_findTxInUtxo findTxInUtxo
    describe "verifyTxUtxo" $ do
        prop description_verifyTxInUtxo verifyTxInUtxo
        prop description_validateGoodTx validateGoodTx
        prop description_badSigsTx badSigsTx
        prop description_doubleInputTx doubleInputTx
    describe "applyTxToUtxoPure" $ do
        prop description_applyTxToUtxoGood applyTxToUtxoGood
    scriptTxSpec
  where
    myTx = TxInUtxo myHash 0
    myHash = unsafeHash @Int32 0
    description_findTxInUtxo =
        "correctly finds the TxOut corresponding to (txHash, txIndex) when the key is in\
        \ the Utxo map, and doesn't find it otherwise"
    description_verifyTxInUtxo =
        "successfully verifies a transaction whose inputs are all present in the utxo\
        \ map"
    description_validateGoodTx =
        "validates a transaction whose inputs and well-formed transaction outputs"
    description_badSigsTx =
        "a transaction with inputs improperly signed is not validated"
    description_doubleInputTx =
        "a transaction that spends an input twice is not validated"
    description_applyTxToUtxoGood =
        "correctly removes spent outputs used as inputs in given transaction and\
        \ successfully adds this transaction's outputs to the utxo map"

----------------------------------------------------------------------------
-- Properties
----------------------------------------------------------------------------

findTxInUtxo :: HasConfiguration => TxIn -> TxOutAux -> Utxo -> Bool
findTxInUtxo key txO utxo =
    let utxo' = M.delete key utxo
        newUtxo = M.insert key txO utxo
    in (isJust $ utxoGet key newUtxo) && (isNothing $ utxoGet key utxo')

verifyTxInUtxo :: HasConfiguration => SmallGenerator GoodTx -> Property
verifyTxInUtxo (SmallGenerator (GoodTx ls)) =
    let txs = fmap (view _1) ls
        witness = V.fromList $ toList $ fmap (view _4) ls
        (ins, outs) = NE.unzip $ map (\(_, tIs, tOs, _) -> (tIs, tOs)) ls
        newTx = UncheckedTx ins (map toaOut outs) (mkAttributes ())
        utxo = M.fromList $ do
            tx@UncheckedTx{..} <- toList txs
            let id = hash tx
            (idx, out) <- zip [0..] (toList _txOutputs)
            pure ((TxInUtxo id idx), TxOutAux out)
        vtxContext = VTxContext False
        txAux = TxAux newTx witness
    in counterexample ("\n"+|nameF "txs" (blockListF' "-" genericF txs)|+""
                           +|nameF "transaction" (B.build txAux)|+"") $
       qcIsRight $ verifyTxUtxoPure vtxContext utxo txAux

badSigsTx :: HasConfiguration => SmallGenerator BadSigsTx -> Property
badSigsTx (SmallGenerator (getBadSigsTx -> ls)) =
    let (tx@UncheckedTx {..}, utxo, extendedInputs, txWits) =
            getTxFromGoodTx ls
        ctx = VTxContext False
        transactionVerRes =
            verifyTxUtxoPure ctx utxo $ TxAux tx txWits
        notAllSignaturesAreValid =
            any (signatureIsNotValid tx)
                (NE.zip (NE.fromList (toList txWits))
                        (map (fmap snd) extendedInputs))
    in notAllSignaturesAreValid ==> qcIsLeft transactionVerRes

doubleInputTx :: HasConfiguration => SmallGenerator DoubleInputTx -> Property
doubleInputTx (SmallGenerator (getDoubleInputTx -> ls)) =
    let ((tx@UncheckedTx {..}), utxo, _extendedInputs, txWits) =
            getTxFromGoodTx ls
        ctx = VTxContext False
        transactionVerRes =
            verifyTxUtxoPure ctx utxo $ TxAux tx txWits
        someInputsAreDuplicated =
            not $ allDistinct (toList _txInputs)
    in someInputsAreDuplicated ==> qcIsLeft transactionVerRes

validateGoodTx :: HasConfiguration => SmallGenerator GoodTx -> Property
validateGoodTx (SmallGenerator (getGoodTx -> ls)) =
    let quadruple@(tx, utxo, _, txWits) = getTxFromGoodTx ls
        ctx = VTxContext False
        transactionVerRes =
            verifyTxUtxoPure ctx utxo $ TxAux tx txWits
        transactionReallyIsGood = individualTxPropertyVerifier quadruple
    in transactionReallyIsGood ==> qcIsRight transactionVerRes

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

type TxVerifyingTools =
    (Tx, Utxo, NonEmpty (Maybe (TxIn, TxOutAux)), TxWitness)

-- | This function takes the list inside a 'GoodTx' and related types, and
-- turns it into something 'verifyTx' can use:
--
-- * the transaction that the list holds
-- * the input resolver associated with that transaction
-- * the list of resolved inputs with all inputs in the transaction
getTxFromGoodTx
    :: NonEmpty (Tx, TxIn, TxOutAux, TxInWitness)
    -> TxVerifyingTools
getTxFromGoodTx ls =
    let txWitness = V.fromList $ toList $ fmap (view _4) ls
        _txOutputs = map (toaOut . view _3) ls
        _txInputs = map (view _2) ls
        utxo :: Utxo
        utxo =
            M.fromList
                [ (i, (TxOutAux (NE.head o)))
                | ((UncheckedTx _ o _), i, _, _) <- toList ls
                ]
        extendInput txIn = (txIn, ) <$> M.lookup txIn utxo
        extendedInputs :: NonEmpty (Maybe (TxIn, TxOutAux))
        extendedInputs = map extendInput _txInputs
        _txAttributes = mkAttributes ()
    in ((UncheckedTx {..}), utxo, extendedInputs, txWitness)

-- | This function, used in 'verifyGoodTx', takes a 'GoodTx' and checks that
-- each property verified by 'verifyTx' holds, meaning:
--
-- * sum of inputs ≥ sum of outputs;
-- * every input is signed properly;
-- * every input is a known unspent output.
-- It also checks that it has good structure w.r.t. 'verifyTxAlone'.
individualTxPropertyVerifier :: HasConfiguration => TxVerifyingTools -> Bool
individualTxPropertyVerifier (tx@UncheckedTx{..}, _, extendedInputs, txWits) =
    let hasGoodSum = txChecksum extendedInputs _txOutputs
        hasGoodInputs =
            all (signatureIsValid tx)
                (NE.zip (NE.fromList (toList txWits))
                        (map (fmap snd) extendedInputs))
    in hasGoodSum && hasGoodInputs

signatureIsValid
    :: HasConfiguration
    => Tx
    -> (TxInWitness, Maybe TxOutAux) -- ^ input witness +
                                     --    output spent by the input
    -> Bool
signatureIsValid tx (PkWitness{..}, Just TxOutAux{..}) =
    let txSigData = TxSigData
            { txSigTxHash = hash tx }
    in checkPubKeyAddress twKey (txOutAddress toaOut) &&
       checkSig SignTx twKey txSigData twSig
signatureIsValid _ _ = False

signatureIsNotValid
    :: HasConfiguration
    => Tx
    -> (TxInWitness, Maybe TxOutAux)
    -> Bool
signatureIsNotValid = not ... signatureIsValid

-- | This function takes a list of resolved inputs from a transaction, that
-- same transaction's outputs, and verifies that the input sum is greater than
-- the output sum.
txChecksum :: NonEmpty (Maybe (TxIn, TxOutAux)) -> NonEmpty TxOut -> Bool
txChecksum extendedInputs txOuts =
    let inpSum = sumCoins . map (txOutValue . toaOut . snd) $
                 catMaybes $ toList extendedInputs
        outSum = sumCoins $ map txOutValue txOuts
    in inpSum >= outSum

applyTxToUtxoGood :: HasConfiguration
                  => (TxIn, TxOutAux)
                  -> M.Map TxIn TxOutAux
                  -> NonEmpty TxOutAux
                  -> Bool
applyTxToUtxoGood (txIn0, txOut0) txMap txOuts =
    let inpList = txIn0 :| M.keys txMap
        tx = UncheckedTx inpList (map toaOut txOuts) (mkAttributes ())
        -- Initial utxo
        initUtxo = M.fromList $ toList $ NE.zip inpList (txOut0 :| M.elems txMap)
        resultUtxo = applyTxToUtxoPure (withHash tx) initUtxo

        -- Inserted tx outputs
        newUtxosInputs =
            NE.fromList $
            (map (TxInUtxo (hash tx)) [0 ..]) `zip` toList txOuts
        -- Utxo without removed known inputs (we musn't remove unknown inputs)
        rmvUtxo = foldr M.delete initUtxo $ NE.filter (not . isTxInUnknown) inpList
        -- Expected Utxo after applying of the tx
        expectedUtxo = foldr (uncurry M.insert) rmvUtxo newUtxosInputs
    in expectedUtxo == resultUtxo

----------------------------------------------------------------------------
-- Script Txs spec
----------------------------------------------------------------------------

scriptTxSpec :: HasConfiguration => Spec
scriptTxSpec = describe "script transactions" $ do
    describe "good cases" $ do
        it "goodIntRedeemer + intValidator" $ do
            txShouldSucceed $ checkScriptTx
                intValidator
                (\_ -> ScriptWitness intValidator goodIntRedeemer)

        it "goodStdlibRedeemer + stdlibValidator" $ do
            txShouldSucceed $ checkScriptTx
                stdlibValidator
                (\_ -> ScriptWitness stdlibValidator goodStdlibRedeemer)

    describe "bad cases" $ do
        it "a P2PK tx spending a P2SH tx" $ do
            txShouldFailWithWitnessMismatch $ checkScriptTx
                alwaysSuccessValidator
                (\_ -> runGen $ -- random pk witness
                      PkWitness <$> arbitrary <*> arbitrary)

        it "validator script provided in witness doesn't match \
           \the validator for which the address was created" $ do
            let witness = ScriptWitness intValidator goodIntRedeemer
            txShouldFailWithWitnessMismatch $ checkScriptTx
                alwaysSuccessValidator
                (const witness)

        it "validator script isn't a proper validator, \
           \redeemer script isn't a proper redeemer" $ do
            let res = checkScriptTx
                    goodIntRedeemer
                    (\_ -> ScriptWitness goodIntRedeemer intValidator)
            res `txShouldFailWithPlutus` PlutusExecutionFailure
                "The validator script is missing `validator` and \
                \the redeemer script is missing `redeemer`"

        it "redeemer >>= validator doesn't typecheck" $ do
            let res = checkScriptTx
                    idValidator
                    (\_ -> ScriptWitness idValidator goodIntRedeemer)
            res `txShouldFailWithPlutus` PlutusExecutionFailure
                "The validation result isn't of type Comp \
                \(i.e. neither success nor failure)"

        it "redeemer and validator define same names" $ do
            let res = checkScriptTx
                    intValidatorWithBlah
                    (\_ -> ScriptWitness
                               intValidatorWithBlah
                               goodIntRedeemerWithBlah)
            res `txShouldFailWithPlutus` PlutusExecutionFailure
                "There are overlapping declared names in \
                \these scripts: User \"blah\""

        it "redeemer >>= validator outputs 'failure'" $ do
            let res = checkScriptTx
                    intValidator
                    (\_ -> ScriptWitness intValidator badIntRedeemer)
            res `txShouldFailWithPlutus` PlutusReturnedFalse

    let sks@[sk1, sk2, sk3,  sk4] = runGen $ nonrepeating 4
    let     [pk1, pk2, pk3, _pk4] = map toPublic sks

    describe "multisig" $ do
        describe "1-of-1" $ do
            let val = multisigValidator 1 [addressHash pk1]
            it "good (1 provided)" $ do
                txShouldSucceed $ checkScriptTx val
                    (\sd -> ScriptWitness val
                        (multisigRedeemer sd [Just $ fakeSigner sk1]))
            it "bad (0 provided)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd [Nothing]))
                res `txShouldFailWithPlutus` PlutusReturnedFalse
            it "bad (1 provided, wrong sig)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd [Just $ fakeSigner sk2]))
                res `txShouldFailWithPlutus` PlutusReturnedFalse
        describe "2-of-3" $ do
            let val = multisigValidator 2 (map addressHash [pk1, pk2, pk3])
            it "good (2 provided)" $ do
                txShouldSucceed $ checkScriptTx val
                    (\sd -> ScriptWitness val
                        (multisigRedeemer sd
                          [ Just $ fakeSigner sk1
                          , Nothing
                          , Just $ fakeSigner sk3]))
            it "good (3 provided)" $ do
                txShouldSucceed $ checkScriptTx val
                    (\sd -> ScriptWitness val
                        (multisigRedeemer sd
                          [ Just $ fakeSigner sk1
                          , Just $ fakeSigner sk2
                          , Just $ fakeSigner sk3]))
            it "good (3 provided, 1 wrong)" $ do
                txShouldSucceed $ checkScriptTx val
                    (\sd -> ScriptWitness val
                        (multisigRedeemer sd
                         [Just $ fakeSigner sk1,
                          Just $ fakeSigner sk4,
                          Just $ fakeSigner sk3]))
            it "bad (1 provided)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd
                             [Just $ fakeSigner sk1, Nothing, Nothing]))
                res `txShouldFailWithPlutus` PlutusReturnedFalse
            it "bad (2 provided, length doesn't match)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd
                             [Just $ fakeSigner sk1, Just $ fakeSigner sk2]))
                res `txShouldFailWithPlutus` PlutusReturnedFalse
            it "bad (3 provided, 2 wrong)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd
                             [Just $ fakeSigner sk1, Just $ fakeSigner sk3, Just $ fakeSigner sk2]))
                res `txShouldFailWithPlutus` PlutusReturnedFalse

    describe "execution limits" $ do
        it "5-of-5 multisig is okay" $ do
            let val = multisigValidator 5 (replicate 5 (addressHash pk1))
            txShouldSucceed $ checkScriptTx val
                (\sd -> ScriptWitness val
                    (multisigRedeemer sd
                     (replicate 5 (Just $ fakeSigner sk1))))
        it "10-of-10 multisig is bad" $ do
            let val = multisigValidator 10 (replicate 10 (addressHash pk1))
            let res = checkScriptTx val
                    (\sd -> ScriptWitness val
                        (multisigRedeemer sd
                         (replicate 10 (Just $ fakeSigner sk1))))
            res `txShouldFailWithPlutus` PlutusExecutionFailure
                "Out of petrol."
        it "500 rounds of SHA3 is okay" $ do
            txShouldSucceed $ checkScriptTx idValidator
                (\_ -> ScriptWitness idValidator (shaStressRedeemer 500))
        it "1000 rounds of SHA3 is bad" $ do
            let res = checkScriptTx idValidator
                      (\_ -> ScriptWitness idValidator (shaStressRedeemer 1000))
            res `txShouldFailWithPlutus` PlutusExecutionFailure
                "Out of petrol."
        it "100 rounds of sigverify is okay" $ do
            txShouldSucceed $ checkScriptTx idValidator
                (\_ -> ScriptWitness idValidator (sigStressRedeemer 100))
        it "200 rounds of sigverify is bad" $ do
            let res = checkScriptTx idValidator
                      (\_ -> ScriptWitness idValidator (sigStressRedeemer 200))
            res `txShouldFailWithPlutus` PlutusExecutionFailure
                "Out of petrol."

  where
    -- Some random stuff we're going to use when building transactions
    randomPkOutput = runGen $ do
        key <- arbitrary
        return (TxOut (makePubKeyAddressBoot key) (mkCoin 1))
    -- Make utxo with a single output; return utxo, the output, and an
    -- input that can be used to spend that output
    mkUtxo :: TxOut -> (TxIn, TxOut, Utxo)
    mkUtxo outp =
        let txid = unsafeHash ("nonexistent tx" :: Text)
        in  (TxInUtxo txid 0, outp, one ((TxInUtxo txid 0), (TxOutAux outp)))

    -- Do not verify versions
    vtxContext = VTxContext False

    -- Try to apply a transaction (with given utxo as context) and say
    -- whether it applied successfully
    tryApplyTx :: HasConfiguration => Utxo -> TxAux -> Either ToilVerFailure ()
    tryApplyTx utxo txa = runExceptT (() <$ verifyTxUtxo vtxContext txa) utxo

    -- Test tx1 against tx0. Tx0 will be a script transaction with given
    -- validator. Tx1 will be a P2PK transaction spending tx0 (with given
    -- input witness).
    checkScriptTx :: HasConfiguration
                  => Script
                  -> (TxSigData -> TxInWitness)
                  -> Either ToilVerFailure ()
    checkScriptTx val mkWit =
        let (inp, _, utxo) = mkUtxo $
                TxOut (makeScriptAddress Nothing val) (mkCoin 1)
            tx = UncheckedTx (one inp) (one randomPkOutput) $ mkAttributes ()
            txSigData = TxSigData { txSigTxHash = hash tx }
            txAux = TxAux tx (one (mkWit txSigData))
        in tryApplyTx utxo txAux

----------------------------------------------------------------------------
-- Script tx testing utilities
----------------------------------------------------------------------------

-- | Script transaction should pass the check and return 'Right'.
txShouldSucceed :: Either ToilVerFailure () -> Expectation
txShouldSucceed test = whenLeft test $ \x ->
    expectationFailure $ "unexpected failure: " <> show x

-- | Transaction should fail with a 'ToilWitnessDoesntMatch' error.
txShouldFailWithWitnessMismatch :: Either ToilVerFailure () -> Expectation
txShouldFailWithWitnessMismatch = \case
    Left ToilWitnessDoesntMatch{} -> pass
    other -> expectationFailure $
        "expected: Left ToilWitnessDoesntMatch{..}\n" <>
        " but got: " <> show other

-- | Transaction should fail with a Plutus error.
txShouldFailWithPlutus :: Either ToilVerFailure () -> PlutusError -> Expectation
txShouldFailWithPlutus res err = case res of
    Left ToilInvalidWitness{..}
        | tiwReason == WitnessScriptError err -> pass
        | otherwise -> expectationFailure $
              "expected: " <> show (WitnessScriptError err) <> "\n" <>
              " but got: " <> show tiwReason
    other -> expectationFailure $
        "expected: Left ...: " <> show (WitnessScriptError err) <> "\n" <>
        " but got: " <> show other
