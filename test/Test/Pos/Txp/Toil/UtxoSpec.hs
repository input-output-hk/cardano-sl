-- | Specification of Pos.Txp.Toil.Utxo

module Test.Pos.Txp.Toil.UtxoSpec
       ( spec
       ) where

import           Universum

import           Control.Monad.Except  (runExceptT)
import           Data.List             (zipWith3)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as NE
import qualified Data.Map              as M
import           Data.Maybe            (isJust, isNothing)
import qualified Data.Vector           as V (fromList, singleton, toList)
import           Formatting            (build, int, sformat, shown, (%))
import           Serokell.Util.Text    (listJsonIndent)
import           Test.Hspec            (Expectation, Spec, describe, expectationFailure,
                                        it, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (arbitrary)
import qualified Text.Regex.TDFA       as TDFA
import qualified Text.Regex.TDFA.Text  as TDFA

import           Pos.Crypto            (checkSig, fakeSigner, hash, toPublic, unsafeHash,
                                        withHash)
import           Pos.Data.Attributes   (mkAttributes)
import           Pos.Script            (Script)
import           Pos.Script.Examples   (alwaysSuccessValidator, badIntRedeemer,
                                        goodIntRedeemer, goodIntRedeemerWithBlah,
                                        goodStdlibRedeemer, idValidator, intValidator,
                                        intValidatorWithBlah, multisigRedeemer,
                                        multisigValidator, shaStressRedeemer,
                                        sigStressRedeemer, stdlibValidator)
import           Pos.Txp               (MonadUtxoRead (utxoGet), ToilVerFailure (..),
                                        Tx (..), TxAux, TxDistribution (..), TxIn (..),
                                        TxInWitness (..), TxOut (..), TxOutAux (..),
                                        TxSigData, TxWitness, Utxo, VTxContext (..),
                                        applyTxToUtxoPure, verifyTxUtxo, verifyTxUtxoPure)
import           Pos.Types             (BadSigsTx (..), GoodTx (..), SmallBadSigsTx (..),
                                        SmallGoodTx (..), checkPubKeyAddress,
                                        makePubKeyAddress, makeScriptAddress, mkCoin,
                                        sumCoins)
import           Pos.Util              (nonrepeating, runGen)

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

spec :: Spec
spec = describe "Txp.Toil.Utxo" $ do
    describe "utxoGet @((->) Utxo)" $ do
        it "returns Nothing when given empty Utxo" $
            utxoGet myTx (mempty @Utxo) == Nothing
        prop description_findTxInUtxo findTxInUtxo
    describe "verifyTxUtxo" $ do
        prop description_verifyTxInUtxo verifyTxInUtxo
        prop description_validateGoodTx validateGoodTx
        prop description_badSigsTx badSigsTx
    describe "applyTxToUtxoPure" $ do
        prop description_applyTxToUtxoGood applyTxToUtxoGood
    scriptTxSpec
  where
    myTx = TxIn myHash 0
    myHash = unsafeHash @Int64 0
    description_findTxInUtxo =
        "correctly finds the TxOut corresponding to (txHash, txIndex) when the key is in\
        \ the Utxo map, and doesn't find it otherwise"
    description_verifyTxInUtxo =
        "successfully verifies a transaction whose inputs are all present in the utxo\
        \ map"
    description_validateGoodTx =
        "validates a transaction whose inputs and well-formed transaction outputs"
    description_badSigsTx =
        "a transaction with inputs improperly signed is never validated"
    description_applyTxToUtxoGood =
        "correctly removes spent outputs used as inputs in given transaction and\
        \ successfully adds this transaction's outputs to the utxo map"

----------------------------------------------------------------------------
-- Properties
----------------------------------------------------------------------------

findTxInUtxo :: TxIn -> TxOutAux -> Utxo -> Bool
findTxInUtxo key txO utxo =
    let utxo' = M.delete key utxo
        newUtxo = M.insert key txO utxo
    in (isJust $ utxoGet key newUtxo) && (isNothing $ utxoGet key utxo')

verifyTxInUtxo :: SmallGoodTx -> Bool
verifyTxInUtxo (SmallGoodTx (GoodTx ls)) =
    let txs = fmap (view _1) ls
        witness = V.fromList $ toList $ fmap (view _4) ls
        (ins, outs) = NE.unzip $ map (\(_, tIs, tOs, _) -> (tIs, tOs)) ls
        newTx = UnsafeTx ins (map toaOut outs) (mkAttributes ())
        newDistr = TxDistribution (map toaDistr outs)
        utxo = foldr (\(tx, d) -> applyTxToUtxoPure (withHash tx) d) mempty txs
        vtxContext = VTxContext False
    in isRight $
       verifyTxUtxoPure vtxContext utxo (newTx, witness, newDistr)

badSigsTx :: SmallBadSigsTx -> Bool
badSigsTx (SmallBadSigsTx (getBadSigsTx -> ls)) =
    let ((tx@UnsafeTx {..}, dist), utxo, extendedInputs, txWits) =
            getTxFromGoodTx ls
        ctx = VTxContext False
        transactionIsNotVerified =
            isLeft $ verifyTxUtxoPure ctx utxo (tx, txWits, dist)
        notAllSignaturesAreValid =
            any
                (signatureIsNotValid
                     (NE.zipWith TxOutAux _txOutputs (getTxDistribution dist)))
                (NE.zip extendedInputs (NE.fromList $ V.toList txWits))
    in notAllSignaturesAreValid == transactionIsNotVerified

validateGoodTx :: SmallGoodTx -> Bool
validateGoodTx (SmallGoodTx (getGoodTx -> ls)) =
    let quadruple@((tx, dist), utxo, _, txWits) = getTxFromGoodTx ls
        ctx = VTxContext False
        transactionIsVerified =
            isRight $ verifyTxUtxoPure ctx utxo (tx, txWits, dist)
        transactionReallyIsGood = individualTxPropertyVerifier quadruple
    in transactionIsVerified == transactionReallyIsGood

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

type TxVerifyingTools =
    ((Tx, TxDistribution), Utxo,
     NonEmpty (Maybe (TxIn, TxOutAux)), TxWitness)

-- | This function takes the list inside a 'GoodTx' and related types, and
-- turns it into something 'verifyTx' can use:
--
-- * the transaction that the list holds
-- * the input resolver associated with that transaction
-- * the list of resolved inputs with all inputs in the transaction
getTxFromGoodTx
    :: NonEmpty ((Tx, TxDistribution), TxIn, TxOutAux, TxInWitness)
    -> TxVerifyingTools
getTxFromGoodTx ls =
    let txWitness = V.fromList $ toList $ fmap (view _4) ls
        unwrapTxOutAux TxOutAux {..} = (toaOut, toaDistr)
        (_txOutputs, TxDistribution -> txDist) =
            NE.unzip $ map (unwrapTxOutAux . view _3) ls
        _txInputs = map (view _2) ls
        utxo :: Utxo
        utxo =
            M.fromList
                [ (i, (TxOutAux (NE.head o) (NE.head d)))
                | ((UnsafeTx _ o _, TxDistribution d), i, _, _) <- toList ls
                ]
        extendInput txIn = (txIn, ) <$> M.lookup txIn utxo
        extendedInputs :: NonEmpty (Maybe (TxIn, TxOutAux))
        extendedInputs = map extendInput _txInputs
        _txAttributes = mkAttributes ()
    in ((UnsafeTx {..}, txDist), utxo, extendedInputs, txWitness)

-- | This function, used in 'verifyGoodTx', takes a 'GoodTx' and checks that
-- each property verified by 'verifyTx' holds, meaning:
--
-- * sum of inputs ≥ sum of outputs;
-- * every input is signed properly;
-- * every input is a known unspent output.
-- It also checks that it has good structure w.r.t. 'verifyTxAlone'.
individualTxPropertyVerifier :: TxVerifyingTools -> Bool
individualTxPropertyVerifier ((UnsafeTx {..}, dist), _, extendedInputs, txWits) =
    let hasGoodSum = txChecksum extendedInputs _txOutputs
        hasGoodInputs =
            all
                (signatureIsValid
                     (NE.zipWith TxOutAux _txOutputs (getTxDistribution dist)))
                (NE.zip extendedInputs (NE.fromList $ toList txWits))
    in hasGoodSum && hasGoodInputs

signatureIsValid :: NonEmpty TxOutAux -> (Maybe (TxIn, TxOutAux), TxInWitness) -> Bool
signatureIsValid outs (Just (TxIn {..}, (TxOutAux TxOut {..} _)), PkWitness {..}) =
    let unwrapedOuts = map (\TxOutAux {..} -> (toaOut, toaDistr)) outs
        (txOutputs, TxDistribution -> txDist) = NE.unzip unwrapedOuts
    in checkPubKeyAddress twKey txOutAddress &&
       checkSig twKey (txInHash, txInIndex, hash txOutputs, hash txDist) twSig
signatureIsValid _ _ = False

signatureIsNotValid :: NonEmpty TxOutAux -> (Maybe (TxIn, TxOutAux), TxInWitness) -> Bool
signatureIsNotValid txOutputs = not . signatureIsValid txOutputs

-- | This function takes a list of resolved inputs from a transaction, that
-- same transaction's outputs, and verifies that the input sum is greater than
-- the output sum.
txChecksum :: NonEmpty (Maybe (TxIn, TxOutAux)) -> NonEmpty TxOut -> Bool
txChecksum extendedInputs txOuts =
    let inpSum = sumCoins . map (txOutValue . toaOut . snd) $
                 catMaybes $ toList extendedInputs
        outSum = sumCoins $ map txOutValue txOuts
    in inpSum >= outSum

applyTxToUtxoGood :: (TxIn, TxOutAux)
                  -> M.Map TxIn TxOutAux
                  -> NonEmpty TxOutAux
                  -> Bool
applyTxToUtxoGood (txIn0, txOut0) txMap txOuts =
    let inpList = txIn0 :| M.keys txMap
        tx = UnsafeTx inpList (map toaOut txOuts) (mkAttributes ())
        txDistr = TxDistribution (map toaDistr txOuts)
        utxoMap = M.fromList $ toList $ NE.zip inpList (txOut0 :| M.elems txMap)
        newUtxoMap = applyTxToUtxoPure (withHash tx) txDistr utxoMap
        newUtxos =
            NE.fromList $
            (zipWith TxIn (repeat (hash tx)) [0 ..]) `zip` toList txOuts
        rmvUtxo = foldr M.delete utxoMap inpList
        insNewUtxo = foldr (uncurry M.insert) rmvUtxo newUtxos
    in insNewUtxo == newUtxoMap

----------------------------------------------------------------------------
-- Script Txs spec
----------------------------------------------------------------------------

scriptTxSpec :: Spec
scriptTxSpec = describe "script transactions" $ do
    describe "good cases" $ do
        it "goodIntRedeemer + intValidator" $ do
            let res = checkScriptTx
                    intValidator
                    (\_ -> ScriptWitness intValidator goodIntRedeemer)
            res `shouldSatisfy` isSuccess

        it "goodStdlibRedeemer + stdlibValidator" $ do
            let res = checkScriptTx
                    stdlibValidator
                    (\_ -> ScriptWitness stdlibValidator goodStdlibRedeemer)
            res `shouldSatisfy` isSuccess

    describe "bad cases" $ do
        it "a P2PK tx spending a P2SH tx" $ do
            let res = checkScriptTx
                    alwaysSuccessValidator
                    (\_ -> randomPkWitness)
            res `errorsShouldMatch` [
                -- There are two errors
                "input #0's witness doesn't match address.*\
                    \address details: ScriptAddress.*\
                    \witness: PkWitness.*",
                "input #0 isn't validated by its witness.*\
                    \signature check failed.*" ]

        it "validator script provided in witness doesn't match \
           \the validator for which the address was created" $ do
            let res = checkScriptTx
                    alwaysSuccessValidator
                    (\_ -> ScriptWitness intValidator goodIntRedeemer)
            res `errorsShouldMatch` [
                "input #0's witness doesn't match address.*\
                     \address details: ScriptAddress.*\
                     \witness: ScriptWitness.*" ]

        it "validator script isn't a proper validator, \
           \redeemer script isn't a proper redeemer" $ do
            let res = checkScriptTx
                    goodIntRedeemer
                    (\_ -> ScriptWitness goodIntRedeemer intValidator)
            res `errorsShouldMatch` [
                "input #0 isn't validated by its witness.*\
                    \reason: The validator script is missing `validator`.*\
                    \the redeemer script is missing `redeemer`"]

        it "redeemer >>= validator doesn't typecheck" $ do
            let res = checkScriptTx
                    idValidator
                    (\_ -> ScriptWitness idValidator goodIntRedeemer)
            res `errorsShouldMatch` [
                "input #0 isn't validated by its witness.*\
                    \reason: The validation result isn't of type Comp.*"]

        it "redeemer and validator define same names" $ do
            let res = checkScriptTx
                    intValidatorWithBlah
                    (\_ -> ScriptWitness
                               intValidatorWithBlah
                               goodIntRedeemerWithBlah)
            res `errorsShouldMatch` [
                "input #0 isn't validated by its witness.*\
                    \reason: There are overlapping declared names \
                    \in these scripts: User \"blah\"*"]

        it "redeemer >>= validator outputs 'failure'" $ do
            let res = checkScriptTx
                    intValidator
                    (\_ -> ScriptWitness intValidator badIntRedeemer)
            res `errorsShouldMatch` [
                "input #0 isn't validated by its witness.*\
                    \reason: result of evaluation is 'failure'.*"]

    let sks@[sk1, sk2, sk3,  sk4] = runGen $ nonrepeating 4
    let     [pk1, pk2, pk3, _pk4] = map toPublic sks
    let shouldBeFailure res = res `errorsShouldMatch` [
            "input #0 isn't validated by its witness.*\
                \reason: result of evaluation is 'failure'.*"]

    describe "multisig" $ do
        describe "1-of-1" $ do
            let val = multisigValidator 1 [pk1]
            it "good (1 provided)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd [Just $ fakeSigner sk1]))
                res `shouldSatisfy` isSuccess
            it "bad (0 provided)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd [Nothing]))
                shouldBeFailure res
            it "bad (1 provided, wrong sig)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd [Just $ fakeSigner sk2]))
                shouldBeFailure res
        describe "2-of-3" $ do
            let val = multisigValidator 2 [pk1, pk2, pk3]
            it "good (2 provided)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd
                             [Just $ fakeSigner sk1, Nothing, Just $ fakeSigner sk3]))
                res `shouldSatisfy` isSuccess
            it "good (3 provided)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd
                             [Just $ fakeSigner sk1, Just $ fakeSigner sk2, Just $ fakeSigner sk3]))
                res `shouldSatisfy` isSuccess
            it "good (3 provided, 1 wrong)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd
                             [Just $ fakeSigner sk1, Just $ fakeSigner sk4, Just $ fakeSigner sk3]))
                res `shouldSatisfy` isSuccess
            it "bad (1 provided)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd
                             [Just $ fakeSigner sk1, Nothing, Nothing]))
                shouldBeFailure res
            it "bad (2 provided, length doesn't match)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd
                             [Just $ fakeSigner sk1, Just $ fakeSigner sk2]))
                shouldBeFailure res
            it "bad (3 provided, 2 wrong)" $ do
                let res = checkScriptTx val
                        (\sd -> ScriptWitness val
                            (multisigRedeemer sd
                             [Just $ fakeSigner sk1, Just $ fakeSigner sk3, Just $ fakeSigner sk2]))
                shouldBeFailure res

    describe "execution limits" $ do
        it "10-of-10 multisig is okay" $ do
            let val = multisigValidator 10 (replicate 10 pk1)
            let res = checkScriptTx val
                    (\sd -> ScriptWitness val
                        (multisigRedeemer sd
                         (replicate 10 (Just $ fakeSigner sk1))))
            res `shouldSatisfy` isSuccess
        it "20-of-20 multisig is bad" $ do
            let val = multisigValidator 20 (replicate 20 pk1)
            let res = checkScriptTx val
                    (\sd -> ScriptWitness val
                        (multisigRedeemer sd
                         (replicate 20 (Just $ fakeSigner sk1))))
            res `errorsShouldMatch` [
                "input #0 isn't validated by its witness.*\
                        \reason: Out of petrol.*"]
        it "1000 rounds of SHA3 is okay" $ do
            let res = checkScriptTx idValidator
                      (\_ -> ScriptWitness idValidator (shaStressRedeemer 1000))
            res `shouldSatisfy` isSuccess
        it "3000 rounds of SHA3 is bad" $ do
            let res = checkScriptTx idValidator
                      (\_ -> ScriptWitness idValidator (shaStressRedeemer 3000))
            res `errorsShouldMatch` [
                "input #0 isn't validated by its witness.*\
                        \reason: Out of petrol.*"]
        it "200 rounds of sigverify is okay" $ do
            let res = checkScriptTx idValidator
                      (\_ -> ScriptWitness idValidator (sigStressRedeemer 200))
            res `shouldSatisfy` isSuccess
        it "500 rounds of sigverify is bad" $ do
            let res = checkScriptTx idValidator
                      (\_ -> ScriptWitness idValidator (sigStressRedeemer 500))
            res `errorsShouldMatch` [
                "input #0 isn't validated by its witness.*\
                        \reason: Out of petrol.*"]

  where
    isSuccess = isRight
    -- Some random stuff we're going to use when building transactions
    randomPkOutput = runGen $ do
        key <- arbitrary
        return (TxOut (makePubKeyAddress key) (mkCoin 1))
    randomPkWitness = runGen $
        PkWitness <$> arbitrary <*> arbitrary
    -- Make utxo with a single output; return utxo, the output, and an
    -- input that can be used to spend that output
    mkUtxo :: TxOut -> (TxIn, TxOut, Utxo)
    mkUtxo outp =
        let txid = unsafeHash ("nonexistent tx" :: Text)
        in  (TxIn txid 0, outp, one ((TxIn txid 0), (TxOutAux outp [])))

    -- Do not verify versions
    vtxContext = VTxContext False

    -- Try to apply a transaction (with given utxo as context) and say
    -- whether it applied successfully
    tryApplyTx :: Utxo -> TxAux -> Either ToilVerFailure ()
    tryApplyTx utxo txa = runExceptT (() <$ verifyTxUtxo vtxContext txa) utxo

    -- Test tx1 against tx0. Tx0 will be a script transaction with given
    -- validator. Tx1 will be a P2PK transaction spending tx0 (with given
    -- input witness).
    checkScriptTx :: Script -> (TxSigData -> TxInWitness) -> Either ToilVerFailure ()
    checkScriptTx val mkWit =
        let (inp, _, utxo) = mkUtxo $
                TxOut (makeScriptAddress val) (mkCoin 1)
            tx = UnsafeTx (one inp) (one randomPkOutput) $ mkAttributes ()
            txDistr = TxDistribution $ one mempty
            txSigData = (txInHash inp, 0, hash (one randomPkOutput), hash txDistr)
        in tryApplyTx utxo (tx, V.singleton (mkWit txSigData), txDistr)

-- | Test that errors in a 'VerFailure' match given regexes.
errorsShouldMatch :: Either ToilVerFailure a -> [Text] -> Expectation
errorsShouldMatch (Right _) _ =
    expectationFailure "expected to have errors, but there were none"
errorsShouldMatch (Left (ToilInvalidInputs xs)) ys = do
    let lx = length xs
        ly = length ys
    when (lx /= ly) $ expectationFailure $ toString $ sformat
        ("expected "%int%" errors: "%listJsonIndent 0%"\n"%
         "but there were "%int%" errors: "%listJsonIndent 0)
        ly ys lx xs
    sequence_ $ zipWith3 tryMatch [1 :: Int ..] xs ys
  where
    tryMatch i x y = do
        let mbRegexp = TDFA.compile
                         TDFA.defaultCompOpt{TDFA.multiline = False}
                         TDFA.defaultExecOpt
                         y
        regexp <- case mbRegexp of
            Right r -> return r
            Left e -> do expectationFailure $ toString $ sformat
                             ("couldn't compile regex for #"%int%": "%build)
                             i e
                         return (error "fail")
        unless (TDFA.matchTest regexp x) $
            expectationFailure $ toString $ sformat
                ("error #"%int%" doesn't match the regexp:\n"%
                 shown%"\n\n"%
                 build)
                i y x
errorsShouldMatch (Left e) _ =
    expectationFailure $ "unexpected error: " <> toString (pretty e)
