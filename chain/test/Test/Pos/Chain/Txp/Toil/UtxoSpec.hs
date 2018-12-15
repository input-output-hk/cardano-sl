{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

-- | Specification of Pos.Chain.Txp.Utxo

module Test.Pos.Chain.Txp.Toil.UtxoSpec
       ( spec
       ) where

import           Universum hiding (id)

import           Control.Monad.Except (runExceptT)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Vector as V (fromList)
import           Fmt (blockListF', genericF, nameF, (+|), (|+))
import qualified Formatting.Buildable as B
import           Serokell.Util (allDistinct)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Property, counterexample, forAll, (==>))

import           Pos.Chain.Txp (ToilVerFailure (..), Tx (..), TxAux (..),
                     TxIn (..), TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxSigData (..), TxWitness, Utxo, VTxContext (..),
                     VerifyTxUtxoRes, applyTxToUtxo, evalUtxoM, execUtxoM,
                     isTxInUnknown, utxoGet, utxoToLookup, verifyTxUtxo)
import           Pos.Core (checkPubKeyAddress, sumCoins)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (ProtocolMagic, SignTag (SignTx), checkSig, hash,
                     unsafeHash, withHash)
import qualified Pos.Util.Modifier as MM

import           Test.Pos.Chain.Txp.Arbitrary (BadSigsTx (..),
                     DoubleInputTx (..), GoodTx (..), genGoodTxWithMagic)
import           Test.Pos.Util.QuickCheck.Arbitrary (SmallGenerator (..))
import           Test.Pos.Util.QuickCheck.Property (qcIsLeft, qcIsRight)

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

spec :: Spec
spec = describe "Txp.Toil.Utxo" $ do
    describe "utxoGet (no modifier)" $ do
        it "returns Nothing when given empty Utxo"
            $ isNothing (utxoGetSimple mempty myTxIn)
        prop description_findTxInUtxo findTxInUtxo
    describe "verifyTxUtxo" $ do
        prop description_verifyTxInUtxo verifyTxInUtxo
        prop description_validateGoodTx validateGoodTx
        prop description_badSigsTx      badSigsTx
        prop description_doubleInputTx  doubleInputTx
    describe "applyTxToUtxo" $ do
        prop description_applyTxToUtxoGood applyTxToUtxoGood

  where
    myTxIn = TxInUtxo myHash 0
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

findTxInUtxo :: TxIn -> TxOutAux -> Utxo -> Bool
findTxInUtxo key txO utxo =
    let utxo' = M.delete key utxo
        newUtxo = M.insert key txO utxo
     in (isJust $ utxoGetSimple newUtxo key) &&
        (isNothing $ utxoGetSimple utxo' key)

verifyTxInUtxo :: ProtocolMagic -> Property
verifyTxInUtxo pm = forAll (genGoodTxWithMagic pm) $ \(GoodTx ls) ->
    let txs = fmap (view _1) ls
        witness = V.fromList $ toList $ fmap (view _4) ls
        (ins, outs) = NE.unzip $ map (\(_, tIs, tOs, _) -> (tIs, tOs)) ls
        newTx = UnsafeTx ins (map toaOut outs) (mkAttributes ())
        utxo = M.fromList $ do
            tx@UnsafeTx{..} <- toList txs
            let id = hash tx
            (idx, out) <- zip [0..] (toList _txOutputs)
            pure ((TxInUtxo id idx), TxOutAux out)
        vtxContext = VTxContext False (makeNetworkMagic pm)
        txAux = TxAux newTx witness
    in counterexample ("\n"+|nameF "txs" (blockListF' "-" genericF txs)|+""
                           +|nameF "transaction" (B.build txAux)|+"") $
       qcIsRight $ verifyTxUtxoSimple pm vtxContext utxo txAux

badSigsTx :: ProtocolMagic -> SmallGenerator BadSigsTx -> Property
badSigsTx pm (SmallGenerator (getBadSigsTx -> ls)) =
    let (tx@UnsafeTx {..}, utxo, extendedInputs, txWits) =
            getTxFromGoodTx ls
        ctx = VTxContext False (makeNetworkMagic pm)
        transactionVerRes =
            verifyTxUtxoSimple pm ctx utxo $ TxAux tx txWits
        notAllSignaturesAreValid =
            any (signatureIsNotValid pm tx)
                (NE.zip (NE.fromList (toList txWits))
                        (map (fmap snd) extendedInputs))
    in notAllSignaturesAreValid ==> qcIsLeft transactionVerRes

doubleInputTx :: ProtocolMagic -> SmallGenerator DoubleInputTx -> Property
doubleInputTx pm (SmallGenerator (getDoubleInputTx -> ls)) =
    let ((tx@UnsafeTx {..}), utxo, _extendedInputs, txWits) =
            getTxFromGoodTx ls
        ctx = VTxContext False (makeNetworkMagic pm)
        transactionVerRes =
            verifyTxUtxoSimple pm ctx utxo $ TxAux tx txWits
        someInputsAreDuplicated =
            not $ allDistinct (toList _txInputs)
    in someInputsAreDuplicated ==> qcIsLeft transactionVerRes

validateGoodTx :: ProtocolMagic -> Property
validateGoodTx pm =
    forAll (genGoodTxWithMagic pm) $ \(GoodTx ls) ->
        let quadruple@(tx, utxo, _, txWits) = getTxFromGoodTx ls
            ctx = VTxContext False (makeNetworkMagic pm)
            transactionVerRes =
                verifyTxUtxoSimple pm ctx utxo $ TxAux tx txWits
            transactionReallyIsGood = individualTxPropertyVerifier pm quadruple
        in transactionReallyIsGood ==> qcIsRight transactionVerRes

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

utxoGetSimple :: Utxo -> TxIn -> Maybe TxOutAux
utxoGetSimple utxo txIn = evalUtxoM mempty (utxoToLookup utxo) (utxoGet txIn)

verifyTxUtxoSimple
    :: ProtocolMagic
    -> VTxContext
    -> Utxo
    -> TxAux
    -> Either ToilVerFailure VerifyTxUtxoRes
verifyTxUtxoSimple pm ctx utxo txAux =
    evalUtxoM mempty (utxoToLookup utxo) . runExceptT $
    verifyTxUtxo pm ctx mempty txAux

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
                | ((UnsafeTx _ o _), i, _, _) <- toList ls
                ]
        extendInput txIn = (txIn, ) <$> M.lookup txIn utxo
        extendedInputs :: NonEmpty (Maybe (TxIn, TxOutAux))
        extendedInputs = map extendInput _txInputs
        _txAttributes = mkAttributes ()
    in ((UnsafeTx {..}), utxo, extendedInputs, txWitness)

-- | This function, used in 'verifyGoodTx', takes a 'GoodTx' and checks that
-- each property verified by 'verifyTx' holds, meaning:
--
-- * sum of inputs ≥ sum of outputs;
-- * every input is signed properly;
-- * every input is a known unspent output.
-- It also checks that it has good structure w.r.t. 'verifyTxAlone'.
individualTxPropertyVerifier :: ProtocolMagic -> TxVerifyingTools -> Bool
individualTxPropertyVerifier pm (tx@UnsafeTx{..}, _, extendedInputs, txWits) =
    let hasGoodSum = txChecksum extendedInputs _txOutputs
        hasGoodInputs =
            all (signatureIsValid pm tx)
                (NE.zip (NE.fromList (toList txWits))
                        (map (fmap snd) extendedInputs))
    in hasGoodSum && hasGoodInputs

signatureIsValid
    :: ProtocolMagic
    -> Tx
    -> (TxInWitness, Maybe TxOutAux)
    -- ^ input witness + output spent by the input
    -> Bool
signatureIsValid pm tx (PkWitness twKey twSig, Just TxOutAux{..}) =
    let txSigData = TxSigData
            { txSigTxHash = hash tx }
    in checkPubKeyAddress twKey (txOutAddress toaOut) &&
       checkSig pm SignTx twKey txSigData twSig
signatureIsValid _ _ _ = False

signatureIsNotValid :: ProtocolMagic -> Tx -> (TxInWitness, Maybe TxOutAux) -> Bool
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

applyTxToUtxoGood :: (TxIn, TxOutAux)
                  -> M.Map TxIn TxOutAux
                  -> NonEmpty TxOutAux
                  -> Bool
applyTxToUtxoGood (txIn0, txOut0) txMap txOuts =
    let inpList = txIn0 :| M.keys txMap
        tx = UnsafeTx inpList (map toaOut txOuts) (mkAttributes ())
        -- Initial utxo
        initUtxo = M.fromList $ toList $ NE.zip inpList (txOut0 :| M.elems txMap)
        resultModifier =
            execUtxoM mempty (utxoToLookup initUtxo)
            (applyTxToUtxo (withHash tx))
        resultUtxo = MM.modifyMap resultModifier initUtxo

        -- Inserted tx outputs
        newUtxosInputs =
            NE.fromList $
            (map (TxInUtxo (hash tx)) [0 ..]) `zip` toList txOuts
        -- Utxo without removed known inputs (we musn't remove unknown inputs)
        rmvUtxo = foldr M.delete initUtxo $ NE.filter (not . isTxInUnknown) inpList
        -- Expected Utxo after applying of the tx
        expectedUtxo = foldr (uncurry M.insert) rmvUtxo newUtxosInputs
    in expectedUtxo == resultUtxo
