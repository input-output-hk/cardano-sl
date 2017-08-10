-- | Specification of Pos.Client.Txp.Util

module Test.Pos.Client.Txp.UtilSpec
       ( spec
       ) where

import           Universum
import           Unsafe                   (unsafeFromJust)

import qualified Data.ByteString          as BS
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Formatting               (build, hex, left, sformat, shown, (%), (%.))
import           Test.Hspec               (Spec, describe)
import           Test.Hspec.QuickCheck    (prop)

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Util      (TxError (..), TxOutputs, TxWithSpendings,
                                           createMTx, createRedemptionTx)
import           Pos.Core                 (HasCoreConstants, unsafeIntegerToCoin)
import           Pos.Crypto               (RedeemSecretKey, SafeSigner, SecretKey,
                                           decodeHash, fakeSigner)
import           Pos.Txp                  (TxId, TxIn (..), TxOut (..), TxOutAux (..),
                                           Utxo)
import           Pos.Types                (Address)
import           Test.Pos.Util            (giveTestsConsts, stopProperty)

import           Test.Pos.Client.Txp.Mode (TxpTestMode, TxpTestProperty)
import           Test.Pos.Client.Txp.Util (generateAddressAux, generateRedeemAddressAux,
                                           seedSize)

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

spec :: Spec
spec = giveTestsConsts $ describe "Client.Txp.Util" $ do
    describe "createMTx" $ createMTxSpec

createMTxSpec :: HasCoreConstants => Spec
createMTxSpec = do
    prop createMTxWorksWhenWeAreRichDesc createMTxWorksWhenWeAreRichSpec
    prop stabilizationDoesNotFailDesc stabilizationDoesNotFailSpec
    prop feeIsNonzeroDesc feeIsNonzeroSpec
    prop manyUtxoTo1Desc manyUtxoTo1Spec
    prop manyAddressesTo1Desc manyAddressesTo1Spec
    prop manyAddressesToManyDesc manyAddressesToManySpec
    prop redemptionDesc redemptionSpec
    prop txWithRedeemOutputFailsDesc txWithRedeemOutputFailsSpec
  where
    createMTxWorksWhenWeAreRichDesc =
        "Transaction is created successfully when we have 1 input with 1M coins " <>
        "and 1 output with 1 coin"
    stabilizationDoesNotFailDesc =
        "The message \"Couldn't stabilize tx fee after 5 attempts\" is not thrown " <>
        "when there is 1 input with 200k coins and 1 output with 1 coin"
    feeIsNonzeroDesc =
        "An attempt to create a tx for 1 coin when we have 100k coins fails " <>
        "because of the fee"
    manyUtxoTo1Desc =
        "Transaction is created successfully when we have 10 items in Utxo " <>
        "for a single address with 100k coins each"
    manyAddressesTo1Desc =
        "Transaction is created successfully when we have 10 items in Utxo " <>
        "for 10 different addresses with 100k coins each and 1 output with 1 coin"
    manyAddressesToManyDesc =
        "Transaction is created successfully when we have 10 items in Utxo " <>
        "for 10 different addresses with 100k coins each and 10 outputs with 1 coin each"
    redemptionDesc =
        "Redemption transaction is created successfully"
    txWithRedeemOutputFailsDesc =
        "An attempt to create a tx with a redeem address as an output fails"

createMTxWorksWhenWeAreRichSpec :: HasCoreConstants => TxpTestProperty ()
createMTxWorksWhenWeAreRichSpec = do
    txOrError <- createMTx cmpUtxo cmpSigners cmpOutputs cmpAddrData
    case txOrError of
        Left err -> stopProperty $ sformat ("Failed to create tx: "%build) err
        Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    CreateMTxParams {..} = makeManyAddressesToManyParams 1 1000000 1 1

stabilizationDoesNotFailSpec :: HasCoreConstants => TxpTestProperty ()
stabilizationDoesNotFailSpec = do
    txOrError <- createMTx cmpUtxo cmpSigners cmpOutputs cmpAddrData
    case txOrError of
        Left err@(FailedToStabilize _) -> stopProperty $ pretty err
        Left _                         -> return ()
        Right tx                       -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    CreateMTxParams {..} = makeManyAddressesToManyParams 1 200000 1 1

feeIsNonzeroSpec :: HasCoreConstants => TxpTestProperty ()
feeIsNonzeroSpec = do
    txOrError <- createMTx cmpUtxo cmpSigners cmpOutputs cmpAddrData
    case txOrError of
        Left (NotEnoughMoney _) -> return ()
        Left err -> stopProperty $ pretty err
        Right _ -> stopProperty $
            sformat ("Transaction was created even though there were "%
                "not enough funds for the fee")
  where
    CreateMTxParams {..} = makeManyAddressesToManyParams 1 100000 1 1

manyUtxoTo1Spec :: HasCoreConstants => TxpTestProperty ()
manyUtxoTo1Spec = do
    txOrError <- createMTx cmpUtxo cmpSigners cmpOutputs cmpAddrData
    case txOrError of
        Left err -> stopProperty $ pretty err
        Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    CreateMTxParams {..} = makeManyUtxoTo1Params 10 100000 1

manyAddressesTo1Spec :: HasCoreConstants => TxpTestProperty ()
manyAddressesTo1Spec = do
    txOrError <- createMTx cmpUtxo cmpSigners cmpOutputs cmpAddrData
    case txOrError of
        Left err -> stopProperty $ pretty err
        Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    CreateMTxParams {..} = makeManyAddressesToManyParams 10 100000 1 1

manyAddressesToManySpec :: HasCoreConstants => TxpTestProperty ()
manyAddressesToManySpec = do
    txOrError <- createMTx cmpUtxo cmpSigners cmpOutputs cmpAddrData
    case txOrError of
        Left err -> stopProperty $ pretty err
        Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    CreateMTxParams {..} = makeManyAddressesToManyParams 10 100000 10 1

redemptionSpec :: HasCoreConstants => TxpTestProperty ()
redemptionSpec = do
    txOrError <- createRedemptionTx utxo rsk outputs
    case txOrError of
        Left err -> stopProperty $ pretty err
        Right _  -> return ()
  where
    seedInput  = BS.replicate seedSize (0 :: Word8)
    seedOutput = BS.replicate seedSize (1 :: Word8)

    (rsk, txOutAuxInput)   = generateRedeemTxOutAux 1 seedInput
    (_, _, txOutAuxOutput) = generateTxOutAux 1 seedOutput

    utxo = one (TxInUtxo (unsafeIntegerToTxId 0) 0, txOutAuxInput)
    outputs = one txOutAuxOutput

txWithRedeemOutputFailsSpec :: HasCoreConstants => TxpTestProperty ()
txWithRedeemOutputFailsSpec = do
    txOrError <- createMTx utxo signers outputs addrData
    case txOrError of
        Left (OutputIsRedeem _) -> return ()
        Left err -> stopProperty $ pretty err
        Right _  -> stopProperty $
            sformat ("Transaction to a redeem address was created")
  where
    seedInput  = BS.replicate seedSize (0 :: Word8)
    seedOutput = BS.replicate seedSize (1 :: Word8)

    (sk, addr, txOutAuxInput) = generateTxOutAux 1000000 seedInput
    (_, txOutAuxOutput)       = generateRedeemTxOutAux 1 seedOutput

    utxo = one (TxInUtxo (unsafeIntegerToTxId 0) 0, txOutAuxInput)
    signers = one (fakeSigner sk, addr)
    outputs = one txOutAuxOutput
    addrData = ()

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

data CreateMTxParams = CreateMTxParams
    { cmpUtxo     :: !Utxo
    , cmpSigners  :: !(NonEmpty (SafeSigner, Address))
    , cmpOutputs  :: !TxOutputs
    , cmpAddrData :: !(AddrData TxpTestMode)
    }

makeManyUtxoTo1Params :: Integer -> Integer -> Integer -> CreateMTxParams
makeManyUtxoTo1Params numFrom amountEachFrom amountTo = CreateMTxParams {..}
  where
    seedInput  = BS.replicate seedSize (0 :: Word8)
    seedOutput = BS.replicate seedSize (1 :: Word8)

    (sk, addr, txOutAuxInput) = generateTxOutAux amountEachFrom seedInput
    (_, _, txOutAuxOutput)    = generateTxOutAux amountTo       seedOutput

    cmpUtxo = M.fromList
        [(TxInUtxo (unsafeIntegerToTxId 0) (fromIntegral k), txOutAuxInput) |
            k <- [0..numFrom-1]]
    cmpSigners = one (fakeSigner sk, addr)
    cmpOutputs = one txOutAuxOutput
    cmpAddrData = ()

makeManyAddressesToManyParams :: Word8 -> Integer -> Word8 -> Integer -> CreateMTxParams
makeManyAddressesToManyParams numFrom amountEachFrom numTo amountEachTo = CreateMTxParams {..}
  where
    seedsInput = [BS.replicate seedSize v | v <- [0..numFrom-1]]
    (cmpSignersList, txOutAuxInputs) = unzip
        [((fakeSigner sk, addr), txOutAux) |
            (sk, addr, txOutAux) <- map (generateTxOutAux amountEachFrom) seedsInput]

    cmpSigners = NE.fromList cmpSignersList

    seedsOutput = [BS.replicate seedSize v | v <- [numFrom..numFrom+numTo-1]]
    txOutAuxOutputs = [txOutAux |
        (_, _, txOutAux) <- map (generateTxOutAux amountEachTo) seedsOutput]

    cmpUtxo = M.fromList [(TxInUtxo (unsafeIntegerToTxId $ fromIntegral k) 0, txOutAux) |
        (k, txOutAux) <- zip [0..numFrom-1] txOutAuxInputs]
    cmpOutputs = NE.fromList txOutAuxOutputs
    cmpAddrData = ()

ensureTxMakesSense
  :: HasCoreConstants
  => TxWithSpendings -> Utxo -> TxOutputs -> TxpTestProperty ()
ensureTxMakesSense (_, neTxOut) utxo _ = do
    unless (S.fromList txOutUsed `S.isSubsetOf` S.fromList txOutAvailable) $
        stopProperty $
            sformat ("Used some inputs that were not available!\n"%
                    "Available: "%shown%"\n"%
                    "Used: "%shown
                    ) txOutAvailable txOutUsed
  where
    txOutAvailable = map toaOut $ M.elems utxo
    txOutUsed = NE.toList neTxOut

unsafeIntegerToTxId :: Integer -> TxId
unsafeIntegerToTxId n =
    unsafeFromJust $ rightToMaybe $ decodeHash $
        sformat (left 64 '0' %. hex) n

generateTxOutAux :: Integer -> ByteString -> (SecretKey, Address, TxOutAux)
generateTxOutAux amount seed =
    let (sk, addr) = generateAddressAux seed
        coin       = unsafeIntegerToCoin amount
        txOut      = TxOut addr coin
    in (sk, addr, TxOutAux txOut)

generateRedeemTxOutAux :: Integer -> ByteString -> (RedeemSecretKey, TxOutAux)
generateRedeemTxOutAux amount seed =
    let (sk, addr) = generateRedeemAddressAux seed
        coin       = unsafeIntegerToCoin amount
        txOut      = TxOut addr coin
    in (sk, TxOutAux txOut)
