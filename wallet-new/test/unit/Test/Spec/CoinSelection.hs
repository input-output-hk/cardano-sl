{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
module Test.Spec.CoinSelection (
    spec
  ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Gen, Property, arbitrary, choose, conjoin,
                     counterexample, forAll)

import           Formatting (bprint, sformat, (%))
import qualified Formatting as F
import           Formatting.Buildable (Buildable (..))
import qualified Text.Tabl as Tabl

import           Pos.Binary.Class (Bi (encode), toLazyByteString)
import qualified Pos.Chain.Txp as Core
import           Pos.Core (Coeff (..), TxSizeLinear (..))
import qualified Pos.Core as Core
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Crypto (SecretKey)
import           Serokell.Data.Memory.Units (Byte, fromBytes)
import           Serokell.Util.Text (listJsonIndent)

import           Util.Buildable

import           Cardano.Wallet.Kernel.CoinSelection (CoinSelFinalResult (..),
                     CoinSelHardErr (..), CoinSelPolicy,
                     CoinSelectionOptions (..), ExpenseRegulation (..),
                     InputGrouping (..), largestFirst, mkStdTx, newOptions,
                     random)
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (estimateCardanoFee, estimateHardMaxTxInputs,
                     estimateMaxTxInputs)
import           Cardano.Wallet.Kernel.Util.Core (paymentAmount, utxoBalance,
                     utxoRestrictToInputs)
import           Pos.Crypto.Signing.Safe (fakeSigner)
import           Test.Pos.Configuration (withDefConfiguration)
import           Test.Spec.CoinSelection.Generators (InitialBalance (..),
                     Pay (..), genFiddlyPayees, genFiddlyUtxo, genGroupedUtxo,
                     genPayee, genPayees, genRedeemPayee,
                     genUniqueChangeAddress, genUtxoWithAtLeast)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

{-------------------------------------------------------------------------------
  Fees
-------------------------------------------------------------------------------}

-- | A fee-estimation policy which doesn't calculate any fee.
freeLunch :: Int -> NonEmpty Core.Coin -> Core.Coin
freeLunch _ _ = Core.mkCoin 0

-- | The smallest fee possible.
minFee :: Int -> NonEmpty Core.Coin -> Core.Coin
minFee _ _ = Core.mkCoin 1

-- | An hopefully-accurate estimate of the Tx fees in Cardano.
cardanoFee :: Int -> NonEmpty Core.Coin -> Core.Coin
cardanoFee inputs outputs = Core.mkCoin $
    estimateCardanoFee linearFeePolicy inputs (toList $ fmap Core.getCoin outputs)
    where
      linearFeePolicy = TxSizeLinear (Coeff 155381) (Coeff 43.946)

-- | A simple linear fee proportional in the #inputs & #outputs.
linearFee :: Int -> NonEmpty Core.Coin -> Core.Coin
linearFee inputsLen outputs = Core.mkCoin (fromIntegral $ inputsLen + length outputs)


-- | For some reason the version of 'QuickCheck' we are using doesn't seem
-- to export 'withMaxSuccess'.
withMaxSuccess :: Int -> Spec -> Spec
withMaxSuccess x = modifyMaxSuccess (const x)


instance (Buildable a, Buildable b) => Buildable (Either a b) where
    build (Right r) = bprint ("Right " % F.build) r
    build (Left l)  = bprint ("Left "  % F.build) l

instance (Buildable a) => Buildable [a] where
    build = bprint (listJsonIndent 4)

{-------------------------------------------------------------------------------
  Rendering test failures in an helpful way to debug problems
-------------------------------------------------------------------------------}

type Cell = T.Text
type Row  = [Cell]

emptyCell :: Cell
emptyCell = mempty

-- | "Zips" two lists together, rendering them by using the provided rendering
-- functions. In case one is shorted than the other, the slack is padded with
-- empty cells.
renderZipped :: (a -> Row) -> (b -> Row) -> [a] -> [b] -> [Row]
renderZipped _ _ [] [] = []
renderZipped renderA renderB [] (x:xs) =
    (emptyCell : renderB x) : renderZipped renderA renderB [] xs
renderZipped renderA renderB (y:ys) [] =
    (renderA y  <> [emptyCell]) : renderZipped renderA renderB ys []
renderZipped renderA renderB (y:ys) (x:xs) =
    (renderA y <> renderB x) : renderZipped renderA renderB ys xs

-- | Merge two list of 'Row's together, filling the spaces with empty cells.
mergeRows :: [Row] -> [Row] -> [Row]
mergeRows [] []         = []
mergeRows (r:rs) []     = (r <> replicate (length r) emptyCell) : mergeRows rs []
mergeRows [] (r:rs)     = (replicate (length r) emptyCell <> r) : mergeRows [] rs
mergeRows (x:xs) (y:ys) = (x <> y) : mergeRows xs ys

renderUtxoAndPayees :: Core.Utxo -> NonEmpty Core.TxOut -> T.Text
renderUtxoAndPayees utxo outputs =
    Tabl.tabl Tabl.EnvAscii Tabl.DecorAll Tabl.DecorAll alignments cells
    where
      outputAddresses :: [Core.Address]
      outputAddresses = map Core.txOutAddress (toList outputs)

      alignments :: [Tabl.Alignment]
      alignments = map (const Tabl.AlignCentre) cells

      cells :: [Row]
      cells =
          let header = ["UTXO", "Payment Amount"]
          in header : renderZipped toUtxoRow
                                   (toPayeeRow outputAddresses)
                                   (sortedUtxo $ Map.toList utxo)
                                   (sortedPayees $ toList outputs) <> footer

      footer :: [Row]
      footer = ["Total", "Total"] : [[T.pack . show . Core.getCoin . utxoBalance $ utxo
                                    , T.pack . show . Core.getCoin . paymentAmount $ outputs
                                    ]]

renderTx :: NonEmpty Core.TxOut
         -- ^ The original payment outputs
         -> Core.Utxo
         -- ^ The original utxo
         -> Core.TxAux
         -- ^ The transaction generated by the coin selection
         -> T.Text
renderTx payees utxo tx =
    Tabl.tabl Tabl.EnvAscii Tabl.DecorAll Tabl.DecorAll alignments cells
    where
      payeeAddresses :: [Core.Address]
      payeeAddresses = map Core.txOutAddress (toList payees)

      txOutputs :: NonEmpty Core.TxOut
      txOutputs = Core._txOutputs . Core.taTx $ tx

      pickedInputs :: Core.Utxo
      pickedInputs = utxo `utxoRestrictToInputs` (Set.fromList $ toList . Core._txInputs . Core.taTx $ tx)

      alignments :: [Tabl.Alignment]
      alignments = map (const Tabl.AlignCentre) cells

      subTable1 :: [Row]
      subTable1 = renderZipped toUtxoRow
                               (toPayeeRow payeeAddresses)
                               (sortedUtxo $ Map.toList utxo)
                               (sortedPayees $ toList payees)

      subTable2 :: [Row]
      subTable2 = renderZipped toUtxoRow
                               (toPayeeRow payeeAddresses)
                               (sortedUtxo $ Map.toList pickedInputs)
                               (sortedPayees $ toList txOutputs)

      cells :: [Row]
      cells =
          let header = [ "Original UTXO"
                       , "Original Payees"
                       , "Generated-Tx Inputs"
                       , "Generated-Tx Outputs"
                       ]
          in header : (subTable1 `mergeRows` subTable2) <> footer

      footer :: [Row]
      footer = replicate 4 "Total" : [[T.pack . show . Core.getCoin . utxoBalance $ utxo
                                     , T.pack . show . Core.getCoin . paymentAmount $ payees
                                     , T.pack . show . Core.getCoin . utxoBalance $ pickedInputs
                                     , T.pack . show . Core.getCoin . paymentAmount $ txOutputs
                                     ]]

sortedUtxo :: [(Core.TxIn, Core.TxOutAux)]
           -> [(Core.TxIn, Core.TxOutAux)]
sortedUtxo = sortBy (\(_, t1) (_, t2) ->
                    (Core.getCoin . Core.txOutValue . Core.toaOut $ t2)
                    `compare`
                    (Core.getCoin . Core.txOutValue . Core.toaOut $ t1))

sortedPayees :: [Core.TxOut]  -> [Core.TxOut]
sortedPayees = sortBy (\p1 p2 ->
                      (Core.getCoin . Core.txOutValue $ p2)
                      `compare`
                      (Core.getCoin . Core.txOutValue $ p1))

toPayeeRow :: [Core.Address] -> Core.TxOut -> Row
toPayeeRow originalOutputs txOut =
    [ renderType <> renderAddress (Core.txOutAddress txOut)
     <> ":"
     <> (T.pack . show . Core.getCoin . Core.txOutValue $ txOut)
    ]
    where
        renderType :: T.Text
        renderType =
            case originalOutputs of
                [] -> mempty
                _ -> if isChangeAddress originalOutputs txOut
                        then "[CHANGE] "
                        else mempty


renderAddress :: Core.Address -> T.Text
renderAddress (sformat F.build -> a) =
    T.take 4 a <> ".." <> T.take 4 (T.reverse a)

toUtxoRow :: (Core.TxIn, Core.TxOutAux) -> Row
toUtxoRow (_, txOutAux) =
    [ renderAddress (Core.txOutAddress . Core.toaOut $ txOutAux)
     <> ":"
     <> (T.pack . show . Core.getCoin . Core.txOutValue . Core.toaOut $ txOutAux)
    ]

-- | Render two outputs together, in a diff style.
renderOutputs :: NonEmpty Core.TxOut -> NonEmpty Core.TxOut -> Text
renderOutputs original actual =
    Tabl.tabl Tabl.EnvAscii Tabl.DecorAll Tabl.DecorAll alignments cells
    where
      outputAddresses :: [Core.Address]
      outputAddresses = map Core.txOutAddress (toList actual)

      alignments :: [Tabl.Alignment]
      alignments = map (const Tabl.AlignCentre) cells

      cells :: [Row]
      cells =
          let header = ["Original Outputs", "Outputs minus the fee"]
          in header : renderZipped (toPayeeRow mempty)
                                   (toPayeeRow outputAddresses)
                                   (sortedPayees $ toList original)
                                   (sortedPayees $ toList actual)

{-------------------------------------------------------------------------------
  Handy combinators for property checking
-------------------------------------------------------------------------------}

-- Fail if the supplied predicate does not hold.
failIf :: Buildable a => String -> (a -> Bool) -> a -> Property
failIf label p a = counterexample msg (p a)
    where
        msg = label <> " triggered on " <> show (STB a)

paymentSucceeded :: (Buildable a, Buildable b)
                 => Core.Utxo -> NonEmpty Core.TxOut -> Either a b -> Property
paymentSucceeded utxo payees res =
    paymentSucceededWith utxo payees res []

paymentSucceededWith :: (Buildable a, Buildable b)
                     => Core.Utxo
                     -> NonEmpty Core.TxOut
                     -> Either a b
                     -> [Core.Utxo -> NonEmpty Core.TxOut -> b -> Property]
                     -> Property
paymentSucceededWith utxo payees res extraChecks =
    let msg = "Selection failed for Utxo with balance = " <> show (utxoBalance utxo) <>
              " and payment amount of " <> show (paymentAmount payees) <> "\n." <>
              "\n\n" <> T.unpack (renderUtxoAndPayees utxo payees) <>
              "\n\n"
    in case res of
            Left _  -> failIf msg isRight res
            Right b -> conjoin $ map (\f -> f utxo payees b) extraChecks

-- | Property that checks that @all@ the inputs in the Utxo ended up in the
-- final transaction.
utxoWasDepleted :: Core.Utxo
                -- ^ The original Utxo
                -> NonEmpty Core.TxOut
                -- ^ The original payment
                -> Core.TxAux
                -- ^ The generated transaction
                -> Property
utxoWasDepleted originalUtxo _originalOutputs tx =
    let originalInputs = map fst (Map.toList originalUtxo)
        txInputs   = toList (Core._txInputs . Core.taTx $ tx)
    in failIf ( "not all the original inputs ended up in the final tx.")
              (== originalInputs)
              txInputs

utxoWasNotDepleted :: Core.Utxo
                   -- ^ The original Utxo
                   -> NonEmpty Core.TxOut
                   -- ^ The original payment
                   -> Core.TxAux
                   -- ^ The generated transaction
                   -> Property
utxoWasNotDepleted originalUtxo _originalOutputs tx =
    let originalInputs = map fst (Map.toList originalUtxo)
        txInputs   = toList (Core._txInputs . Core.taTx $ tx)
    in failIf ( "all the original inputs ended up in the final tx.")
              (/= originalInputs)
              txInputs

feeWasPayed :: ExpenseRegulation
            -> Core.Utxo
            -> NonEmpty Core.TxOut
            -> Core.TxAux
            -> Property
feeWasPayed SenderPaysFee originalUtxo originalOutputs tx =
    let txOutputs = Core._txOutputs . Core.taTx $ tx
        in failIf ( "original utxo balance is greater or equal " <>
                    "the balance reconstructed with the change outputs, " <>
                    "which means the fee was not payed by the sender.\n\n" <>
                    T.unpack (renderTx originalOutputs originalUtxo tx) <>
                    "\n\n"
              )
              (< utxoBalance originalUtxo)
              (paymentAmount txOutputs)
feeWasPayed ReceiverPaysFee _ originalOutputs tx =
    let txOutputs = Core._txOutputs . Core.taTx $ tx
        amended   = removeChangeAddresses originalOutputs txOutputs
    in failIf ( "final outputs value less than the original:\n\n"
                <> T.unpack (renderOutputs originalOutputs amended)
                <> "\n\n"
              )
              (< paymentAmount originalOutputs)
              (paymentAmount amended)

removeChangeAddresses :: NonEmpty Core.TxOut
                      -> NonEmpty Core.TxOut
                      -> NonEmpty Core.TxOut
removeChangeAddresses originalNE actualNE =
    let original = map Core.txOutAddress (toList originalNE)
        actual   = toList actualNE
    in case filter (not . isChangeAddress original) actual of
         []     -> error $ "removeChangeAddresses: invariant violated, no outputs found.\n\n"
                         <> (renderOutputs originalNE actualNE) <> "\n\n"
         (x:xs) -> x :| xs

-- | Returns 'True' if the input 'Core.TxOut' contains a change address, i.e.
-- an address which was not contained in the initial outputs.
isChangeAddress :: [Core.Address] -> Core.TxOut -> Bool
isChangeAddress original x =
    not (Core.txOutAddress x `Data.List.elem` original)

{-------------------------------------------------------------------------------
  Dealing with errors
-------------------------------------------------------------------------------}

paymentFailedWith :: (Buildable a, Buildable b)
                  => Core.Utxo
                  -> NonEmpty Core.TxOut
                  -> Either a b
                  -> [Core.Utxo -> NonEmpty Core.TxOut -> a -> Property]
                  -> Property
paymentFailedWith utxo payees res extraChecks =
    let msg = "Selection succeeded (but we were expecting it to fail) " <>
              " for Utxo with balance = " <> show (utxoBalance utxo) <>
              " and payment amount of " <> show (paymentAmount payees) <> "\n." <>
              "\n\n" <> T.unpack (renderUtxoAndPayees utxo payees) <>
              "\n\n"
    in case res of
            Right _ -> failIf msg isLeft res
            Left a  -> conjoin $ map (\f -> f utxo payees a) extraChecks

notEnoughMoney :: CoinSelHardErr -> Bool
notEnoughMoney (CoinSelHardErrUtxoExhausted _ _) = True
notEnoughMoney CoinSelHardErrUtxoDepleted        = True
notEnoughMoney _                                 = False

outputWasRedeem :: CoinSelHardErr -> Bool
outputWasRedeem (CoinSelHardErrOutputIsRedeemAddress _) = True
outputWasRedeem _                                       = False

maxInputsReached :: CoinSelHardErr -> Bool
maxInputsReached (CoinSelHardErrMaxInputsReached _) = True
maxInputsReached _                                  = False

errorWas :: (CoinSelHardErr -> Bool)
         -> Core.Utxo
         -> NonEmpty Core.TxOut
         -> ShowThroughBuild CoinSelHardErr
         -> Property
errorWas predicate _ _ (STB hardErr) =
    failIf "This is not the error type we were expecting!" predicate hardErr

{-------------------------------------------------------------------------------
  Generator for transactions with as many inputs as possible.
-------------------------------------------------------------------------------}

-- | Generate a random maximum transaction size, then create a transaction with
--   as many inputs as possible. The @estimator@ parameter is used to compute the
--   number of inputs, as a function of the maximum transaction size and the sizes
--   of @Attributes AddrAttributes@ and @Attributes ()@.
genMaxInputTx :: (Byte -> Byte -> Byte -> Word64) -> Gen (Either Text (Byte, Byte))
genMaxInputTx estimator = do
    -- Generate the output and compute the attribute sizes.
    let genIn  = Core.TxInUtxo <$> arbitrary <*> pure maxBound

    output <- genOutAux
    let addrAttrSize = getAddrAttrSize output

    -- Select the maximum transaction size and compute the number of inputs.
    maxTxSize <- genMaxTxSize
    let maxInputs = fromIntegral (estimator addrAttrSize txAttrSize maxTxSize)

    -- Now build the transaction, attempting to make the encoded size of the transaction
    -- as large as possible.
    bimap pretty ((,maxTxSize) . encodedSize) <$> (
        withDefConfiguration $ \pm -> do
            key    <- arbitrary
            inputs <- replicateM maxInputs ((,) <$> genIn <*> genOutAux)
            mkTx pm key (NE.fromList inputs) (NE.fromList [output]) [])

genMaxTxSize :: Gen Byte
genMaxTxSize = fromBytes <$> choose (4000, 100000)

genOutAux :: Gen Core.TxOutAux
genOutAux = Core.TxOutAux <$> (Core.TxOut <$> arbitrary <*> arbitrary)

getAddrAttrSize :: Core.TxOutAux -> Byte
getAddrAttrSize = encodedSize . Core.addrAttributes . Core.txOutAddress . Core.toaOut

txAttrSize :: Byte
txAttrSize = encodedSize (mkAttributes ())

encodedSize :: Bi a => a -> Byte
encodedSize = fromBytes . fromIntegral . LBS.length . toLazyByteString . encode

{-------------------------------------------------------------------------------
  Combinators to assemble properties easily
-------------------------------------------------------------------------------}

type Policy = CoinSelectionOptions
           -> Word64
           -> CoinSelPolicy Core.Utxo Gen CoinSelFinalResult

type RunResult = ( Core.Utxo
                 , NonEmpty Core.TxOut
                 , Either (ShowThroughBuild CoinSelHardErr) Core.TxAux
                 )

maxNumInputs :: Word64
maxNumInputs = 300

genChange :: Core.Utxo
          -> NonEmpty Core.TxOut
          -> [Core.Coin]
          -> Gen [Core.TxOutAux]
genChange utxo payee css = forM css $ \change -> do
    changeAddr <- genUniqueChangeAddress utxo payee
    return Core.TxOutAux {
        Core.toaOut = Core.TxOut {
            Core.txOutAddress = changeAddr
          , Core.txOutValue   = change
          }
      }

mkTx :: Core.ProtocolMagic
     -> SecretKey
     -> NonEmpty (Core.TxIn, Core.TxOutAux)
     -- ^ Selected inputs
     -> NonEmpty Core.TxOutAux
     -- ^ Selected outputs
     -> [Core.TxOutAux]
     -- ^ A list of change addresess, in the form of 'TxOutAux'(s).
     -> Gen (Either CoinSelHardErr Core.TxAux)
mkTx pm key = mkStdTx pm (\_addr -> Right (fakeSigner key))


payRestrictInputsTo :: Word64
                    -> (InitialBalance -> Gen Core.Utxo)
                    -> (Core.Utxo -> Pay -> Gen (NonEmpty Core.TxOut))
                    -> (Int -> NonEmpty Core.Coin -> Core.Coin)
                    -> (CoinSelectionOptions -> CoinSelectionOptions)
                    -> InitialBalance
                    -> Pay
                    -> Policy
                    -> Gen RunResult
payRestrictInputsTo maxInputs genU genP feeFunction adjustOptions bal amount policy =
    withDefConfiguration $ \pm -> do
        utxo  <- genU bal
        payee <- genP utxo amount
        key   <- arbitrary
        let options = adjustOptions (newOptions feeFunction)
        res <- policy options
                      maxInputs
                      (fmap Core.TxOutAux payee)
                      utxo
        case res of
             Left e -> return (utxo, payee, Left (STB e))
             Right (CoinSelFinalResult inputs outputs coins) -> do
                    change <- genChange utxo payee coins
                    txAux  <- mkTx pm key inputs outputs change
                    return (utxo, payee, bimap STB identity txAux)

pay :: (InitialBalance -> Gen Core.Utxo)
    -> (Core.Utxo -> Pay -> Gen (NonEmpty Core.TxOut))
    -> (Int -> NonEmpty Core.Coin -> Core.Coin)
    -> (CoinSelectionOptions -> CoinSelectionOptions)
    -> InitialBalance
    -> Pay
    -> Policy
    -> Gen RunResult
pay = payRestrictInputsTo maxNumInputs

payOne :: (Int -> NonEmpty Core.Coin -> Core.Coin)
       -> (CoinSelectionOptions -> CoinSelectionOptions)
       -> InitialBalance
       -> Pay
       -> Policy
       -> Gen RunResult
payOne = pay genUtxoWithAtLeast genPayee

-- | Like 'payOne', but allows a custom 'Gen' for the payees to be supplied
payOne' :: (Core.Utxo -> Pay -> Gen (NonEmpty Core.TxOut))
        -> (Int -> NonEmpty Core.Coin -> Core.Coin)
        -> (CoinSelectionOptions -> CoinSelectionOptions)
        -> InitialBalance
        -> Pay
        -> Policy
        -> Gen RunResult
payOne' payeeGenerator = pay genUtxoWithAtLeast payeeGenerator

payBatch :: (Int -> NonEmpty Core.Coin -> Core.Coin)
         -> (CoinSelectionOptions -> CoinSelectionOptions)
         -> InitialBalance
         -> Pay
         -> Policy
         -> Gen RunResult
payBatch = pay genUtxoWithAtLeast genPayees

receiverPays :: CoinSelectionOptions -> CoinSelectionOptions
receiverPays o = o { csoExpenseRegulation = ReceiverPaysFee }

requireGrouping :: CoinSelectionOptions -> CoinSelectionOptions
requireGrouping o = o { csoInputGrouping = RequireGrouping }

preferGrouping :: CoinSelectionOptions -> CoinSelectionOptions
preferGrouping o = o { csoInputGrouping = PreferGrouping }

ignoreGrouping :: CoinSelectionOptions -> CoinSelectionOptions
ignoreGrouping o = o { csoInputGrouping = IgnoreGrouping }

spec :: Spec
spec =
    describe "Coin selection policies unit tests" $ do
        withMaxSuccess 1000 $ describe "largestFirst" $ do
            prop "one payee, SenderPaysFee, fee = 0" $ forAll (
                 payOne freeLunch identity (InitialLovelace 1000) (PayLovelace 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "one payee, ReceiverPaysFee, fee = 0" $ forAll (
                 payOne freeLunch receiverPays (InitialLovelace 1000) (PayLovelace 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, SenderPaysFee, fee = 0" $ forAll (
                 payBatch freeLunch identity (InitialLovelace 1000) (PayLovelace 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, ReceiverPaysFee, fee = 0" $ forAll (
                 payBatch freeLunch receiverPays (InitialLovelace 1000) (PayLovelace 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res

            -- Minimal fee
            prop "one payee, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payOne minFee identity (InitialLovelace 1000) (PayLovelace 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "one payee, ReceiverPaysFee, fee = 1 Lovelace" $ forAll (
                payOne minFee receiverPays (InitialLovelace 1000) (PayLovelace 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payBatch minFee identity (InitialLovelace 1000) (PayLovelace 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, ReceiverPaysFee, fee = 1 Lovelace" $ forAll (
                payBatch minFee receiverPays (InitialLovelace 1000) (PayLovelace 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res

        withMaxSuccess 2000 $ describe "random" $ do
            prop "one payee, SenderPaysFee, fee = 0" $ forAll (
                payOne freeLunch identity (InitialLovelace 1000) (PayLovelace 100) random
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "one payee, ReceiverPaysFee, fee = 0" $ forAll (
                payOne freeLunch receiverPays (InitialLovelace 1000) (PayLovelace 100) random
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, SenderPaysFee, fee = 0" $ forAll (
                payBatch freeLunch identity (InitialLovelace 1000) (PayLovelace 100) random
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, ReceiverPaysFee, fee = 0" $ forAll (
                payBatch freeLunch receiverPays (InitialLovelace 1000) (PayLovelace 100) random
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res

            -- minimal fee. It doesn't make sense to use it for 'ReceiverPaysFee', because
            -- rounding will essentially cause the computed @epsilon@ will be 0 for each
            -- output. For those cases, we use the 'linear' fee policy.
            prop "one payee, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payOne minFee identity (InitialLovelace 1000) (PayLovelace 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed SenderPaysFee]
            prop "multiple payees, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payBatch minFee identity (InitialLovelace 1000) (PayLovelace 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed SenderPaysFee]

            -- linear fee
            prop "one payee, ReceiverPaysFee, fee = linear" $ forAll (
                payOne linearFee receiverPays (InitialLovelace 1000) (PayLovelace 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed ReceiverPaysFee]
            prop "multiple payees, ReceiverPaysFee, fee = linear" $ forAll (
                payBatch linearFee receiverPays (InitialLovelace 1000) (PayLovelace 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed ReceiverPaysFee]

            -- cardano fee. In order for this policy to succeed, we need to
            -- have some non-negligible amount of coins in our initial Utxo, as
            -- `estimateCardanoFee` works on "real world estimates"  for things
            -- like attributes, and trying to setup syntetic experiments with
            -- less than 1ADA (10^6 lovelaces) is probably counter-productive
            prop "one payee, SenderPaysFee, fee = cardano" $ forAll (
                payOne cardanoFee identity (InitialADA 1000) (PayADA 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed SenderPaysFee]
            prop "multiple payees, SenderPaysFee, fee = cardano" $ forAll (
                payBatch cardanoFee identity (InitialADA 1000) (PayADA 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed SenderPaysFee]
            prop "one payee, ReceiverPaysFee, fee = cardano" $ forAll (
                payOne cardanoFee receiverPays (InitialADA 1000) (PayADA 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed ReceiverPaysFee]
            prop "multiple payees, ReceiverPaysFee, fee = cardano" $ forAll (
                payBatch cardanoFee receiverPays (InitialADA 1000) (PayADA 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed ReceiverPaysFee]

        withMaxSuccess 2000 $ describe "Expected failures" $ do
            prop "Paying a redeem address should always be rejected" $ forAll (
                payOne' genRedeemPayee linearFee receiverPays (InitialLovelace 1000) (PayLovelace 100) random
                ) $ \(utxo, payee, res) ->
                  paymentFailedWith utxo payee res [errorWas outputWasRedeem]
            prop "Paying somebody not having enough money should fail" $ forAll (
                payBatch linearFee receiverPays (InitialLovelace 10) (PayLovelace 100) random
                ) $ \(utxo, payee, res) -> do
                  paymentFailedWith utxo payee res [errorWas notEnoughMoney]
            prop "Restricting too much the number of inputs results in an hard error for a single payee" $ forAll (
                payRestrictInputsTo 1 genUtxoWithAtLeast genPayee freeLunch identity (InitialLovelace 200) (PayLovelace 100) random
                ) $ \(utxo, payee, res) -> do
                  paymentFailedWith utxo payee res [errorWas maxInputsReached]
            prop "Restricting too much the number of inputs results in an hard error for multiple payees" $ forAll (
                payRestrictInputsTo 1 genUtxoWithAtLeast genPayees freeLunch identity (InitialLovelace 200) (PayLovelace 100) random
                ) $ \(utxo, payee, res) -> do
                  paymentFailedWith utxo payee res [errorWas maxInputsReached]

        -- Tests that the coin selection is unaffected by the size of the
        -- Addresses in Cardano, as in the past there was a subtle corner case
        -- where coin selection would fail for Addresses of size < 104, which is
        -- the average in Cardano.
        withMaxSuccess 200 $ describe "Fiddly Addresses" $ do
            prop "multiple payees, SenderPaysFee, fee = cardano" $ forAll (
                pay genFiddlyUtxo genFiddlyPayees cardanoFee identity (InitialADA 1000) (PayADA 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed SenderPaysFee]
            prop "multiple payees, ReceiverPaysFee, fee = cardano" $ forAll (
                pay genFiddlyUtxo genFiddlyPayees cardanoFee receiverPays (InitialADA 1000) (PayADA 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed ReceiverPaysFee]

        -- Tests for the input grouping. By input grouping we mean the
        -- circumstance where one user of the wallet used the same 'Address'
        -- more than once (which is, technically, considered a bad practice).
        -- In case of a security breach (for example if quantum computers will
        -- become mainstream and able to brute force the current crypto in a
        -- matter of minutes) if a single address is \"cracked\" then all the
        -- associated inputs (and their funds) paying into this address would
        -- be at risk. This is why we allow an 'InputGrouping' option to be
        -- passed, which allows the coin selection to, if needed, pick all
        -- the associated inputs paying into the address we just picked.
        withMaxSuccess 2000 $ describe "Input Grouping" $ do
            prop "Require grouping, fee = 0, one big group depletes the Utxo completely" $ forAll (
                pay (genGroupedUtxo 1) genPayee freeLunch requireGrouping (InitialLovelace 1000) (PayLovelace 10) random
                ) $ \(utxo, payee, res) -> do
                  paymentSucceededWith utxo payee res [utxoWasDepleted]
            prop "Require grouping, fee = cardano, one big group depletes the Utxo completely" $ forAll (
                pay (genGroupedUtxo 1) genPayee freeLunch requireGrouping (InitialADA 1000) (PayADA 10) random
                ) $ \(utxo, payee, res) -> do
                  paymentSucceededWith utxo payee res [utxoWasDepleted]
            prop "Require grouping, fee = 0, several groups allows the payment to be fullfilled" $ forAll (
                pay (genGroupedUtxo 10) genPayee freeLunch requireGrouping (InitialLovelace 1000) (PayLovelace 10) random
                ) $ \(utxo, payee, res) -> do
                  paymentSucceeded utxo payee res
            prop "Prefer grouping, fee = 0" $ forAll (
                payOne freeLunch preferGrouping (InitialLovelace 1000) (PayLovelace 10) random
                ) $ \(utxo, payee, res) -> do
                  paymentSucceeded utxo payee res
            prop "IgnoreGrouping, fee = 0 must not deplete the utxo" $ forAll (
                pay (genGroupedUtxo 1) genPayee freeLunch ignoreGrouping (InitialLovelace 1000) (PayLovelace 10) random
                ) $ \(utxo, payee, res) -> do
                  paymentSucceededWith utxo payee res [utxoWasNotDepleted]

        describe "Estimating the maximum number of inputs" $ do
            prop "estimateMaxTxInputs yields a lower bound." $
                forAll (genMaxInputTx estimateMaxTxInputs) $ \case
                    Left _err        -> False
                    Right (lhs, rhs) -> lhs <= rhs

            prop "estimateMaxTxInputs yields a relatively tight bound." $
                forAll (genMaxInputTx $ \x y z -> 1 + estimateHardMaxTxInputs x y z) $ \case
                    Left _err        -> False
                    Right (lhs, rhs) -> lhs > rhs

            withMaxSuccess 1000 $ prop "estimateHardMaxTxInputs is close to estimateMaxTxInputs." $
                forAll ((,) <$> genMaxTxSize
                            <*> (getAddrAttrSize <$> genOutAux)) $
                \(maxTxSize, addrAttrSize) ->
                    let safeMax = estimateMaxTxInputs     addrAttrSize txAttrSize maxTxSize
                        hardMax = estimateHardMaxTxInputs addrAttrSize txAttrSize maxTxSize
                        threshold = 5 -- percent
                    in (hardMax * 100) `div` safeMax <= 100 + threshold
