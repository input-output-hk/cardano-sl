{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Spec.CoinSelection (
    spec
  ) where

import           Universum

import qualified Data.List
import           Data.List.NonEmpty (cons)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Formatting (sformat)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Gen, Property, arbitrary, choose, conjoin, counterexample, forAll,
                                  suchThat)

import           Data.Text.Buildable (Buildable (..))
import           Formatting (bprint, (%))
import qualified Formatting as F
import qualified Text.Tabl as Tabl

import           Pos.Core (HasConfiguration)
import qualified Pos.Core as Core
import           Pos.Crypto (SecretKey)
import qualified Pos.Txp as Core

import           Util.Buildable

import           Cardano.Wallet.Kernel.CoinSelection (CoinSelHardErr (..), CoinSelPolicy,
                                                      CoinSelectionOptions (..),
                                                      ExpenseRegulation (..), MkTx, largestFirst,
                                                      mkStdTx, newOptions, random)
import           Cardano.Wallet.Kernel.Util (paymentAmount, utxoBalance, utxoRestrictToInputs)
import           Pos.Crypto.Signing.Safe (fakeSigner)

{-------------------------------------------------------------------------------
  Generating stake for Utxo & payments
-------------------------------------------------------------------------------}

data StakeGenOptions = StakeGenOptions {
      stakeOnEachInputPercentage :: Maybe Double
      -- ^ How much of the total stake each input should hold. For example,
      -- passing 1.0 would mean that 100% of the stake will be allocated on a
      -- single input, which would make running the coin selection policy harder
      -- with multiple outputs. If not specified, the generator will pick a
      -- random value in (0.0,1.0] for each generated output.
    , stakeGenerationTarget      :: GenerationTarget
    -- ^ How close we want to hit our target.
    , stakeNeeded                :: Core.Coin
    -- ^ How much stake we want to generate
    , stakeMaxInputsNum          :: Maybe Int
    -- ^ If specified, stop generating inputs if they exceed the supplied
    -- number, to keep the Utxo size under control.
}

data GenerationTarget =
      AtLeast
    -- ^ Generate an 'Utxo' which has @at least@ Core.Coin stake.
    | Exactly
    -- ^ Generate an 'Utxo' which has @exactly@ Core.Coin stake.
    deriving Eq

-- | Generate an Utxo. The returned Utxo is not valid from a Cardano
-- perspective, but it is for coin selection.
genUtxo :: StakeGenOptions -> Gen Core.Utxo
genUtxo o = go mempty (stakeNeeded o) o
    where
        finalise :: Core.Utxo -> Core.Coin -> Gen Core.Utxo
        finalise utxo amountToCover =
            case amountToCover `Core.subCoin` (utxoBalance utxo) of
                 Nothing -> return utxo
                 Just (Core.Coin 0) -> return utxo
                 Just remaining -> do
                     txIn <- Core.TxInUtxo <$> arbitrary <*> arbitrary
                     addr <- arbitrary
                     let txOutAux = Core.TxOutAux (Core.TxOut addr remaining)
                     return $ Map.insert txIn txOutAux utxo

        go :: Core.Utxo -> Core.Coin -> StakeGenOptions -> Gen Core.Utxo
        go accUtxo amountToCover opts =
            case needToStop (Map.size accUtxo) (stakeMaxInputsNum opts) of
                 True  -> finalise accUtxo amountToCover
                 False -> do
                     stakePercentage <- case stakeOnEachInputPercentage opts of
                                            Nothing -> choose (0.1, 1.0)
                                            Just r  -> pure r
                     txIn <- (Core.TxInUtxo <$> arbitrary <*> arbitrary) `suchThat` (not . flip Map.member accUtxo)
                     let adjust c = ceiling $ ((fromIntegral (Core.coinToInteger c)) * stakePercentage)
                     coin <- Core.mkCoin <$> choose (1, adjust (stakeNeeded opts))
                     addr <- arbitrary
                     let txOutAux = Core.TxOutAux (Core.TxOut addr coin)
                     let utxo' = Map.insert txIn txOutAux accUtxo
                     case utxoBalance utxo' of
                         bal | bal >= stakeNeeded opts && stakeGenerationTarget opts == AtLeast -> return utxo'
                         bal | bal >= stakeNeeded opts && stakeGenerationTarget opts == Exactly ->
                                 -- Cover 'amountToCover' exactly, on the old Utxo.
                                 finalise accUtxo amountToCover
                             | otherwise ->
                                 case amountToCover `Core.subCoin` coin of
                                     Nothing        -> error "invariant violated!"
                                     Just remaining -> go utxo' remaining opts

-- | Generate some Utxo with @at least@ the supplied amount of money.
genUtxoWithAtLeast :: Word64 -> Gen Core.Utxo
genUtxoWithAtLeast amountToCover =
    genUtxo $ StakeGenOptions {
                  stakeOnEachInputPercentage = Just 0.2
                , stakeGenerationTarget = AtLeast
                , stakeNeeded           = Core.mkCoin amountToCover
                , stakeMaxInputsNum     = Just 100
            }

-- | A fee-estimation policy which doesn't calculate any fee.
freeLunch :: Int -> NonEmpty Core.Coin -> Core.Coin
freeLunch _ _ = Core.mkCoin 0

-- | The smallest fee possible.
minFee :: Int -> NonEmpty Core.Coin -> Core.Coin
minFee _ _ = Core.mkCoin 1

-- | A simple linear fee proportional in the #inputs & #outputs.
linearFee :: Int -> NonEmpty Core.Coin -> Core.Coin
linearFee inputsLen outputs = Core.mkCoin (fromIntegral $ inputsLen + length outputs)

-- | Generates a \"unique\" change address. A change address is unique is this
-- context if is not part of the Utxo & is not one of the original outputs
-- we need to pay.
genUniqueChangeAddress :: Core.Utxo
                       -> NonEmpty Core.TxOut
                       -> Gen Core.Address
genUniqueChangeAddress (map (Core.txOutAddress . Core.toaOut . snd) . Map.toList -> utxo)
                       (map Core.txOutAddress . toList -> outputs) =
    arbitrary `suchThat` (\a -> not (inUtxo utxo a) &&
                                not (a `Data.List.elem` outputs) &&
                                not (Core.isRedeemAddress a)
                         )
    where
        inUtxo :: [Core.Address] -> Core.Address -> Bool
        inUtxo addrs a = a `Data.List.elem` addrs

genTxOut :: StakeGenOptions
         -> Gen (NonEmpty Core.TxOut)
genTxOut opts =
    if stakeNeeded opts == Core.mkCoin 0
       then error "invaliant violation! You cannot pay 0 coins."
       else do o <- genOne
               go (o :| [])
    where
        genOne :: Gen Core.TxOut
        genOne = do
            stakePercentage <- case stakeOnEachInputPercentage opts of
                                   Nothing -> choose (0.1, 1.0)
                                   Just r  -> pure r
            let adjust c = ceiling $ fromIntegral (Core.coinToInteger c) * stakePercentage
            given <- Core.mkCoin <$> choose (1, adjust (stakeNeeded opts))
            addr  <- arbitrary `suchThat` (not . Core.isRedeemAddress)
            return $ Core.TxOut addr given

        finalise :: NonEmpty Core.TxOut -> Gen (NonEmpty Core.TxOut)
        finalise acc = do
            let slack = (stakeNeeded opts) `Core.unsafeSubCoin` (paymentAmount acc)
            addr  <- arbitrary `suchThat` (not . Core.isRedeemAddress)
            return $ (Core.TxOut addr slack) `cons` acc

        go :: NonEmpty Core.TxOut -> Gen (NonEmpty Core.TxOut)
        go acc = case needToStop (length acc) (stakeMaxInputsNum opts) of
            True  -> finalise acc
            False -> do
                o <- genOne
                let acc' = o `cons` acc
                case paymentAmount acc' of
                    bal | bal >= stakeNeeded opts && stakeGenerationTarget opts == AtLeast -> return acc'
                    bal | bal >= stakeNeeded opts && stakeGenerationTarget opts == Exactly ->
                            finalise acc
                        | otherwise -> go acc'

needToStop :: Int -> Maybe Int -> Bool
needToStop _ Nothing               = False
needToStop actual (Just requested) = actual >= requested

-- | For some reason the version of 'QuickCheck' we are using doesn't seem
-- to export 'withMaxSuccess'.
withMaxSuccess :: Int -> Spec -> Spec
withMaxSuccess x = modifyMaxSuccess (const x)

-- | Generates multiple payees.
genPayees :: Word64 -> Gen (NonEmpty Core.TxOut)
genPayees amountToCover =
    genTxOut StakeGenOptions {
               stakeOnEachInputPercentage = Just 0.15
             , stakeGenerationTarget = AtLeast
             , stakeNeeded           = Core.mkCoin amountToCover
             , stakeMaxInputsNum     = Just 5
             }

-- | Generates a single payee.
genPayee :: Word64 -> Gen (NonEmpty Core.TxOut)
genPayee amountToCover =
    genTxOut StakeGenOptions {
              stakeOnEachInputPercentage = Nothing
            , stakeGenerationTarget = AtLeast
            , stakeNeeded           = Core.mkCoin amountToCover
            , stakeMaxInputsNum     = Just 1
            }

-- | Generates a single payee which has a redeem address inside.
genRedeemPayee :: Word64 -> Gen (NonEmpty Core.TxOut)
genRedeemPayee c = do
    a <- arbitrary `suchThat` Core.isRedeemAddress
    return (Core.TxOut a (Core.mkCoin c) :| [])

instance (Buildable a, Buildable b) => Buildable (Either a b) where
    build (Right r) = bprint ("Right " % F.build) r
    build (Left l)  = bprint ("Left "  % F.build) l

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
      pickedInputs = utxoRestrictToInputs (Set.fromList $ toList . Core._txInputs . Core.taTx $ tx) utxo

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

errorWas :: (CoinSelHardErr -> Bool)
         -> Core.Utxo
         -> NonEmpty Core.TxOut
         -> ShowThroughBuild CoinSelHardErr
         -> Property
errorWas predicate _ _ (STB hardErr) =
    failIf "This is not the error type we were expecting!" predicate hardErr

{-------------------------------------------------------------------------------
  Combinators to assemble properties easily
-------------------------------------------------------------------------------}

type Policy = CoinSelectionOptions
           -> Gen Core.Address
           -> MkTx Gen
           -> Word64
           -> CoinSelPolicy Core.Utxo Gen Core.TxAux

type RunResult = ( Core.Utxo
                 , NonEmpty Core.TxOut
                 , Either (ShowThroughBuild CoinSelHardErr) Core.TxAux
                 )

newtype InitialBalance = InitialBalance Word64
newtype Pay = Pay Word64

maxNumInputs :: Word64
maxNumInputs = 300

mkTx :: Core.HasProtocolMagic => SecretKey -> MkTx Gen
mkTx key = mkStdTx (\_addr -> Right (fakeSigner key))

pay :: Core.HasProtocolMagic
    => (Word64 -> Gen Core.Utxo)
    -> (Word64 -> Gen (NonEmpty Core.TxOut))
    -> (Int -> NonEmpty Core.Coin -> Core.Coin)
    -> (CoinSelectionOptions -> CoinSelectionOptions)
    -> InitialBalance
    -> Pay
    -> Policy
    -> Gen RunResult
pay genU genP feeFunction adjustOptions (InitialBalance bal) (Pay amount) policy = do
    utxo  <- genU bal
    payee <- genP amount
    key   <- arbitrary
    let options = adjustOptions (newOptions feeFunction)
    res <- bimap STB identity <$>
             policy
               options
               (genUniqueChangeAddress utxo payee)
               (mkTx key)
               maxNumInputs
               (fmap Core.TxOutAux payee)
               utxo
    return (utxo, payee, res)

payOne :: Core.HasProtocolMagic
       => (Int -> NonEmpty Core.Coin -> Core.Coin)
       -> (CoinSelectionOptions -> CoinSelectionOptions)
       -> InitialBalance
       -> Pay
       -> Policy
       -> Gen RunResult
payOne = pay genUtxoWithAtLeast genPayee

-- | Like 'payOne', but allows a custom 'Gen' for the payees to be supplied
payOne' :: Core.HasProtocolMagic
        => (Word64 -> Gen (NonEmpty Core.TxOut))
        -> (Int -> NonEmpty Core.Coin -> Core.Coin)
        -> (CoinSelectionOptions -> CoinSelectionOptions)
        -> InitialBalance
        -> Pay
        -> Policy
        -> Gen RunResult
payOne' payeeGenerator = pay genUtxoWithAtLeast payeeGenerator

payBatch :: Core.HasProtocolMagic
         => (Int -> NonEmpty Core.Coin -> Core.Coin)
         -> (CoinSelectionOptions -> CoinSelectionOptions)
         -> InitialBalance
         -> Pay
         -> Policy
         -> Gen RunResult
payBatch = pay genUtxoWithAtLeast genPayees

receiverPays :: CoinSelectionOptions -> CoinSelectionOptions
receiverPays o = o { csoExpenseRegulation = ReceiverPaysFee }

spec :: HasConfiguration => Spec
spec =
    describe "Coin selection policies unit tests" $ do
        withMaxSuccess 1000 $ describe "largestFirst" $ do
            prop "one payee, SenderPaysFee, fee = 0" $ forAll (
                 payOne freeLunch identity (InitialBalance 1000) (Pay 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "one payee, ReceiverPaysFee, fee = 0" $ forAll (
                 payOne freeLunch receiverPays (InitialBalance 1000) (Pay 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, SenderPaysFee, fee = 0" $ forAll (
                 payBatch freeLunch identity (InitialBalance 1000) (Pay 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, ReceiverPaysFee, fee = 0" $ forAll (
                 payBatch freeLunch receiverPays (InitialBalance 1000) (Pay 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res

            -- Minimal fee
            prop "one payee, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payOne minFee identity (InitialBalance 1000) (Pay 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "one payee, ReceiverPaysFee, fee = 1 Lovelace" $ forAll (
                payOne minFee receiverPays (InitialBalance 1000) (Pay 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payBatch minFee identity (InitialBalance 1000) (Pay 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, ReceiverPaysFee, fee = 1 Lovelace" $ forAll (
                payBatch minFee receiverPays (InitialBalance 1000) (Pay 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res

        withMaxSuccess 1000 $ describe "random" $ do
            prop "one payee, SenderPaysFee, fee = 0" $ forAll (
                payOne freeLunch identity (InitialBalance 1000) (Pay 100) random
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "one payee, ReceiverPaysFee, fee = 0" $ forAll (
                payOne freeLunch receiverPays (InitialBalance 1000) (Pay 100) random
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, SenderPaysFee, fee = 0" $ forAll (
                payBatch freeLunch identity (InitialBalance 1000) (Pay 100) random
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, ReceiverPaysFee, fee = 0" $ forAll (
                payBatch freeLunch receiverPays (InitialBalance 1000) (Pay 100) random
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res

            -- minimal fee. It doesn't make sense to use it for 'ReceiverPaysFee', because
            -- rounding will essentially cause the computed @epsilon@ will be 0 for each
            -- output. For those cases, we use the 'linear' fee policy.
            prop "one payee, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payOne minFee identity (InitialBalance 1000) (Pay 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed SenderPaysFee]
            prop "multiple payees, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payBatch minFee identity (InitialBalance 1000) (Pay 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed SenderPaysFee]

            -- linear fee
            prop "one payee, ReceiverPaysFee, fee = linear" $ forAll (
                payOne linearFee receiverPays (InitialBalance 1000) (Pay 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed ReceiverPaysFee]
            prop "multiple payees, ReceiverPaysFee, fee = linear" $ forAll (
                payBatch linearFee receiverPays (InitialBalance 1000) (Pay 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed ReceiverPaysFee]

        describe "Expected failures" $ do
            prop "Paying a redeem address should always be rejected" $ forAll (
                payOne' genRedeemPayee linearFee receiverPays (InitialBalance 1000) (Pay 100) random
                ) $ \(utxo, payee, res) ->
                  paymentFailedWith utxo payee res [errorWas outputWasRedeem]
            prop "Paying somebody not having enough money should fail" $ forAll (
                payBatch linearFee receiverPays (InitialBalance 10) (Pay 100) random
                ) $ \(utxo, payee, res) -> do
                  paymentFailedWith utxo payee res [errorWas notEnoughMoney]

        withMaxSuccess 1000 $ describe "Input Grouping" $ do
            prop "one payee, ReceiverPaysFee, fee = linear" $ forAll (
                payOne linearFee receiverPays (InitialBalance 1000) (Pay 100) random
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed ReceiverPaysFee]
