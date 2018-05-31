{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Test.Spec.CoinSelection (
    spec
  ) where

import           Universum

import           Crypto.Random (MonadRandom)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Gen, Property, arbitrary, choose, counterexample, suchThat)
import           Test.QuickCheck.Monadic (PropertyM, monadic', pick, run, stop)

import           Data.Text.Buildable (Buildable (..))
import           Formatting (bprint, (%))
import qualified Formatting as F
import           Serokell.Util.Text (listJsonIndent)
import qualified Text.Tabl as Tabl

import           Pos.Core (HasConfiguration)
import qualified Pos.Core as Core
import qualified Pos.Txp as Core

import           Util.Buildable

import           Cardano.Wallet.Kernel.CoinSelection.Policies (defaultPolicy, largestFirst,
                                                               utxoBalance)
import           Cardano.Wallet.Kernel.CoinSelection.Types
import           Pos.Crypto.Signing.Safe (fakeSigner)

data UtxoGenOptions = UtxoGenOptions {
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

genUtxo :: UtxoGenOptions -> Gen Core.Utxo
genUtxo o = go mempty (stakeNeeded o) o
    where
        needToStop :: Int -> Maybe Int -> Bool
        needToStop _ Nothing               = False
        needToStop actual (Just requested) = actual >= requested

        finalise :: Core.Utxo -> Core.Coin -> Gen Core.Utxo
        finalise utxo amountToCover = do
            case amountToCover `Core.subCoin` (utxoBalance utxo) of
                 Nothing -> return utxo
                 Just (Core.Coin 0) -> return utxo
                 Just remaining -> do
                     txIn <- Core.TxInUtxo <$> arbitrary <*> arbitrary
                     addr <- arbitrary
                     let txOutAux = Core.TxOutAux (Core.TxOut addr remaining)
                     return $ Map.insert txIn txOutAux utxo

        go :: Core.Utxo -> Core.Coin -> UtxoGenOptions -> Gen Core.Utxo
        go accUtxo amountToCover opts = do
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
                         bal | bal >= (stakeNeeded opts) && (stakeGenerationTarget opts) == AtLeast -> return utxo'
                         bal | bal >= (stakeNeeded opts) && (stakeGenerationTarget opts) == Exactly ->
                                 -- Cover 'amountToCover' exactly, on the old Utxo.
                                 finalise accUtxo amountToCover
                             | otherwise ->
                                 case amountToCover `Core.subCoin` coin of
                                     Nothing        -> error "invariant violated!"
                                     Just remaining -> go utxo' remaining opts

genUtxoWithAtLeast :: Word64 -> Gen Core.Utxo
genUtxoWithAtLeast amountToCover =
    genUtxo $ UtxoGenOptions {
                  stakeOnEachInputPercentage = Just 0.2
                , stakeGenerationTarget = AtLeast
                , stakeNeeded           = Core.mkCoin amountToCover
                , stakeMaxInputsNum     = Just 100
            }

-- | A fee-estimation policy which doesn't calculate any fee.
freeLunch :: Int -> NonEmpty Core.Coin -> Core.Coin
freeLunch _ _ = Core.mkCoin 0

genPayees :: Word64
         -- ^ The amount of the payment
         -> Gen (NonEmpty Core.TxOut)
genPayees 0 = error "Precondition failed: You cannot pay 0 into an output."
genPayees amountToCover = do
    given <- Core.mkCoin <$> choose (1, amountToCover)
    addr  <- arbitrary `suchThat` (not . Core.isRedeemAddress)
    let txOut = Core.TxOut addr given
    case ((Core.mkCoin amountToCover) `Core.subCoin` given) of
        Nothing -> pure (txOut :| [])
        Just (Core.Coin remaining)
            | remaining == 0 -> pure (txOut :| [])
            | otherwise      -> ((<>) (pure txOut)) <$> genPayees remaining

newtype TestMonad a = TM { runTest :: IdentityT Gen a }
  deriving (Functor, Applicative, Monad, MonadRandom)

runIt :: PropertyM TestMonad a -> Gen Property
runIt m = monadic' m >>= runIdentityT . runTest

instance (Buildable a, Buildable b) => Buildable (Either a b) where
    build (Right r) = bprint ("Right " % F.build) r
    build (Left l)  = bprint ("Left "  % F.build) l

instance Buildable [CoinSelectionFailure Core.Address] where
    build vals = bprint (listJsonIndent 4) vals

paymentAmount :: NonEmpty Core.TxOut -> TotalOutput
paymentAmount = TotalOutput . Core.unsafeIntegerToCoin
                            . Core.sumCoins
                            . map Core.txOutValue
                            . toList

-- Fail if the supplied predicate does not hold.
failIf :: Buildable a => String -> (a -> Bool) -> a -> Property
failIf label p a = counterexample msg (p a)
    where
        msg = label <> " triggered on " <> show (STB a)

paymentSucceeded :: (Buildable a, Buildable b)
                 => Core.Utxo -> NonEmpty Core.TxOut -> Either a b -> Property
paymentSucceeded utxo payees res =
    let msg = "Selection failed for Utxo with balance = " <> show (utxoBalance utxo) <>
              " and payment amount of " <> show (paymentAmount payees) <> "\n." <>
              "\n\n===  UTXO  ===\n\n" <> T.unpack (renderUtxo utxo) <>
              "\n\n=== PAYEES ===\n\n" <> T.unpack (renderPayees payees) <> "\n\n"
    in failIf msg isRight res

renderPayees :: NonEmpty Core.TxOut -> T.Text
renderPayees outputs = Tabl.tabl env hDec vDec alignments cells
  where
      env :: Tabl.Environment
      env = Tabl.EnvAscii

      hDec :: Tabl.Decoration
      hDec = Tabl.DecorAll

      vDec :: Tabl.Decoration
      vDec = Tabl.DecorAll

      alignments :: [Tabl.Alignment]
      alignments = map (const Tabl.AlignCentre) (toList outputs)

      cells :: [[Text]]
      cells = ["Payment Amount"] : map toCell (toList outputs)

      toCell :: Core.TxOut -> [T.Text]
      toCell txOut =
        [T.pack $ show $ Core.getCoin $ Core.txOutValue $ txOut]

renderUtxo :: Core.Utxo -> T.Text
renderUtxo utxo = Tabl.tabl env hDec vDec alignments cells
  where
      env :: Tabl.Environment
      env = Tabl.EnvAscii

      hDec :: Tabl.Decoration
      hDec = Tabl.DecorAll

      vDec :: Tabl.Decoration
      vDec = Tabl.DecorAll

      alignments :: [Tabl.Alignment]
      alignments = map (const Tabl.AlignCentre) (Map.toList utxo)

      cells :: [[Text]]
      cells = ["Amount"] : map toCell (Map.toList utxo)

      toCell :: (Core.TxIn, Core.TxOutAux) -> [T.Text]
      toCell (_, txOutAux) =
        [T.pack $ show $ Core.getCoin $ Core.txOutValue . Core.toaOut $ txOutAux]

spec :: HasConfiguration => Spec
spec =
    describe "Coin selection policies unit tests" $ do
        describe "largestFirst" $ do
            prop "one payee, SenderPaysFee, fee = 0" $ runIt $ do
                utxo  <- pick $ genUtxoWithAtLeast 200
                (payee :| _) <- pick $ genPayees 100
                key   <- pick arbitrary
                let options = newOptions freeLunch (\_ -> Right $ fakeSigner key)
                res <- run $ largestFirst options (TM $ lift arbitrary) utxo (payee :| [])
                stop (paymentSucceeded utxo (payee :| []) res)
            prop "multiple payees, SenderPaysFee, fee = 0" $ runIt $ do
                utxo  <- pick $ genUtxoWithAtLeast 200
                payees <- pick $ genPayees 100
                key   <- pick arbitrary
                let options = newOptions freeLunch (\_ -> Right $ fakeSigner key)
                res <- run $ largestFirst options (TM $ lift arbitrary) utxo payees
                stop (paymentSucceeded utxo payees res)
            prop "multiple payees, SenderPaysFee, fee = 0" $ runIt $ do
                utxo  <- pick $ genUtxoWithAtLeast 200
                payees <- pick $ genPayees 100
                key   <- pick arbitrary
                let options = newOptions freeLunch (\_ -> Right $ fakeSigner key)
                res <- run $ largestFirst options (TM $ lift arbitrary) utxo payees
                stop (paymentSucceeded utxo payees res)
        describe "defaultPolicy" $ do
            prop "one payee, SenderPaysFee, fee = 0" $ runIt $ do
                utxo  <- pick $ genUtxoWithAtLeast 200
                (payee :| _) <- pick $ genPayees 100
                key   <- pick arbitrary
                let options = newOptions freeLunch (\_ -> Right $ fakeSigner key)
                res <- run $ defaultPolicy options (TM $ lift arbitrary) utxo (payee :| [])
                stop (paymentSucceeded utxo (payee :| []) res)
            prop "multiple payees, SenderPaysFee, fee = 0" $ runIt $ do
                utxo   <- pick $ genUtxoWithAtLeast 200
                payees <- pick $ genPayees 100
                key   <- pick arbitrary
                let options = newOptions freeLunch (\_ -> Right $ fakeSigner key)
                res <- run $ defaultPolicy options (TM $ lift arbitrary) utxo payees
                stop (paymentSucceeded utxo payees res)
