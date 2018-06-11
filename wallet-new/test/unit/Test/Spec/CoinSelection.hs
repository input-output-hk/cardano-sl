{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
module Test.Spec.CoinSelection (
    spec
  ) where

import           Universum

import           Crypto.Random (MonadRandom)
import qualified Data.Map as Map
import           Test.Hspec (Spec, describe, hspec, it)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Gen, Property, arbitrary, choose, counterexample, suchThat,
                                  vectorOf, (===))
import           Test.QuickCheck.Monadic (PropertyM, assert, monadic', pick, run, stop)

import           Data.Text.Buildable (Buildable (..))
import           Formatting (bprint, (%))
import qualified Formatting as F
import           Serokell.Util.Text (listJsonIndent)

import           Pos.Core (HasConfiguration)
import qualified Pos.Core as Core
import qualified Pos.Txp as Core

import qualified Pos.Txp.Toil.Types as Core
import           Pos.Util.Chrono (getOldestFirst)
import           Test.Infrastructure.Generator
import           Util.Buildable

import           Cardano.Wallet.Kernel.CoinSelection.Policies (defaultPolicy, largestFirst,
                                                               utxoBalance)
import           Cardano.Wallet.Kernel.CoinSelection.Types
import           Pos.Crypto.Signing.Safe (fakeSigner)

genUtxoWithAtLeast :: Word64 -> Gen Core.Utxo
genUtxoWithAtLeast amountToCover = do
    txIn <- Core.TxInUtxo <$> arbitrary <*> arbitrary
    coin <- choose (1, amountToCover)
    addr <- arbitrary
    let txOutAux = Core.TxOutAux (Core.TxOut addr (Core.mkCoin coin))
    let utxo = Map.singleton txIn txOutAux
    case utxoBalance utxo of
        bal | Core.getCoin bal >= amountToCover -> return utxo
            | otherwise ->
            case (Core.mkCoin amountToCover) `Core.subCoin` bal of
                Nothing -> return utxo
                Just remaining ->
                    (utxo `Map.union`) <$> (genUtxoWithAtLeast (Core.getCoin remaining))

-- | A fee-estimation policy which doesn't calculate any fee.
freeLunch :: Int -> NonEmpty Core.Coin -> Core.Coin
freeLunch _ _ = Core.mkCoin 0

genPayee :: Word64
         -- ^ The amount of the payment
         -> Gen (NonEmpty Core.TxOut)
genPayee 0 = error "Precondition failed: You cannot pay 0 into an output."
genPayee amountToCover = do
    given <- Core.mkCoin <$> choose (1, amountToCover)
    addr  <- arbitrary `suchThat` (not . Core.isRedeemAddress)
    let txOut = Core.TxOut addr given
    case ((Core.mkCoin amountToCover) `Core.subCoin` given) of
        Nothing -> pure (txOut :| [])
        Just (Core.Coin remaining)
            | remaining == 0 -> pure (txOut :| [])
            | otherwise      -> ((<>) (pure txOut)) <$> genPayee remaining

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
paymentSucceeded utxo payee res =
    let msg = "Selection failed for Utxo with balance = " <> show (utxoBalance utxo) <>
              " and payment amount of " <> show (paymentAmount payee)
    in failIf msg isRight res

spec :: HasConfiguration => Spec
spec =
    describe "Coin selection policies unit tests" $ do
        prop "Sending a payment with largestFirst, one payee and fee = 0 works" $ runIt $ do
            utxo  <- pick $ genUtxoWithAtLeast 200
            (payee :| _) <- pick $ genPayee 100
            key   <- pick arbitrary
            let options = newOptions freeLunch (\_ -> Right $ fakeSigner key)
            res <- run $ largestFirst options (TM $ lift arbitrary) utxo (payee :| [])
            stop (paymentSucceeded utxo (payee :| []) res)
        prop "Sending a payment with defaultPolicy and fee = 0 works" $ runIt $ do
            utxo  <- pick $ genUtxoWithAtLeast 200
            (payee :| _) <- pick $ genPayee 100
            key   <- pick arbitrary
            let options = newOptions freeLunch (\_ -> Right $ fakeSigner key)
            res <- run $ defaultPolicy options (TM $ lift arbitrary) utxo (payee :| [])
            stop (paymentSucceeded utxo (payee :| []) res)
