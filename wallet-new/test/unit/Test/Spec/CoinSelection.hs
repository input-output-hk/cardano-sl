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
--import           UTxO.Context
--import           UTxO.DSL
--import           UTxO.Interpreter (int, runIntT)
--import           UTxO.Translate

import           Cardano.Wallet.Kernel.CoinSelection.Policies (defaultPolicy, utxoBalance)
import           Cardano.Wallet.Kernel.CoinSelection.Types
import           Pos.Crypto.Signing.Safe (fakeSigner)

{--
genUtxo :: HasConfiguration => Gen Core.Utxo
genUtxo = do
    chain <- genChainUsingModel model
    return $ fst $ runTranslate $ runIntT _ $ int (slurp chain)
    where
        slurp = foldl' (flip utxoApplyBlock) utxoEmpty . getOldestFirst
        transCtxt = initContext initCardanoContext
        boot      = bootstrapTransaction transCtxt
        model     = (cardanoModel boot) {
                        gmMaxNumOurs    = 1
                      , gmPotentialOurs = isPoorAddr
                      }
--}

genUtxo :: Word64 -> Gen Core.Utxo
genUtxo amountToCover = do
    utxo <- arbitrary `suchThat` (\m -> Map.size m >= 0 && Core.getCoin (utxoBalance m) <= Core.maxCoinVal)
    case utxoBalance utxo of
        bal | Core.getCoin bal >= amountToCover -> return utxo
            | otherwise -> (utxo `Map.union`) <$> (genUtxo $ amountToCover - (Core.getCoin bal))


-- | A fee-estimation policy which doesn't calculate any fee.
freeLunch :: Int -> NonEmpty Core.Coin -> Core.Coin
freeLunch _ _ = Core.mkCoin 0

genPayee :: Word64
         -- ^ The amount of the payment
         -> Gen (NonEmpty Core.TxOut)
genPayee 0 = error "Precondition failed: You cannot pay 0 into an output."
genPayee amountToCover = do
    given <- choose (1, amountToCover)
    addr  <- arbitrary `suchThat` (not . Core.isRedeemAddress . Core.txOutAddress)
    case (amountToCover - given) of
        remaining | remaining <= 0 -> pure (addr :| [])
                  | otherwise      -> ((<>) (pure addr)) <$> genPayee remaining

newtype TestMonad a = TM { runTest :: IdentityT Gen a }
  deriving (Functor, Applicative, Monad, MonadRandom)

instance RunPolicy TestMonad Core.Address where
    genChangeAddr = TM (lift arbitrary)

runIt :: PropertyM TestMonad a -> Gen Property
runIt m = monadic' m >>= runIdentityT . runTest

instance (Buildable a, Buildable b) => Buildable (Either a b) where
    build (Right r) = bprint ("Right " % F.build) r
    build (Left l)  = bprint ("Left "  % F.build) l

instance Buildable [CoinSelectionFailure Core.Address] where
    build vals = bprint (listJsonIndent 4) vals

failIf :: Buildable a => String -> (a -> Bool) -> a -> Property
failIf label p a = counterexample msg (p a)
    where
        msg = label <> " triggered on " <> show (STB a)

spec :: HasConfiguration => Spec
spec =
    describe "Coin selection policies unit tests" $ do
        prop "Sending a payment with fee = 0 works" $ runIt $ do
            utxo  <- pick $ genUtxo 200
            payee <- pick $ genPayee 100
            key   <- pick arbitrary
            let options = newOptions freeLunch (\_ -> Right $ fakeSigner key)
            res <- run $ defaultPolicy options utxo payee
            stop (failIf "Returns an error" isRight res)
