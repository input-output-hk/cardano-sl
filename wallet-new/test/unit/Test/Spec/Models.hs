module Test.Spec.Models (
    spec
  ) where

import           Universum

import qualified Data.Set as Set

import           Test.Infrastructure.Generator
import           Util.Buildable.Hspec
import           Util.Buildable.QuickCheck
import           UTxO.Bootstrap
import           UTxO.DSL
import           UTxO.Translate
import           Wallet.Abstract
import           Wallet.Inductive
import           Wallet.Inductive.Invariants
import           Wallet.Inductive.Validation

import qualified Wallet.Basic as Base
import qualified Wallet.Incremental as Incr
import qualified Wallet.Prefiltered as Pref
import qualified Wallet.Rollback.Basic as Roll
import qualified Wallet.Rollback.Full as Full

import           Pos.Core (Coeff (..), TxSizeLinear (..))
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))

{-------------------------------------------------------------------------------
  Pure wallet tests
-------------------------------------------------------------------------------}

-- | Test the pure wallet models
spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm =
    describe "Test pure wallets" $ do
      let rnm = getRequiresNetworkMagic pm
      it "Using simple model" $
        forAll (genInductiveUsingModel rnm simpleModel) $ testPureWalletWith
      it "Using Cardano model" $
        forAll (genInductiveUsingModel rnm (cardanoModel linearFeePolicy boot)) $ testPureWalletWith
  where
    transCtxt = runTranslateNoErrors pm ask
    boot      = bootstrapTransaction transCtxt
    linearFeePolicy = TxSizeLinear (Coeff 155381) (Coeff 43.946)

testPureWalletWith :: forall h a. (Hash h a, Ord a, Buildable a)
                   => Inductive h a -> Property
testPureWalletWith indWithRoll = conjoin [
      -- sanity check on the test
      shouldBeValidated (void (inductiveIsValid indWithRoll))

      -- check that the invariants hold in each model
    , checkInvariants NoRollback    "base" indDontRoll baseEmpty
    , checkInvariants NoRollback    "incr" indDontRoll incrEmpty
    , checkInvariants NoRollback    "pref" indDontRoll prefEmpty
    , checkInvariants BasicRollback "roll" indWithRoll rollEmpty
    , checkInvariants FullRollback  "full" indWithRoll fullEmpty

      -- check equivalence between the models (no rollbacks)
    , checkEquivalent "base/incr" indDontRoll baseEmpty incrEmpty
    , checkEquivalent "base/pref" indDontRoll baseEmpty prefEmpty
    , checkEquivalent "base/roll" indDontRoll baseEmpty rollEmpty
    , checkEquivalent "base/full" indDontRoll baseEmpty fullEmpty

      -- check equivalence between models (with rollbacks)
    , checkEquivalent "roll/full" indWithRoll rollEmpty fullEmpty
    ]
  where
    -- Prefix of the 'Inductive' without any rollbacks
    indDontRoll :: Inductive h a
    indDontRoll = uptoFirstRollback indWithRoll

    checkInvariants :: ApplicableInvariants
                    -> Text
                    -> Inductive h a
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> Expectation
    checkInvariants applicableInvariants label ind@Inductive{..} w =
        shouldBeValidated $
          walletInvariants applicableInvariants label (w inductiveOurs) ind

    checkEquivalent :: Text
                    -> Inductive h a
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> Expectation
    checkEquivalent label ind@Inductive{..} w w' =
        shouldBeValidated $
          walletEquivalent label (w inductiveOurs) (w' inductiveOurs) ind

    oursFromSet :: Set a -> Ours a
    oursFromSet = flip Set.member

    baseEmpty :: Set a -> Transaction h a -> Wallet h a
    incrEmpty :: Set a -> Transaction h a -> Wallet h a
    prefEmpty :: Set a -> Transaction h a -> Wallet h a
    rollEmpty :: Set a -> Transaction h a -> Wallet h a
    fullEmpty :: Set a -> Transaction h a -> Wallet h a

    baseEmpty = walletBoot Base.walletEmpty . oursFromSet
    incrEmpty = walletBoot Incr.walletEmpty . oursFromSet
    prefEmpty = walletBoot Pref.walletEmpty . oursFromSet
    rollEmpty = walletBoot Roll.walletEmpty . oursFromSet
    fullEmpty = walletBoot Full.walletEmpty . oursFromSet
