module Test.Spec.Kernel (
    spec
  ) where

import           Universum

import qualified Data.Set as Set

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor (nodeStateUnavailable)
import           Pos.Core (Coeff (..), TxSizeLinear (..))
import           Pos.Core.Chrono

import           Test.Infrastructure.Generator
import           Test.Infrastructure.Genesis
import           Test.Pos.Configuration (withDefConfiguration)
import           Util.Buildable.Hspec
import           Util.Buildable.QuickCheck
import           UTxO.Bootstrap
import           UTxO.Context
import           UTxO.Crypto
import           UTxO.DSL
import           UTxO.Translate
import           Wallet.Abstract
import           Wallet.Inductive
import           Wallet.Inductive.Cardano
import           Wallet.Inductive.Validation

import qualified Wallet.Rollback.Full as Full

{-------------------------------------------------------------------------------
  Compare the wallet kernel with the pure model
-------------------------------------------------------------------------------}

spec :: Spec
spec =
    describe "Compare wallet kernel to pure model" $ do
      describe "Using hand-written inductive wallets" $ do
        it "computes identical results in presence of dependent pending transactions" $
          bracketActiveWallet $ \activeWallet -> do
            checkEquivalent activeWallet (dependentPending genesis)
      it "computes identical results using generated inductive wallets" $
        forAll (genInductiveUsingModel model) $ \ind -> do
          conjoin [
              shouldBeValidated $ void (inductiveIsValid ind)
            , bracketActiveWallet $ \activeWallet -> do
                checkEquivalent activeWallet ind
            ]
  where
    transCtxt = runTranslateNoErrors ask
    boot      = bootstrapTransaction transCtxt
    model     = (cardanoModel linearFeePolicy boot) {
                     gmMaxNumOurs    = 1
                   , gmPotentialOurs = isPoorAddr
                   }

    -- TODO: These constants should not be hardcoded here.
    linearFeePolicy :: TxSizeLinear
    linearFeePolicy = TxSizeLinear (Coeff 155381) (Coeff 43.946)

    genesis :: GenesisValues GivenHash Addr
    genesis = genesisValues linearFeePolicy boot

    checkEquivalent :: forall h. Hash h Addr
                    => Kernel.ActiveWallet
                    -> Inductive h Addr
                    -> Expectation
    checkEquivalent activeWallet ind = do
       shouldReturnValidated $ runTranslateTNoErrors $ do
         equivalentT activeWallet (encKpEnc ekp) (mkWallet (== addr)) ind
      where
        [addr]       = Set.toList $ inductiveOurs ind
        AddrInfo{..} = resolveAddr addr transCtxt
        Just ekp     = addrInfoMasterKey

    mkWallet :: Hash h Addr => Ours Addr -> Transaction h Addr -> Wallet h Addr
    mkWallet = walletBoot Full.walletEmpty

{-------------------------------------------------------------------------------
  Manually written inductives

  NOTE: In order to test the wallet we want a HD structure. This means that
  the rich actors are not suitable test subjects.
-------------------------------------------------------------------------------}

-- | Inductive where the rollback causes dependent transactions to exist
--
-- This tests that when we report the 'change' of the wallet, we don't include
-- any outputs from pending transactions that are consumed by /other/ pending
-- transactions.
dependentPending :: forall h. Hash h Addr
                 => GenesisValues h Addr -> Inductive h Addr
dependentPending GenesisValues{..} = Inductive {
      inductiveBoot   = boot
    , inductiveOurs   = Set.singleton p0
    , inductiveEvents = OldestFirst [
          NewPending t0                  -- t0 pending
        , ApplyBlock $ OldestFirst [t0]  -- t0 new confirmed, change available
        , NewPending t1                  -- t1 pending, uses change from t0
        , Rollback                       -- now we have a dependent pending tr
        ]
    }
  where
    fee = overestimate txFee 1 2

    t0 :: Transaction h Addr
    t0 = Transaction {
             trFresh = 0
           , trIns   = Set.fromList [ fst initUtxoP0 ]
           , trOuts  = [ Output p1 1000
                       , Output p0 (initBalP0 - 1 * (1000 + fee))
                       ]
           , trFee   = fee
           , trHash  = 1
           , trExtra = []
           }

    t1 :: Transaction h Addr
    t1 = Transaction {
             trFresh = 0
           , trIns   = Set.fromList [ Input (hash t0) 1 ]
           , trOuts  = [ Output p1 1000
                       , Output p0 (initBalP0 - 2 * (1000 + fee))
                       ]
           , trFee   = fee
           , trHash  = 2
           , trExtra = []
           }

{-------------------------------------------------------------------------------
  Wallet resource management
-------------------------------------------------------------------------------}

-- | Initialize passive wallet in a manner suitable for the unit tests
bracketPassiveWallet :: (Kernel.PassiveWallet -> IO a) -> IO a
bracketPassiveWallet postHook = do
      Keystore.bracketTestKeystore $ \keystore ->
          Kernel.bracketPassiveWallet logMessage keystore nodeStateUnavailable postHook
  where
   -- TODO: Decide what to do with logging.
   -- For now we are not logging them to stdout to not alter the output of
   -- the test runner, but in the future we could store them into a mutable
   -- reference or a TBQueue and perform assertions on them.
    logMessage _ _  = return ()

-- | Initialize active wallet in a manner suitable for generator-based testing
bracketActiveWallet :: (Kernel.ActiveWallet -> IO a) -> IO a
bracketActiveWallet test =
    withDefConfiguration $ \pm -> do
        bracketPassiveWallet $ \passive ->
          Kernel.bracketActiveWallet pm passive diffusion $ \active ->
            test active

-- TODO: Decide what we want to do with submitted transactions
diffusion :: Kernel.WalletDiffusion
diffusion =  Kernel.WalletDiffusion {
    walletSendTx = \_tx -> return False
  }
