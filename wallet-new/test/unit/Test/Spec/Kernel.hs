module Test.Spec.Kernel (
    spec
  ) where

import           Universum

import qualified Data.Set as Set

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import           Pos.Core (Coeff (..), TxSizeLinear (..))

import           Test.Infrastructure.Generator
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

import qualified Wallet.Basic as Base

{-------------------------------------------------------------------------------
  Compare the wallet kernel with the pure model
-------------------------------------------------------------------------------}

spec :: Spec
spec =
    it "Compare wallet kernel to pure model" $
      forAll (genInductiveUsingModel model) $ \ind -> do
        -- TODO: remove once we have support for rollback in the kernel
        let indDontRoll = uptoFirstRollback ind
        bracketActiveWallet $ \activeWallet -> do
          checkEquivalent activeWallet indDontRoll
  where
    transCtxt = runTranslateNoErrors ask
    boot      = bootstrapTransaction transCtxt
    model     = (cardanoModel linearFeePolicy boot) {
                    gmMaxNumOurs    = 1
                  , gmPotentialOurs = isPoorAddr
                  }
    linearFeePolicy = TxSizeLinear (Coeff 155381) (Coeff 43.946)

    checkEquivalent :: forall h. Hash h Addr
                    => Kernel.ActiveWallet
                    -> Inductive h Addr
                    -> Expectation
    checkEquivalent activeWallet ind = do
       shouldReturnValidated $ runTranslateT $ do
         equivalentT activeWallet (encKpEnc ekp) (mkWallet (== addr)) ind
      where
        [addr]       = Set.toList $ inductiveOurs ind
        AddrInfo{..} = resolveAddr addr transCtxt
        Just ekp     = addrInfoMasterKey

    -- TODO: We should move to the full model instead of the base model
    mkWallet :: Hash h Addr => Ours Addr -> Transaction h Addr -> Wallet h Addr
    mkWallet = walletBoot Base.walletEmpty

{-------------------------------------------------------------------------------
  Wallet resource management
-------------------------------------------------------------------------------}

-- | Initialize passive wallet in a manner suitable for the unit tests
bracketPassiveWallet :: (Kernel.PassiveWallet -> IO a) -> IO a
bracketPassiveWallet = Kernel.bracketPassiveWallet logMessage
  where
   -- TODO: Decide what to do with logging
    logMessage _sev txt = print txt

-- | Initialize active wallet in a manner suitable for generator-based testing
bracketActiveWallet :: (Kernel.ActiveWallet -> IO a) -> IO a
bracketActiveWallet test =
    bracketPassiveWallet $ \passive ->
      Kernel.bracketActiveWallet passive diffusion $ \active ->
        test active

-- TODO: Decide what we want to do with submitted transactions
diffusion :: Kernel.WalletDiffusion
diffusion =  Kernel.WalletDiffusion {
    walletSendTx = \_tx -> return False
  }
