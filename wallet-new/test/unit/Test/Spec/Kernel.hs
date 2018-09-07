module Test.Spec.Kernel (
    spec
  ) where

import           Universum

import qualified Data.Set as Set

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import           Pos.Core (Coeff (..), TxSizeLinear (..))
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))

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
    it "Compare wallet kernel to pure model" $
      let rnm = getRequiresNetworkMagic pm in
      forAll (genInductiveUsingModel rnm model) $ \ind -> do
        -- TODO: remove once we have support for rollback in the kernel
        let indDontRoll = uptoFirstRollback ind
        bracketActiveWallet pm $ \activeWallet -> do
          checkEquivalent activeWallet indDontRoll
  where
    transCtxt = runTranslateNoErrors pm ask
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
       shouldReturnValidated $ runTranslateT pm $ do
         equivalentT activeWallet (encKpHash ekp, encKpEnc ekp) (mkWallet (== addr)) ind
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
bracketPassiveWallet :: ProtocolMagic -> (Kernel.PassiveWallet -> IO a) -> IO a
bracketPassiveWallet _pm = Kernel.bracketPassiveWallet logMessage
  where
   -- TODO: Decide what to do with logging
    logMessage _sev txt = print txt

-- | Initialize active wallet in a manner suitable for generator-based testing
bracketActiveWallet :: ProtocolMagic -> (Kernel.ActiveWallet -> IO a) -> IO a
bracketActiveWallet pm test =
    bracketPassiveWallet pm $ \passive ->
      Kernel.bracketActiveWallet passive diffusion $ \active ->
        test active

-- TODO: Decide what we want to do with submitted transactions
diffusion :: Kernel.WalletDiffusion
diffusion =  Kernel.WalletDiffusion {
    walletSendTx = \_tx -> return False
  }
