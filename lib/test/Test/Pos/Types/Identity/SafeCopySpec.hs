-- | This module tests SafeCopy instances.

module Test.Pos.Types.Identity.SafeCopySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (generate)

import qualified Pos.Core as Core
import qualified Pos.Core.Txp as Txp
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.SafeCopy ()

import           Test.Pos.Binary.Helpers (safeCopyTest)
import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Crypto.Arbitrary (genProtocolMagicUniformWithRNM)
import           Test.Pos.Txp.Arbitrary ()
import           Test.Pos.Txp.Arbitrary.Network ()


-- We run the tests this number of times, with different `ProtocolMagics`, to get increased
-- coverage. We should really do this inside of the `prop`, but it is difficult to do that
-- without significant rewriting of the testsuite.
testMultiple :: Int
testMultiple = 3

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = replicateM_ testMultiple $
    modifyMaxSuccess (`div` testMultiple) $ do
        pm <- runIO (generate (genProtocolMagicUniformWithRNM rnm))
        describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
            specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $ describe "Types" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @Core.EpochIndex
        safeCopyTest @Core.LocalSlotIndex
        safeCopyTest @Core.SlotId
        safeCopyTest @Core.Coin
        safeCopyTest @Core.Address
        safeCopyTest @Core.Address'
        safeCopyTest @Core.SharedSeed
        safeCopyTest @Core.ChainDifficulty
        safeCopyTest @Core.VssCertificate

        safeCopyTest @Txp.TxInWitness
        safeCopyTest @Txp.TxIn
        safeCopyTest @Txp.TxOut
        safeCopyTest @Txp.Tx
