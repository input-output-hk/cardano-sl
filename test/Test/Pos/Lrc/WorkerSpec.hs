-- | Specification of 'Pos.Lrc.Worker' (actually only
-- 'lrcSingleShotNoLock' which probably shouldn't be there, but it
-- doesn't matter now).

module Test.Pos.Lrc.WorkerSpec
       ( spec
       ) where

import           Universum

import           Control.Lens              (_head, _tail)
import qualified Data.HashMap.Strict       as HM
import           Serokell.Util             (enumerate, subList)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (modifyMaxSuccess, prop)
import           Test.QuickCheck           (Gen, arbitrary)

import qualified Pos.Constants             as Const
import           Pos.Core                  (Address, Coin, applyCoinPortionUp,
                                            makePubKeyAddress, mkCoin, unsafeAddCoin,
                                            unsafeSubCoin)
import           Pos.Crypto                (SecretKey, toPublic)
import           Pos.Generator.Block       (AllSecrets (..), mkInvSecretsMap)
import           Pos.Genesis               (StakeDistribution (..), genesisUtxo)
import qualified Pos.GState                as GS
import qualified Pos.Lrc                   as Lrc
import           Pos.Txp                   (mkGenesisTxpContext)
import           Pos.Util.Arbitrary        (nonrepeating)

import           Test.Pos.Block.Logic.Mode (BlockProperty, TestParams (..),
                                            blockPropertyToProperty)
import           Test.Pos.Block.Logic.Util (EnableTxPayload (..), InplaceDB (..),
                                            bpGenBlocks)
import           Test.Pos.Util             (maybeStopProperty, stopProperty)

spec :: Spec
-- Currently we want to run it only once, because there is no much
-- randomization (its effect is likely negligible) and performance is
-- the issue.
spec = describe "Block.Logic.VAR" $ modifyMaxSuccess (const 1) $ do
    describe "lrcSingleShotNoLock" $ do
        prop lrcCorrectnessDesc $
            blockPropertyToProperty genTestParams lrcCorrectnessProp
  where
    lrcCorrectnessDesc =
        "Computes richmen correctly according to the stake distribution " <>
        "right before the '8 * k'-th slot.\n" <>
        "Computes leaders using follow-the-satoshi algorithm using stake " <>
        "distribution or utxo right before the '8 * k'-th slot."

-- | We need to generate some genesis with
-- genesis stakeholders `RC × {A, B, C, D}` (where `RC` is the set of
-- all richmen components and `{A, B, C, D}` is just a set of 4 items)
-- and make sure that there are `|RC| · 3` richmen (`RC × {A, B, C}`).
genTestParams :: Gen TestParams
genTestParams = do
    let _tpStartTime = 0
    let stakeholdersNum = 4 * length Lrc.richmenComponents
    secretKeys <- nonrepeating stakeholdersNum
    let invSecretsMap = mkInvSecretsMap secretKeys
    let _tpAllSecrets = AllSecrets invSecretsMap
    totalStake <- max minTotalStake <$> arbitrary
    addressesAndDistrs <-
        mapM
            (genAddressesAndDistrs totalStake secretKeys)
            (enumerate Lrc.richmenComponents)
    let _tpStakeDistributions = snd <$> addressesAndDistrs
    let utxo = genesisUtxo Nothing addressesAndDistrs
    let _tpGenTxpContext = mkGenesisTxpContext utxo
    return TestParams {..}
  where
    minTotalStake = mkCoin 100000
    genAddressesAndDistrs ::
           Coin
        -> [SecretKey]
        -> (Int, Lrc.SomeRichmenComponent)
        -> Gen ([Address], StakeDistribution)
    genAddressesAndDistrs totalStake allSecretKeys (i, Lrc.SomeRichmenComponent proxy) = do
        let secretKeysRange = subList (4 * i, 4 * (i + 1) - 1) allSecretKeys
        let skToAddr = makePubKeyAddress . toPublic
        let addresses = map skToAddr secretKeysRange
        let thresholdCoin =
                Lrc.rcInitialThreshold proxy `applyCoinPortionUp` totalStake
        -- Let's add some stake to richmen just for fun. Total stake
        -- is at least 100000, so it should be harmless.
        let genSummand = min (mkCoin 10) <$> arbitrary
        summands <- replicateM 3 genSummand
        let subtrahend = mkCoin 1
        -- We give 'thresholdCoin' to 4 stakeholders and all of them
        -- become richmen.  Then we decrease stake of one of them by 1
        -- and this stakeholder is no loger richman.  Then we add
        -- small amount of coins to richmen.
        let stakes =
                replicate 4 thresholdCoin &
                _head %~ flip unsafeSubCoin subtrahend &
                _tail %~ zipWith unsafeAddCoin summands
        return (addresses, CustomStakes stakes)

lrcCorrectnessProp :: BlockProperty ()
lrcCorrectnessProp = do
    -- We don't use 'crucialSlot' or anything similar, because we
    -- don't want to rely on the code, but rather want to use our knowledge.
    let blockCount0 = 8 * Const.blkSecurityParam
    () <$ bpGenBlocks (Just blockCount0) (EnableTxPayload False) (InplaceDB True)
    stableUtxo <- lift GS.getAllPotentiallyHugeUtxo
    stableStakes <- lift GS.getAllPotentiallyHugeStakesMap
    let blockCount1 = 2 * Const.blkSecurityParam
    () <$ bpGenBlocks (Just blockCount1) (EnableTxPayload False) (InplaceDB True)
    lift $ Lrc.lrcSingleShotNoLock 1
    leaders1 <-
        maybeStopProperty "No leaders for epoch#1!" =<< lift (Lrc.getLeaders 1)
    -- Here we use 'genesisSeed' (which is the seed for the 0-th
    -- epoch) because we have a contract that if there is no ssc
    -- payload the previous seed must be reused (which is the case in
    -- this test).
    let expectedLeadersUtxo =
            Lrc.followTheSatoshiUtxo Lrc.genesisSeed stableUtxo
    let expectedLeadersStakes =
            Lrc.followTheSatoshi Lrc.genesisSeed (HM.toList stableStakes)
    unless (expectedLeadersUtxo /= leaders1) $
        stopProperty "expectedLeadersUtxo /= leaders1"
    unless (expectedLeadersStakes /= leaders1) $
        stopProperty "expectedLeadersStakes /= leaders1"
