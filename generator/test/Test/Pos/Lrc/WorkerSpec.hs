{-# LANGUAGE TypeFamilies #-}

-- | Specification of 'Pos.Lrc.Worker' (actually only
-- 'lrcSingleShotNoLock' which probably shouldn't be there, but it
-- doesn't matter now).

module Test.Pos.Lrc.WorkerSpec
       ( spec
       ) where

import           Universum

import           Control.Exception.Safe (try)
import           Control.Lens (At (at), Index, _Right)
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import           Formatting (build, int, sformat, (%))
import           Serokell.Util (listJson)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Gen, arbitrary, choose)
import           Test.QuickCheck.Monadic (pick)

import           Pos.Binary.Class (serialize')
import           Pos.Block.Logic (applyBlocksUnsafe)
import           Pos.Block.Slog (ShouldCallBListener (..))
import           Pos.Core (Coin, EpochIndex, GenesisData (..), GenesisInitializer (..),
                           StakeholderId, TestnetBalanceOptions (..), addressHash, blkSecurityParam,
                           coinF, genesisData, genesisSecretKeysPoor, genesisSecretKeysRich)
import           Pos.Core.Block (mainBlockTxPayload)
import           Pos.Core.Txp (TxAux, mkTxPayload)
import           Pos.Crypto (SecretKey, toPublic)
import qualified Pos.GState as GS
import           Pos.Launcher (HasConfigurations)
import qualified Pos.Lrc as Lrc
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.Util (getKeys)

import           Pos.Util.QuickCheck (maybeStopProperty, stopProperty)
import           Test.Pos.Block.Logic.Mode (BlockProperty, TestParams (..), blockPropertyToProperty)
import           Test.Pos.Block.Logic.Util (EnableTxPayload (..), InplaceDB (..), bpGenBlock,
                                            bpGenBlocks)
import           Test.Pos.Block.Property (blockPropertySpec)
import           Test.Pos.Configuration (defaultTestBlockVersionData, withStaticConfigurations)


spec :: Spec
spec = withStaticConfigurations $ \_ -> withCompileInfo def $
    describe "Lrc.Worker" $ modifyMaxSuccess (const 4) $ do
        describe "lrcSingleShot" $ do
            -- Currently we want to run it only 4 times, because there
            -- is no much randomization (its effect is likely
            -- negligible) and performance matters (but not very much,
            -- so we can run more than once).
            modifyMaxSuccess (const 4) $ prop lrcCorrectnessDesc $
                blockPropertyToProperty genTestParams lrcCorrectnessProp
            -- This test is relatively slow, hence we launch it only 15 times.
            modifyMaxSuccess (const 15) $ blockPropertySpec lessThanKAfterCrucialDesc
                lessThanKAfterCrucialProp
  where
    lrcCorrectnessDesc =
        "Computes richmen correctly according to the stake distribution " <>
        "right before the '8 * k'-th slot.\n" <>
        "Computes leaders using follow-the-satoshi algorithm using stake " <>
        "distribution right before the '8 * k'-th slot."
    lessThanKAfterCrucialDesc =
        "Fails for epoch 'e' if there are less than 'k' blocks in slots " <>
        "[(e, 8 * k - 1) .. (e, 10 * k - 1)]"

----------------------------------------------------------------------------
-- Parameters generation
----------------------------------------------------------------------------

genTestParams :: Gen TestParams
genTestParams = do
    let _tpStartTime = 0
    let _tpBlockVersionData = defaultTestBlockVersionData
    _tpGenesisInitializer <- genGenesisInitializer
    return TestParams {..}

genGenesisInitializer :: Gen GenesisInitializer
genGenesisInitializer = do
    giTestBalance <- genTestnetBalanceOptions
    giFakeAvvmBalance <- arbitrary
    giAvvmBalanceFactor <- arbitrary
    -- Currently these tests don't work well with genesis delegation.
    let giUseHeavyDlg = False
    giSeed <- arbitrary
    return GenesisInitializer {..}
  where
    -- We want to be sure that richmen are indeed richmen according to
    -- genesis thresholds from all components and that poor guys are
    -- not richmen.
    genTestnetBalanceOptions :: Gen TestnetBalanceOptions
    genTestnetBalanceOptions = do
        tboPoors <- choose (101, 201)
        -- ↓ should be relatively small so that they are really richmen
        tboRichmen <- choose (1, 5)
        tboTotalBalance <- choose (1000, 10000000)
        -- Here ↓ we simply assume that the lowest threshold (recall
        -- that there are 3 richmen components in the system with
        -- different thresholds) is at least 1e-5 (as there are at
        -- least 101 poors). It's a bit dumb, but simple and sufficient.
        --
        -- Clarification: richmenShare = 0.999 means that poor stakeholders
        -- own 1e-3 stake together. There are more than 100 poor stakeholders.
        -- So each of them owns less than 1e-5. So if a threshold is at least
        -- 1e-5, poor nodes will be poor with respect to this threshold.
        let tboRichmenShare = 0.999
        let tboUseHDAddresses = False
        return TestnetBalanceOptions {..}

----------------------------------------------------------------------------
-- Actual correctness test
----------------------------------------------------------------------------

lrcCorrectnessProp :: (HasConfigurations, HasCompileInfo) => BlockProperty ()
lrcCorrectnessProp = do
    let k = blkSecurityParam
    -- This value is how many blocks we need to generate first. We
    -- want to generate blocks for all slots which will be considered
    -- in LRC except the last one, because we want to include some
    -- special transactions into it.  We don't use 'crucialSlot' or
    -- anything similar, because we don't want to rely on the code,
    -- but rather want to use our knowledge.
    let blkCount0 = 8 * k - 1
    () <$ bpGenBlocks (Just blkCount0) (EnableTxPayload False) (InplaceDB True)
    genAndApplyBlockFixedTxs =<< txsBeforeBoundary
    -- At this point we have applied '8 * k' blocks. The current state
    -- will be used in LRC.
    stableStakes <- lift GS.getAllPotentiallyHugeStakesMap
    -- All further blocks will not be considered by LRC for the 1-st
    -- epoch. So we include some transactions to make sure they are
    -- not considered.
    genAndApplyBlockFixedTxs =<< txsAfterBoundary
    -- We need to have at least 'k' blocks after the boundary to make
    -- sure that stable blocks are indeed stable. Note that we have
    -- already applied 1 blocks, hence 'pred'.
    blkCount1 <- pred <$> pick (choose (k, 2 * k))
    () <$ bpGenBlocks (Just blkCount1) (EnableTxPayload False) (InplaceDB True)
    lift $ Lrc.lrcSingleShot 1
    leaders1 <-
        maybeStopProperty "No leaders for epoch#1!" =<< lift (Lrc.getLeadersForEpoch 1)
    -- Here we use 'genesisSeed' (which is the seed for the 0-th
    -- epoch) because we have a contract that if there is no ssc
    -- payload the previous seed must be reused (which is the case in
    -- this test).
    let genesisSeed = gdFtsSeed genesisData
    -- It's important to sort stakes and iterate in the same order as
    -- DB iteration.
    let sortedStakes = sortOn (serialize' . fst) (HM.toList stableStakes)
    let expectedLeadersStakes =
            Lrc.followTheSatoshi genesisSeed sortedStakes
    when (expectedLeadersStakes /= leaders1) $
        stopProperty $ sformat ("expectedLeadersStakes /= leaders1\n"%
                                "Stakes version: "%listJson%
                                ", computed leaders: "%listJson)
           expectedLeadersStakes leaders1

    checkRichmen

checkRichmen :: HasConfigurations => BlockProperty ()
checkRichmen = do
    checkRichmenStakes =<< getRichmen (lift . Lrc.tryGetSscRichmen)
    checkRichmenFull =<< getRichmen (lift . Lrc.tryGetUSRichmen)
    checkRichmenSet =<< getRichmen (lift . Lrc.tryGetDlgRichmen)
  where
    toStakeholders :: Maybe [SecretKey] -> [StakeholderId]
    toStakeholders = map (addressHash . toPublic) . fromMaybe
        (error "genesis secrets are unknown in tests")
    poorStakeholders :: HasConfigurations => [StakeholderId]
    poorStakeholders = toStakeholders genesisSecretKeysPoor
    richStakeholders :: HasConfigurations => [StakeholderId]
    richStakeholders = toStakeholders genesisSecretKeysRich

    getRichmen ::
           (EpochIndex -> BlockProperty (Maybe richmen))
        -> BlockProperty richmen
    getRichmen getter = maybeStopProperty "No richmen for epoch#1!" =<< getter 1

    checkRichmenFull :: HasConfigurations => Lrc.FullRichmenData -> BlockProperty ()
    checkRichmenFull (totalStake, richmenStakes) = do
        realTotalStake <- lift GS.getRealTotalStake
        unless (totalStake == realTotalStake) $
            stopProperty $ sformat
            ("Total stake returned by LRC differs from the real one: (LRC = "
             %coinF% ", real = " %coinF%")")
             totalStake realTotalStake
        checkRichmenStakes richmenStakes

    checkRichmenStakes :: HasConfigurations => Lrc.RichmenStakes -> BlockProperty ()
    checkRichmenStakes richmenStakes = do
        checkRichmenSet (getKeys richmenStakes)
        let checkRich (id, realStake)
                | Just lrcStake <- richmenStakes ^. at id
                , lrcStake /= realStake =
                    stopProperty $ sformat
                    ("Richman's stake differs from the real one (LRC returned "
                     %coinF%", the real one is "%coinF%")")
                     lrcStake realStake
                | otherwise = pass
        mapM_ checkRich =<< expectedRichmenStakes

    checkRichmenSet :: HasConfigurations => Lrc.RichmenSet -> BlockProperty ()
    checkRichmenSet richmenSet = do
        mapM_ (checkPoor richmenSet) poorStakeholders
        let checkRich (id, realStake) =
                when (isNothing (richmenSet ^. at id)) $
                stopProperty $ sformat
                (build%" has stake "%coinF%", but wasn't considered richman")
                 id realStake
        mapM_ checkRich =<< expectedRichmenStakes

    expectedRichmenStakes :: HasConfigurations => BlockProperty [(StakeholderId, Coin)]
    expectedRichmenStakes = do
        let resolve id = (id, ) . fromMaybe minBound <$> GS.getRealStake id
        lift $ mapM resolve richStakeholders

    checkPoor ::
           (Index m ~ StakeholderId, At m) => m -> StakeholderId -> BlockProperty ()
    checkPoor richmen poorGuy = do
        poorGuyStake <- lift $ fromMaybe minBound <$> GS.getRealStake poorGuy
        unless (isNothing $ richmen ^. at poorGuy) $ do
            totalStake <- lift GS.getRealTotalStake
            stopProperty $ sformat
                ("Poor guy was considered rich by LRC! His stake is "
                 %coinF%", total stake is "%coinF)
                poorGuyStake totalStake

genAndApplyBlockFixedTxs :: (HasConfigurations,HasCompileInfo) => [TxAux] -> BlockProperty ()
genAndApplyBlockFixedTxs txs = do
    let txPayload = mkTxPayload txs
    emptyBlund <- bpGenBlock (EnableTxPayload False) (InplaceDB False)
    let blund = emptyBlund & _1 . _Right . mainBlockTxPayload .~ txPayload
    lift $ applyBlocksUnsafe (ShouldCallBListener False)(one blund) Nothing

-- TODO: we can't change stake in bootstrap era!
-- This part should be implemented in CSL-1450.

txsBeforeBoundary :: BlockProperty [TxAux]
txsBeforeBoundary = pure []

txsAfterBoundary :: BlockProperty [TxAux]
txsAfterBoundary = pure []

-- mkStakeTransfer :: SecretKey -> SecretKey -> BlockProperty TxAux
-- mkStakeTransfer fromSK toSK = do
--     utxo <- lift GS.getAllPotentiallyHugeUtxo -- utxo is small
--     let fromAddr = makePubKeyAddress . toPublic $ fromSK
--     -- We don't care about balances, only stakes, so we won't change
--     -- balances.
--     let toAddr = fromAddr
--     return undefined

----------------------------------------------------------------------------
-- Less than `k` blocks test.
----------------------------------------------------------------------------

lessThanKAfterCrucialProp :: (HasConfigurations, HasCompileInfo) => BlockProperty ()
lessThanKAfterCrucialProp = do
    let k = blkSecurityParam
    -- We need to generate '8 * k' blocks for first '8 * k' slots.
    let inFirst8K = 8 * k
    -- And then we need to generate random number of blocks in range
    -- '[0 .. 2 * k]'.
    inLast2K <- pick (choose (0, 2 * k))
    let toGenerate = inFirst8K + inLast2K
    -- LRC should succeed iff number of blocks in last '2 * k' slots is
    -- at least 'k'.
    let shouldSucceed = inLast2K >= k
    () <$ bpGenBlocks (Just toGenerate) (EnableTxPayload False) (InplaceDB True)
    let mkFormat expectedOutcome =
            ("We expected LRC to " %expectedOutcome % " because there are " %int %
             " blocks after crucial slot, but it failed")
    let unexpectedFailMsg = sformat (mkFormat "succeed") inLast2K
    let unexpectedSuccessMsg = sformat (mkFormat "fail") inLast2K
    lift (try $ Lrc.lrcSingleShot 1) >>= \case
        Left Lrc.UnknownBlocksForLrc
            | shouldSucceed -> stopProperty unexpectedFailMsg
            | otherwise -> pass
        Left e -> lift (throwM e)
        Right ()
            | shouldSucceed -> pass
            | otherwise -> stopProperty unexpectedSuccessMsg
