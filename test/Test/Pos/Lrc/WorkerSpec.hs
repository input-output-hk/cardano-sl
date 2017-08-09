{-# LANGUAGE TypeFamilies #-}

-- | Specification of 'Pos.Lrc.Worker' (actually only
-- 'lrcSingleShotNoLock' which probably shouldn't be there, but it
-- doesn't matter now).

module Test.Pos.Lrc.WorkerSpec
       ( spec
       ) where

import           Universum
import           Unsafe                    (unsafeHead, unsafeTail)

import           Control.Lens              (At (at), Index, _Right)
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map.Strict           as M
import           Formatting                (sformat, (%))
import           Serokell.Util             (enumerate, listJson, pairF, subList)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (modifyMaxSuccess, prop)
import           Test.QuickCheck           (Gen, choose)
import           Test.QuickCheck.Monadic   (pick)

import           Pos.Block.Core            (mainBlockTxPayload)
import           Pos.Block.Logic           (applyBlocksUnsafe)
import qualified Pos.Constants             as Const
import           Pos.Core                  (Address, Coin, EpochIndex, StakeholderId,
                                            addressHash, applyCoinPortionUp, coinF,
                                            makePubKeyAddress, mkCoin, unsafeAddCoin,
                                            unsafeGetCoin, unsafeIntegerToCoin,
                                            unsafeMulCoin, unsafeSubCoin)
import           Pos.Crypto                (SecretKey, toPublic)
import           Pos.Generator.Block       (AllSecrets (..), HasAllSecrets (asSecretKeys),
                                            mkInvSecretsMap)
import           Pos.Genesis               (StakeDistribution (..),
                                            genesisContextImplicit)
import qualified Pos.GState                as GS
import qualified Pos.Lrc                   as Lrc
import           Pos.Txp                   (TxAux, mkTxPayload)
import           Pos.Util.Arbitrary        (nonrepeating)
import           Pos.Util.Util             (getKeys)

import           Test.Pos.Block.Logic.Mode (BlockProperty, TestParams (..),
                                            blockPropertyToProperty)
import           Test.Pos.Block.Logic.Util (EnableTxPayload (..), InplaceDB (..),
                                            bpGenBlock, bpGenBlocks)
import           Test.Pos.Util             (maybeStopProperty, stopProperty)

spec :: Spec
-- Currently we want to run it only once, because there is no much
-- randomization (its effect is likely negligible) and performance is
-- the issue.
spec = describe "Lrc.Worker" $ modifyMaxSuccess (const 1) $ do
    describe "lrcSingleShotNoLock" $ do
        prop lrcCorrectnessDesc $
            blockPropertyToProperty genTestParams lrcCorrectnessProp
  where
    lrcCorrectnessDesc =
        "Computes richmen correctly according to the stake distribution " <>
        "right before the '8 * k'-th slot.\n" <>
        "Computes leaders using follow-the-satoshi algorithm using stake " <>
        "distribution or utxo right before the '8 * k'-th slot."

-- We split everything into groups corresponding to different richmen
-- components.
type GroupId = Int

-- It's copy-pasted here because we rely on the order.
allRichmenComponents :: [Lrc.SomeRichmenComponent]
allRichmenComponents =
    [ Lrc.someRichmenComponent @Lrc.RCSsc
    , Lrc.someRichmenComponent @Lrc.RCUs
    , Lrc.someRichmenComponent @Lrc.RCDlg
    ]

-- | We need to generate some genesis with
-- genesis stakeholders `RC × {A, B, C, D}` (where `RC` is the set of
-- all richmen components and `{A, B, C, D}` is just a set of 4 items)
-- and make sure that there are `|RC| · 3` richmen (`RC × {A, B, C}`).
genTestParams :: Gen TestParams
genTestParams = do
    let _tpStartTime = 0
    let stakeholdersNum = 4 * groupsNumber
    secretKeys <- nonrepeating stakeholdersNum
    let invSecretsMap = mkInvSecretsMap secretKeys
    let _tpAllSecrets = AllSecrets invSecretsMap
    r <- choose (1000::Word64, 10000)
    -- Total stake inside one group.
    let totalStakeGroup = (`unsafeMulCoin` baseN) $ mkCoin r

    -- It's essential to use 'toList invSecretsMap' instead of
    -- 'secretKeys' here, because we rely on the order further. Later
    -- we can add ability to extend 'TestParams' or context.
    addressesAndDistrs <-
        mapM (genAddressesAndDistrs r totalStakeGroup (toList invSecretsMap))
             (enumerate allRichmenComponents)
    let _tpStakeDistributions = snd <$> addressesAndDistrs
    let _tpGenesisContext = genesisContextImplicit addressesAndDistrs
    return TestParams {..}
  where
    baseN :: Integral i => i
    baseN = 1000000
    groupsNumber = length allRichmenComponents
    genAddressesAndDistrs ::
           Word64
        -> Coin
        -> [SecretKey]
        -> (GroupId, Lrc.SomeRichmenComponent)
        -> Gen ([Address], StakeDistribution)
    genAddressesAndDistrs r totalStakeGroup allSecretKeys (i, Lrc.SomeRichmenComponent proxy) = do
        let secretKeysRange = subList (4 * i, 4 * (i + 1)) allSecretKeys
        let skToAddr = makePubKeyAddress . toPublic
        let addresses = map skToAddr secretKeysRange
        let totalStake = totalStakeGroup `unsafeMulCoin` groupsNumber
        let thresholdCoin =
                Lrc.rcInitialThreshold proxy `applyCoinPortionUp` totalStake
        let thresholdN = unsafeGetCoin thresholdCoin
            -- m1
        let poorStakeN = thresholdN `div` baseN
            -- P1
        let poorStake = mkCoin baseN `unsafeMulCoin` poorStakeN
        let k1 = (r - poorStakeN) `div` 3
        [richStake1,richStake2] <-
            replicateM 2 $
            (`unsafeMulCoin` (baseN :: Int)) . mkCoin <$>
            choose (poorStakeN, k1)
        let richStake3 =
                totalStakeGroup `unsafeSubCoin`
                (foldr1 unsafeAddCoin [poorStake,richStake1,richStake2])
        let stakes = [poorStake, richStake1, richStake2, richStake3]
        case richStake3 >= thresholdCoin of
            True  -> return (addresses, CustomStakes stakes)
            False -> error "threshold is too big, tests are not ready for it"

lrcCorrectnessProp :: BlockProperty ()
lrcCorrectnessProp = do
    let k = Const.blkSecurityParam
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
    stableUtxo <- lift GS.getAllPotentiallyHugeUtxo
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
    checkRichmen

checkRichmen :: BlockProperty ()
-- Here we check richmen.  The order must be the same as the one
-- in 'allRichmenComponents'. Unfortunately, I don't know how to
-- do it better without spending too much time on it (@gromak).
checkRichmen = do
    checkRichmenStakes 0 =<< getRichmen (lift . Lrc.getRichmenSsc)
    checkRichmenFull 1 =<< getRichmen (lift . Lrc.getRichmenUS)
    checkRichmenSet 2 =<< getRichmen (lift . Lrc.getRichmenDlg)
  where
    relevantStakeholders :: GroupId -> BlockProperty [StakeholderId]
    -- The order is important, so we don't use `keys`.
    relevantStakeholders i =
        map (addressHash . toPublic) . subList (4 * i, 4 * (i + 1)) . toList <$>
        view asSecretKeys
    getRichmen ::
           (EpochIndex -> BlockProperty (Maybe richmen))
        -> BlockProperty richmen
    getRichmen getter = maybeStopProperty "No richmen for epoch#1!" =<< getter 1
    checkRichmenFull :: GroupId -> Lrc.FullRichmenData -> BlockProperty ()
    checkRichmenFull i (totalStake, richmenStakes) = do
        realTotalStake <- lift GS.getRealTotalStake
        unless (totalStake == realTotalStake) $
            stopProperty $ sformat
            ("Total stake returned by LRC differs from the real one: (LRC = "
             %coinF% ", real = " %coinF%")")
             totalStake realTotalStake
        checkRichmenStakes i richmenStakes
    checkRichmenStakes :: GroupId -> Lrc.RichmenStakes -> BlockProperty ()
    checkRichmenStakes i richmenStakes = do
        checkRichmenSet i (getKeys richmenStakes)
        let checkRich (id, realStake)
                | Just lrcStake <- richmenStakes ^. at id
                , lrcStake /= realStake =
                    stopProperty $ sformat
                    ("Richman's stake differs from the real one (LRC returned "
                     %coinF%", the real one is "%coinF%")")
                     lrcStake realStake
                | otherwise = pass
        mapM_ checkRich =<< expectedRichmenStakes i
    checkRichmenSet :: GroupId -> Lrc.RichmenSet -> BlockProperty ()
    checkRichmenSet i richmenSet = do
        checkPoor i richmenSet
        let checkRich (id, realStake) =
                when (isNothing (richmenSet ^. at id)) $
                stopProperty $ sformat
                ("Someone has stake "%coinF%", but wasn't considered richman")
                 realStake
        mapM_ checkRich =<< expectedRichmenStakes i
    expectedRichmenStakes :: GroupId -> BlockProperty [(StakeholderId, Coin)]
    expectedRichmenStakes i = do
        richmen <- unsafeTail <$> relevantStakeholders i
        let resolve id = (id, ) . fromMaybe minBound <$> GS.getRealStake id
        lift $ mapM resolve richmen
    checkPoor ::
           (Index m ~ StakeholderId, At m) => GroupId -> m -> BlockProperty ()
    checkPoor i richmen = do
        let __unit = () -- workaround for hindent
        -- It's safe because there must be 4 stakeholders by construction.
        poorGuy <- unsafeHead <$> relevantStakeholders i
        poorGuyStake <- lift $ fromMaybe minBound <$> GS.getRealStake poorGuy
        unless (isNothing $ richmen ^. at poorGuy) $ do
            totalStake <- lift GS.getRealTotalStake
            stopProperty $ sformat
                ("Poor guy was considered rich by LRC! His real stake is "
                 %coinF%", real total stake is "%coinF)
                poorGuyStake totalStake

genAndApplyBlockFixedTxs :: [TxAux] -> BlockProperty ()
genAndApplyBlockFixedTxs txs =
    case mkTxPayload txs of
        Left err -> stopProperty err
        Right txPayload -> do
            emptyBlund <- bpGenBlock (EnableTxPayload False) (InplaceDB False)
            let blund =
                    emptyBlund & _1 . _Right . mainBlockTxPayload .~ txPayload
            lift $ applyBlocksUnsafe (one blund) Nothing

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
