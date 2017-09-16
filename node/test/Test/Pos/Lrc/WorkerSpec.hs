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
import           Serokell.Util             (enumerate, subList)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (modifyMaxSuccess, prop)
import           Test.QuickCheck           (Gen, arbitrary, choose)
import           Test.QuickCheck.Monadic   (pick)

import           Pos.AllSecrets            (HasAllSecrets (..), mkAllSecretsSimple)
import           Pos.Block.Core            (mainBlockTxPayload)
import           Pos.Block.Logic           (applyBlocksUnsafe)
import           Pos.Core                  (AddrDistribution, Coin, EpochIndex,
                                            GenesisWStakeholders (..),
                                            IsBootstrapEraAddr (..), StakeholderId,
                                            addressHash, applyCoinPortionUp,
                                            blkSecurityParam, coinF, divCoin,
                                            makePubKeyAddress, mkCoin, unsafeAddCoin,
                                            unsafeMulCoin, unsafeSubCoin)
import           Pos.Crypto                (SecretKey, toPublic, unsafeHash)
import           Pos.Genesis               (BalanceDistribution (..), GenesisContext (..),
                                            GenesisUtxo (..), concatAddrDistrs,
                                            noGenesisDelegation)
import qualified Pos.GState                as GS
import qualified Pos.Lrc                   as Lrc
import           Pos.Txp                   (TxAux, TxIn (..), TxOut (..), TxOutAux (..),
                                            mkTxPayload)
import           Pos.Util.Arbitrary        (nonrepeating)
import           Pos.Util.Util             (getKeys, lensOf)

import           Test.Pos.Block.Logic.Mode (BlockProperty, HasVarSpecConfigurations,
                                            TestParams (..), blockPropertyToProperty)
import           Test.Pos.Block.Logic.Util (EnableTxPayload (..), InplaceDB (..),
                                            bpGenBlock, bpGenBlocks)
import           Test.Pos.Util             (giveGtConf, giveInfraConf, giveNodeConf,
                                            giveCoreConf, giveUpdateConf,
                                            maybeStopProperty, stopProperty)

spec :: Spec
-- Currently we want to run it only 4 times, because there is no
-- much randomization (its effect is likely negligible) and
-- performance matters (but not very much, so we can run more than once).
spec = giveGtConf $ giveNodeConf $ giveInfraConf $ giveUpdateConf $ giveCoreConf $
    describe "Lrc.Worker" $ modifyMaxSuccess (const 4) $ do
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
allRichmenComponents :: HasVarSpecConfigurations => [Lrc.SomeRichmenComponent]
allRichmenComponents =
    [ Lrc.someRichmenComponent @Lrc.RCSsc
    , Lrc.someRichmenComponent @Lrc.RCUs
    , Lrc.someRichmenComponent @Lrc.RCDlg
    ]

-- | We need to generate some genesis with
-- genesis stakeholders `RC × {A, B, C, D}` (where `RC` is the set of
-- all richmen components and `{A, B, C, D}` is just a set of 4 items)
-- and make sure that there are `|RC| · 3` richmen (`RC × {B, C, D}`).
genTestParams :: HasVarSpecConfigurations => Gen TestParams
genTestParams = do
    let _tpStartTime = 0
    let stakeholdersNum = 4 * groupsNumber
    secretKeys <- nonrepeating stakeholdersNum
    let _tpAllSecrets = mkAllSecretsSimple secretKeys
    let invSecretsMap = _tpAllSecrets ^. asSecretKeys
    let minTotalStake = mkCoin 100000
    -- Total stake inside one group.
    totalStakeGroup <-
        (`divCoin` groupsNumber) . max minTotalStake <$> arbitrary

    -- It's essential to use 'toList invSecretsMap' instead of
    -- 'secretKeys' here, because we rely on the order further. Later
    -- we can add ability to extend 'TestParams' or context.
    addressesAndDistrs <-
        mapM (genAddressesAndDistrs totalStakeGroup (toList invSecretsMap))
             (enumerate allRichmenComponents)
    let _tpBalanceDistributions = snd <$> addressesAndDistrs
    let _tpGenesisContext = genesisContextSimple addressesAndDistrs
    return TestParams {..}
  where
    genesisContextSimple :: [AddrDistribution] -> GenesisContext
    genesisContextSimple addrDistr = do
        let balances = concatAddrDistrs addrDistr
        let utxoEntry (addr, coin) =
                ( TxInUtxo (unsafeHash addr) 0
                , TxOutAux (TxOut addr coin)
                )
        -- We don't care about genesis stakeholders, because in this
        -- test we don't make any txs.  If we add txs, we will need to
        -- do it after bootstrap era, so these stakeholders will be
        -- ignored.
        -- We also don't care about genesis delegation for now.
        GenesisContext (GenesisUtxo $ M.fromList $ map utxoEntry balances)
                       (GenesisWStakeholders mempty)
                       noGenesisDelegation

    groupsNumber = length allRichmenComponents
    genAddressesAndDistrs
        :: HasVarSpecConfigurations
        => Coin
        -> [SecretKey]
        -> (GroupId, Lrc.SomeRichmenComponent)
        -> Gen AddrDistribution
    genAddressesAndDistrs totalStakeGroup allSecretKeys (i, Lrc.SomeRichmenComponent proxy) = do
        let secretKeysRange = subList (4 * i, 4 * (i + 1)) allSecretKeys
        -- We set single key distribution (not bootstrap era) despite
        -- generating genesis utxo (which conceptually should contain
        -- bootstrap era addresses only). That's fine in these
        -- particular tests, because we just want to assign concrete
        -- stakes to nodes.
        let skToAddr = makePubKeyAddress (IsBootstrapEraAddr False) . toPublic
        let addresses = map skToAddr secretKeysRange
        let totalStake = totalStakeGroup `unsafeMulCoin` groupsNumber
        let thresholdCoin =
                Lrc.rcInitialThreshold proxy `applyCoinPortionUp` totalStake
        -- Poor guy gets one coin less than threshold.
        let poorStake = thresholdCoin `unsafeSubCoin` mkCoin 1
        -- Let's add small stake to two richmen just for fun.
        let genSmallRichStake =
                unsafeAddCoin thresholdCoin . mkCoin <$> choose (0, 10)
        richStake1 <- genSmallRichStake
        richStake2 <- genSmallRichStake
        let richStake3 =
                foldl'
                    unsafeSubCoin
                    totalStakeGroup
                    [poorStake, richStake1, richStake2]
        let stakes = [poorStake, richStake1, richStake2, richStake3]
        case richStake3 >= thresholdCoin of
            True  -> return (addresses, CustomBalances stakes)
            False -> error "threshold is too big, tests are not ready for it"

lrcCorrectnessProp :: HasVarSpecConfigurations => BlockProperty ()
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
    lift $ Lrc.lrcSingleShot 1
    leaders1 <-
        maybeStopProperty "No leaders for epoch#1!" =<< lift (Lrc.getLeaders 1)
    gws <- view (lensOf @GenesisWStakeholders)
    -- Here we use 'genesisSeed' (which is the seed for the 0-th
    -- epoch) because we have a contract that if there is no ssc
    -- payload the previous seed must be reused (which is the case in
    -- this test).
    let expectedLeadersUtxo =
            Lrc.followTheSatoshiUtxo gws Lrc.genesisSeed stableUtxo
    let expectedLeadersStakes =
            Lrc.followTheSatoshi Lrc.genesisSeed (HM.toList stableStakes)
    unless (expectedLeadersUtxo /= leaders1) $
        stopProperty "expectedLeadersUtxo /= leaders1"
    unless (expectedLeadersStakes /= leaders1) $
        stopProperty "expectedLeadersStakes /= leaders1"
    checkRichmen

checkRichmen :: HasVarSpecConfigurations => BlockProperty ()
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

    checkRichmenFull :: HasVarSpecConfigurations => GroupId -> Lrc.FullRichmenData -> BlockProperty ()
    checkRichmenFull i (totalStake, richmenStakes) = do
        realTotalStake <- lift GS.getRealTotalStake
        unless (totalStake == realTotalStake) $
            stopProperty $ sformat
            ("Total stake returned by LRC differs from the real one: (LRC = "
             %coinF% ", real = " %coinF%")")
             totalStake realTotalStake
        checkRichmenStakes i richmenStakes

    checkRichmenStakes :: HasVarSpecConfigurations => GroupId -> Lrc.RichmenStakes -> BlockProperty ()
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

    checkRichmenSet :: HasVarSpecConfigurations => GroupId -> Lrc.RichmenSet -> BlockProperty ()
    checkRichmenSet i richmenSet = do
        checkPoor i richmenSet
        let checkRich (id, realStake) =
                when (isNothing (richmenSet ^. at id)) $
                stopProperty $ sformat
                ("Someone has stake "%coinF%", but wasn't considered richman")
                 realStake
        mapM_ checkRich =<< expectedRichmenStakes i

    expectedRichmenStakes :: HasVarSpecConfigurations => GroupId -> BlockProperty [(StakeholderId, Coin)]
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

genAndApplyBlockFixedTxs :: HasVarSpecConfigurations => [TxAux] -> BlockProperty ()
genAndApplyBlockFixedTxs txs = do
    let txPayload = mkTxPayload txs
    emptyBlund <- bpGenBlock (EnableTxPayload False) (InplaceDB False)
    let blund = emptyBlund & _1 . _Right . mainBlockTxPayload .~ txPayload
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
