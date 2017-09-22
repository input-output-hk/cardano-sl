{-# LANGUAGE TypeFamilies #-}

-- | Specification of 'Pos.Lrc.Worker' (actually only
-- 'lrcSingleShotNoLock' which probably shouldn't be there, but it
-- doesn't matter now).

module Test.Pos.Lrc.WorkerSpec
       ( spec
       , checkRichmen  -- TODO: remove when this test is fixed. This is only
                       -- exported to prevent an “unused function” warning
       ) where

import           Universum
import           Unsafe                    (unsafeHead, unsafeTail)

import           Control.Lens              (At (at), Index, _Right)
import qualified Data.HashMap.Strict       as HM
import           Formatting                (build, sformat, (%))
import           Serokell.Util             (subList)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (modifyMaxSuccess, prop)
import           Test.QuickCheck           (choose)
import           Test.QuickCheck.Monadic   (pick)

import           Pos.AllSecrets            (HasAllSecrets (..))
import           Pos.Block.Core            (mainBlockTxPayload)
import           Pos.Block.Logic           (applyBlocksUnsafe)
import           Pos.Core                  (Coin, EpochIndex, GenesisData (..),
                                            StakeholderId, addressHash, blkSecurityParam,
                                            coinF, genesisData)
import           Pos.Crypto                (toPublic)
import qualified Pos.GState                as GS
import qualified Pos.Lrc                   as Lrc
import           Pos.Txp                   (TxAux, mkTxPayload)
import           Pos.Util.Util             (getKeys)

import           Test.Pos.Block.Logic.Mode (BlockProperty, HasVarSpecConfigurations)
import           Test.Pos.Block.Logic.Util (EnableTxPayload (..), InplaceDB (..),
                                            bpGenBlock, bpGenBlocks)
import           Test.Pos.Util             (giveCoreConf, giveGtConf, giveInfraConf,
                                            giveNodeConf, giveUpdateConf,
                                            maybeStopProperty, stopProperty)

spec :: Spec
-- Currently we want to run it only 4 times, because there is no
-- much randomization (its effect is likely negligible) and
-- performance matters (but not very much, so we can run more than once).
spec = giveGtConf $ giveNodeConf $ giveInfraConf $ giveUpdateConf $ giveCoreConf $
    describe "Lrc.Worker" $ modifyMaxSuccess (const 4) $ do
        -- [CSL-1669] We had a rather complicated test here but it got
        -- broken after the genesis refactoring. See
        -- https://github.com/input-output-hk/cardano-sl/blob/68d829ea3f225a02a831ae941aa210df399f4063/node/test/Test/Pos/Lrc/WorkerSpec.hs#L83-L154
        -- for the original test
        describe "lrcSingleShotNoLock (abridged; see comments!)" $ do
            prop lrcCorrectnessDesc lrcCorrectnessProp
  where
    lrcCorrectnessDesc =
        "Computes richmen correctly according to the stake distribution " <>
        "right before the '8 * k'-th slot.\n" <>
        "Computes leaders using follow-the-satoshi algorithm using stake " <>
        "distribution or utxo right before the '8 * k'-th slot."

-- We split everything into groups corresponding to different richmen
-- components.
type GroupId = Int

{-

-- It's copy-pasted here because we rely on the order.
allRichmenComponents :: HasVarSpecConfigurations => [Lrc.SomeRichmenComponent]
allRichmenComponents =
    [ Lrc.someRichmenComponent @Lrc.RCSsc
    , Lrc.someRichmenComponent @Lrc.RCUs
    , Lrc.someRichmenComponent @Lrc.RCDlg
    ]

-}

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
    let gws = gdBootStakeholders genesisData
    -- Here we use 'genesisSeed' (which is the seed for the 0-th
    -- epoch) because we have a contract that if there is no ssc
    -- payload the previous seed must be reused (which is the case in
    -- this test).
    let genesisSeed = gdFtsSeed genesisData
    let expectedLeadersUtxo =
            Lrc.followTheSatoshiUtxo gws genesisSeed stableUtxo
    let expectedLeadersStakes =
            Lrc.followTheSatoshi genesisSeed (HM.toList stableStakes)
    unless (expectedLeadersUtxo /= leaders1) $
        stopProperty "expectedLeadersUtxo /= leaders1"
    unless (expectedLeadersStakes /= leaders1) $
        stopProperty "expectedLeadersStakes /= leaders1"
    -- TODO: resurrect.

    -- checkRichmen

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
                (build%" has stake "%coinF%", but wasn't considered richman")
                 id realStake
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
