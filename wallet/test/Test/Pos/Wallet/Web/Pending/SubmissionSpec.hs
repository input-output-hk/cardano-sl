module Test.Pos.Wallet.Web.Pending.SubmissionSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Gen, arbitrary, generate)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary)

import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Core (Timestamp (..), addMicrosecondsToTimestamp)
import           Pos.Launcher (HasConfigurations)
import           Pos.Txp.Toil.Failure (ToilVerFailure (..))
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.ClientTypes (CHash (..), CId (..))

import           Pos.Wallet.Web.Pending.Submission (TxSubmissionResult (..), submitAndSavePtxMocked)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..), PtxPoolInfo)
import           Pos.Util.QuickCheck.Property (assertProperty)
import           Test.Pos.Configuration (withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)


-- stack test cardano-sl-wallet --fast --test-arguments "-m Wallet.Web.Pending.SubmissionSpec"
spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
    describe "Wallet.Web.Pending.SubmissionSpec" $ modifyMaxSuccess (const 100) $ do
        describe "submitAndSaveTx" $ do
            describe "Normal application" normalApplicationSpec
            describe "Tx timeout after an hour " txTimeoutSpec
            describe "ToilKnown exception occurs " toilKnownLargeSpec
            describe "ToilTooLarge exception occurs " toilTooLargeSpec

-- testTime :: UTCTime
-- testTime = parseTime True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" "2017-03-29T11:02:57+00:00 "

normalApplicationSpec :: (HasCompileInfo, HasConfigurations) => Spec
normalApplicationSpec = walletPropertySpec normalApplicationDesc $ do
    let timestamp = Timestamp $ 1518968949 * 1000000 -- In microseconds
    pendingTx  <- liftIO $ applyingTx $ Just timestamp
    result     <- lift $ submitAndSavePtxMocked
                      pendingTx
                      timestamp
                      (\_ -> pure TxApplying)

    assertProperty (testState result) $
        ("Tx should be in state TxStillApplying, in state " <> show result)
  where
    normalApplicationDesc = "Normal application: `PtxApplying -> TxStillApplying`"

    testState TxStillApplying = True
    testState _               = False

txTimeoutSpec :: (HasCompileInfo, HasConfigurations) => Spec
txTimeoutSpec = walletPropertySpec normalApplicationDesc $ do
    let timestampTx   = Timestamp $ 1518968949 * 1000000 -- In microseconds
    -- After an hour.
    let timestampNode = addMicrosecondsToTimestamp (3600000000 + 1) timestampTx
    pendingTx  <- liftIO $ applyingTx $ Just timestampTx
    result     <- lift $ submitAndSavePtxMocked
                      pendingTx
                      timestampNode
                      (\_ -> pure TxApplying)

    assertProperty (testState result) $
        ("Tx should be in state TxTimeoutWhenApplying, in state " <> show result)
  where
    normalApplicationDesc = "Tx timeout application: `PtxApplying -> TxTimeoutWhenApplying`"

    testState (TxTimeoutWhenApplying _ _) = True
    testState _                           = False


toilKnownLargeSpec :: (HasCompileInfo, HasConfigurations) => Spec
toilKnownLargeSpec = walletPropertySpec normalApplicationDesc $ do
    let timestamp = Timestamp $ 1518968949 * 1000000 -- In microseconds
    tx         <- liftIO $ creatingTx $ Just timestamp
    result     <- lift $ submitAndSavePtxMocked
                      tx
                      timestamp
                      (\_ -> throwM ToilKnown)

    assertProperty (testState result) $
        ("Tx should be in state TxMinorError, in state " <> show result)
  where
    normalApplicationDesc = "Toil known application: `PtxCreating -> TxMinorError`"

    testState (TxMinorError _ _) = True
    testState _                  = False


toilTooLargeSpec :: (HasCompileInfo, HasConfigurations) => Spec
toilTooLargeSpec = walletPropertySpec normalApplicationDesc $ do
    let timestamp = Timestamp $ 1518968949 * 1000000 -- In microseconds
    tx         <- liftIO $ creatingTx $ Just timestamp
    result     <- lift $ submitAndSavePtxMocked
                      tx
                      timestamp
                      (\_ -> throwM $ ToilTooLargeTx 100 100)

    assertProperty (testState result) $
        ("Tx should be in state TxNonReclaimableError, in state " <> show result)
  where
    normalApplicationDesc = "Toil too large application: `PtxCreating -> TxNonReclaimableError`"

    testState (TxNonReclaimableError _) = True
    testState _                         = False


applyingTx :: HasConfigurations => Maybe Timestamp -> IO PendingTx
applyingTx mTimestamp =
    generate $ genPendingTxWithCondition =<< genPtxCondition
  where
    genPtxCondition :: Gen PtxCondition
    genPtxCondition = PtxApplying <$> ptxPoolInfo

    ptxPoolInfo :: Gen PtxPoolInfo
    ptxPoolInfo = genPtxPollInfo mTimestamp


creatingTx :: HasConfigurations => Maybe Timestamp -> IO PendingTx
creatingTx mTimestamp =
    generate $ genPendingTxWithCondition =<< genPtxCondition
  where
    genPtxCondition :: Gen PtxCondition
    genPtxCondition = PtxCreating <$> ptxPoolInfo

    ptxPoolInfo :: Gen PtxPoolInfo
    ptxPoolInfo = genPtxPollInfo mTimestamp


-- persistedTx :: HasConfigurations => IO PendingTx
-- persistedTx =
--    generate $ genPendingTxWithCondition PtxPersisted


-- We don't care about most of the fields. We could also use
-- a full @Arbitrary@ instance and just replace the field, but you never
-- know what is the next field that should be included in the test.
genPtxPollInfo :: Maybe Timestamp -> Gen PtxPoolInfo
genPtxPollInfo _thTimestamp = do
    _thTxId         <- arbitrary
    _thTx           <- arbitrary
    _thDifficulty   <- arbitrary
    _thInputs       <- arbitrary
    _thOutputAddrs  <- arbitrary
    pure $ THEntry{..}

genPendingTxWithCondition :: HasConfigurations => PtxCondition -> Gen PendingTx
genPendingTxWithCondition _ptxCond = do
    _ptxTxId          <- arbitrary
    _ptxTxAux         <- genericArbitrary
    _ptxCreationSlot  <- arbitrary

    let _ptxWallet    =  CId $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"
    _ptxPeerAck       <- arbitrary
    _ptxSubmitTiming  <- genericArbitrary
    pure PendingTx {..}


