module Test.Pos.Wallet.Web.Pending.SubmissionSpec
       ( spec
       ) where

import           Universum

import           Control.Exception (throw)

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

import           Pos.Wallet.Web.Pending.Submission (TxSubmissionResult (..), saveTxWithHandlers,
                                                    submitAndSavePtxMocked)
import           Pos.Wallet.Web.Pending.Types (PendingTx (..), PtxCondition (..), PtxPoolInfo)
import           Test.Pos.Util (assertProperty, withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)


-- stack test cardano-sl-wallet --fast --test-arguments "-m Wallet.Web.Pending.SubmissionSpec"
spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
    describe "Wallet.Web.Pending.SubmissionSpec" $ modifyMaxSuccess (const 100) $ do
        describe "submitAndSaveTx" $ do
            describe "Tx minor error handling" saveTxWithHandlersSpec

            describe "Normal application" normalApplicationSpec
            describe "Tx timeout after an hour " txTimeoutSpec
            describe "ToilTooLarge exception occurs " toilTooLargeSpec
            describe "ToilKnown exception occurs " toilKnownLargeSpec


saveTxWithHandlersSpec :: (HasCompileInfo, HasConfigurations) => Spec
saveTxWithHandlersSpec = walletPropertySpec normalApplicationDesc $ do
    result     <- lift $ saveTxWithHandlers (throw ToilKnown)

    assertProperty (testState result) $
      ("Tx should be in state TxMinorError, in state " <> show result)
  where
    normalApplicationDesc = "Tx handling: `TxMinorError`"

    testState (TxMinorError _ _) = True
    testState _                  = False


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
    -- _ <- stopProperty $ "Should be valid: " <> show result

    assertProperty (testState result) $
        "Tx should be in state TxApplying"
  where
    normalApplicationDesc = "Normal application: `PtxApplying -> PtxApplying`"

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
        "Tx should be in state TxApplying"
  where
    normalApplicationDesc = "Tx timeout application: `PtxApplying -> PtxWontApply`"

    testState (TxTimeoutWhenApplying _ _) = True
    testState _                           = False

-- Exceptions that can occur should be in TxMinorError --

toilTooLargeSpec :: (HasCompileInfo, HasConfigurations) => Spec
toilTooLargeSpec = walletPropertySpec normalApplicationDesc $ do
    let timestamp = Timestamp $ 1518968949 * 1000000 -- In microseconds
    tx         <- liftIO $ creatingTx $ Just timestamp
    result     <- lift $ submitAndSavePtxMocked
                      tx
                      timestamp
                      (\_ -> throwM $ ToilTooLargeTx 100 100)

    assertProperty (testState result) $
        "Tx should be in state TxApplying"
  where
    normalApplicationDesc = "Toil too large application: `PtxPersisted -> PtxApplying`"

    testState TxApplying = True
    testState _          = False


toilKnownLargeSpec :: (HasCompileInfo, HasConfigurations) => Spec
toilKnownLargeSpec = walletPropertySpec normalApplicationDesc $ do
    let timestamp = Timestamp $ 1518968949 * 1000000 -- In microseconds
    tx         <- liftIO $ creatingTx $ Just timestamp
    result     <- lift $ submitAndSavePtxMocked
                      tx
                      timestamp
                      (\_ -> throwM ToilKnown)

    assertProperty (testState result) $
        "Tx should be in state TxApplying"
  where
    normalApplicationDesc = "Toil known application: `PtxPersisted -> PtxApplying`"

    testState TxApplying = True
    testState _          = False


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


