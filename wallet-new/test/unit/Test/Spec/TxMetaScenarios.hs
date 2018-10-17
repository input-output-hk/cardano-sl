module Test.Spec.TxMetaScenarios (
      txMetaScenarioA
    , txMetaScenarioB
    , txMetaScenarioC
    , txMetaScenarioD
    , txMetaScenarioE
    , txMetaScenarioF
    , txMetaScenarioG
    , txMetaScenarioH
    , txMetaScenarioI
    , txMetaScenarioJ

    , bracketActiveWalletTxMeta
    , TxScenarioRet
    ) where

import           Universum

import qualified Data.Set as Set
import           Data.Time.Units (fromMicroseconds)

import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor
                     (MockNodeStateParams (..), SecurityParameter (..),
                     mockNodeState)
import           Cardano.Wallet.WalletLayer.Kernel.Transactions

import           Pos.Chain.Genesis (Config (..))
import           Pos.Core
import           Pos.Core.Chrono
import           Pos.Core.Slotting (EpochIndex (..), LocalSlotIndex (..),
                     SlotId (..))
import           Pos.Infra.InjectFail (mkFInjects)
import           Pos.Util (withCompileInfo)

import           Test.Hspec
import           Test.Infrastructure.Genesis
import           Test.Pos.Configuration (withDefConfiguration,
                     withDefUpdateConfiguration)
import           UTxO.Context
import           UTxO.DSL
import           Wallet.Inductive

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | A Payment from P0 to P1 with change returned to P0
paymentWithChangeFromP0ToP1 :: forall h. Hash h Addr
                            => GenesisValues h Addr -> Transaction h Addr
paymentWithChangeFromP0ToP1 GenesisValues{..} = Transaction {
         trFresh = 0
       , trIns   = Set.fromList [ fst initUtxoP0 ]
       , trOuts  = [ Output p1 1000
                   , Output p0 (initBalP0 - 1 * (1000 + fee)) -- change
                   ]
       , trFee   = fee
       , trHash  = 1
       , trExtra = []
       }
  where
    fee = overestimate txFee 1 2

-- | A payment from P1 to P0 with change returned to P1.
paymentWithChangeFromP1ToP0 :: forall h. Hash h Addr
                            => GenesisValues h Addr -> Transaction h Addr
paymentWithChangeFromP1ToP0 GenesisValues{..} = Transaction {
         trFresh = 0
       , trIns   = Set.fromList [ fst initUtxoP1 ]
       , trOuts  = [ Output p0 1000
                   , Output p1 (initBalP1 - 1 * (1000 + fee)) -- change
                   ]
       , trFee   = fee
       , trHash  = 1
       , trExtra = []
       }
  where
    fee = overestimate txFee 1 2

-- | A payment from P0 to himself.
paymentWithChangeFromP0ToP0 :: forall h. Hash h Addr
                            => GenesisValues h Addr -> Transaction h Addr
paymentWithChangeFromP0ToP0 GenesisValues{..} = Transaction {
         trFresh = 0
       , trIns   = Set.fromList [ fst initUtxoP0 ]
       , trOuts  = [ Output p0 1000
                   , Output p0 (initBalP1 - 1 * (1000 + fee))
                   ]
       , trFee   = fee
       , trHash  = 1
       , trExtra = []
       }
  where
    fee = overestimate txFee 1 2


-- | A payment from P0 to himself.
bigPaymentWithChange :: forall h. Hash h Addr
                     => GenesisValues h Addr -> Transaction h Addr
bigPaymentWithChange GenesisValues{..} = Transaction {
         trFresh = 0
       , trIns   = Set.fromList [ fst initUtxoP0 ]
       , trOuts  = [ Output p1 1000
                   , Output r0 2000
                   , Output r1 3000
                   , Output p0 (initBalP0 - 1 * (6000 + fee))
                   ]
       , trFee   = fee
       , trHash  = 1
       , trExtra = []
       }
  where
    fee = overestimate txFee 1 4


-- | Two payments from P0 to P1 with change returned to P0.
--   (t0 uses change address p0 and t1 uses p0b)
--   The second payment spends the change of the first payment.
repeatPaymentWithChangeFromP0ToP1 :: forall h. Hash h Addr
                                  => GenesisValues h Addr
                                  -> Addr
                                  -> (Transaction h Addr, Transaction h Addr)
repeatPaymentWithChangeFromP0ToP1 genVals@GenesisValues{..} changeAddr =
    (t0,t1)
  where
    fee = overestimate txFee 1 2

    t0 = paymentWithChangeFromP0ToP1 genVals
    t1 = Transaction {
            trFresh = 0
          , trIns   = Set.fromList [ Input (hash t0) 1 ]
          , trOuts  = [ Output p1 1000
                      , Output changeAddr (initBalP0 - 2 * (1000 + fee)) -- change
                      ]
          , trFee   = fee
          , trHash  = 2
          , trExtra = []
          }

type TxScenarioRet h = (MockNodeStateParams, Inductive h Addr, PassiveWallet -> IO ())

-- | Scenario A
-- Empty case
txMetaScenarioA :: GenesisValues h Addr
                   -> TxScenarioRet h
txMetaScenarioA GenesisValues{..} = (nodeStParams1, ind, lengthCheck 0)
  where
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
            ]
        }

-- | Scenario B
-- A single pending payment.
txMetaScenarioB :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> TxScenarioRet h
txMetaScenarioB genVals@GenesisValues{..} = (nodeStParams1, ind, check)
  where
    t0 = paymentWithChangeFromP0ToP1 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0
        , inductiveEvents = OldestFirst [
                NewPending t0
            ]
        }

    check = checkWithTxs $ \ txs -> do
      let fees = overestimate txFee 1 2
      let props = map (\t -> (V1.txConfirmations t, V1.txAmount t, V1.txType t,V1.txDirection t, V1.txStatus t)) txs
      props `shouldMatchList` [(0,V1.V1 . Coin $ fees + 1000,V1.ForeignTransaction,V1.OutgoingTransaction,V1.Applying)]

-- | Scenario C
-- A single pending payment and then confirmation.
txMetaScenarioC :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> TxScenarioRet h
txMetaScenarioC genVals@GenesisValues{..} = (nodeStParams1, ind, check)
  where
    t0 = paymentWithChangeFromP0ToP1 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0]
            ]
        }

    check = checkWithTxs $ \ txs -> do
      let fees = overestimate txFee 1 2
      let props = map (\t -> (V1.txConfirmations t, V1.txAmount t, V1.txType t,V1.txDirection t, V1.txStatus t)) txs
      props `shouldMatchList` [(3,V1.V1 . Coin $ fees + 1000,V1.ForeignTransaction,V1.OutgoingTransaction,V1.InNewestBlocks)]

-- | Scenario D
-- Two confirmed payments from P0 to P1, using `change` addresses P0 and P0b respectively
txMetaScenarioD :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> TxScenarioRet h
txMetaScenarioD genVals@GenesisValues{..} = (nodeStParams1, ind, check)
  where
    (t0,t1) = repeatPaymentWithChangeFromP0ToP1 genVals p0b
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.fromList [p0,p0b]
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0]
            , ApplyBlock $ OldestFirst [t1]
            ]
        }

    check = checkWithTxs $ \ txs -> do
--      txs `shouldBe` []
      let fees = overestimate txFee 1 2
      let props = map (\t -> (V1.txConfirmations t, V1.txAmount t, V1.txType t,V1.txDirection t, V1.txStatus t)) txs
      props `shouldMatchList` [(3,V1.V1 . Coin $ fees + 1000,V1.ForeignTransaction,V1.OutgoingTransaction,V1.InNewestBlocks)
                              ,(2,V1.V1 . Coin $ fees + 1000,V1.ForeignTransaction,V1.OutgoingTransaction,V1.InNewestBlocks)
                              ]

-- | Scenario E
-- ScenarioD + Rollback
--
-- This scenario exercises Rollback behaviour.
txMetaScenarioE :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> TxScenarioRet h
txMetaScenarioE genVals@GenesisValues{..} = (nodeStParams1, ind, check)
  where
    (t0,t1) = repeatPaymentWithChangeFromP0ToP1 genVals p0b
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.fromList [p0,p0b] -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            , ApplyBlock $ OldestFirst [t1] -- confirms t1 and updates block metadata
            , Rollback                      -- rolls back t1, so it should be V1.WontApply
            , ApplyBlock $ OldestFirst []
            , ApplyBlock $ OldestFirst []
            ]
        }

    check = checkWithTxs $ \ txs -> do
            let fees = overestimate txFee 1 2
            let props = map (\t -> (V1.txConfirmations t, V1.txAmount t, V1.txType t,V1.txDirection t, V1.txStatus t)) txs
            props `shouldMatchList` [(3,V1.V1 . Coin $ fees + 1000,V1.ForeignTransaction,V1.OutgoingTransaction,V1.InNewestBlocks)
                                    ,(0,V1.V1 . Coin $ fees + 1000,V1.ForeignTransaction,V1.OutgoingTransaction,V1.WontApply)
                                    ]

-- | Scenario F
-- A payment from P1 to P0's single address.
-- This should create IncomingTransactions.
txMetaScenarioF :: forall h. Hash h Addr
                  => GenesisValues h Addr
                  -> TxScenarioRet h
txMetaScenarioF genVals@GenesisValues{..} = (nodeStParams1, ind, check)
  where
    t0 = paymentWithChangeFromP1ToP0 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
            ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            ]
        }

    check = checkWithTxs $ \ txs -> do
      let props = map (\t -> (V1.txConfirmations t, V1.txAmount t, V1.txType t,V1.txDirection t, V1.txStatus t)) txs
      props `shouldMatchList` [(3,V1.V1 . Coin $ 1000,V1.ForeignTransaction,V1.IncomingTransaction,V1.InNewestBlocks)]

-- | Scenario G
-- A single pending payment and then confirmation.
-- This tests differnet node parameters.
txMetaScenarioG :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> TxScenarioRet h
txMetaScenarioG genVals@GenesisValues{..} = (nodeStParams2, ind, check)
  where
    t0 = paymentWithChangeFromP0ToP1 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            ]
        }

    check = checkWithTxs $ \ txs -> do
      let fees = overestimate txFee 1 2
      let props = map (\t -> (V1.txAmount t, V1.txType t,V1.txDirection t, V1.txStatus t)) txs
      props `shouldMatchList` [(V1.V1 . Coin $ fees + 1000,V1.ForeignTransaction,V1.OutgoingTransaction,V1.Persisted)]

-- | Scenario H
-- A single pending payment to itself and then confirmation.
-- This should be a Local Tx.
txMetaScenarioH :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> TxScenarioRet h
txMetaScenarioH genVals@GenesisValues{..} = (nodeStParams1, ind, check)
  where
    t0 = paymentWithChangeFromP0ToP0 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            ]
        }

    check = checkWithTxs $ \ txs -> do
      let fees = overestimate txFee 1 2
      let props = map (\t -> (V1.txAmount t, V1.txType t,V1.txDirection t, V1.txStatus t)) txs
      props `shouldMatchList` [(V1.V1 . Coin $ fees,V1.LocalTransaction,V1.OutgoingTransaction,V1.InNewestBlocks)]

-- | Scenario I. This is like Scenario C with rollbacks.
-- results should not change.
txMetaScenarioI :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> TxScenarioRet h
txMetaScenarioI genVals@GenesisValues{..} = (nodeStParams1, ind, check)
  where
    t0 = paymentWithChangeFromP0ToP1 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            , Rollback
            , ApplyBlock $ OldestFirst [t0]
            , Rollback
            , ApplyBlock $ OldestFirst [t0]
            ]
        }

    check = checkWithTxs $ \ txs -> do
      let fees = overestimate txFee 1 2
      let props = map (\t -> (V1.txConfirmations t, V1.txAmount t, V1.txType t,V1.txDirection t, V1.txStatus t)) txs
      props `shouldMatchList` [(3,V1.V1 . Coin $ fees + 1000,V1.ForeignTransaction,V1.OutgoingTransaction,V1.InNewestBlocks)]

-- | Scenario J
-- A single payment with 4 outputs.
txMetaScenarioJ :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> TxScenarioRet h
txMetaScenarioJ genVals@GenesisValues{..} = (nodeStParams1, ind, check)
  where
    t0 = bigPaymentWithChange genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
                NewPending t0
            ]
        }

    check = checkWithTxs $ \ txs -> do
      let fees = overestimate txFee 1 4
      let props = map (\t -> (V1.txConfirmations t, V1.txAmount t, V1.txType t,V1.txDirection t, V1.txStatus t)) txs
      props `shouldMatchList` [(0,V1.V1 . Coin $ fees + 6000,V1.ForeignTransaction,V1.OutgoingTransaction,V1.Applying)]

lengthCheck :: Int -> PassiveWallet -> IO ()
lengthCheck n pw = do
    let db = pw ^. Kernel.walletMeta
    meta <- getAllTxMetas db
    length meta `shouldBe` n

checkWithTxs :: ([V1.Transaction] -> IO ()) -> PassiveWallet -> IO ()
checkWithTxs check pw = do
    let db = pw ^. Kernel.walletMeta
    metas <- getAllTxMetas db
    txs <- do
      mapOfEitherTx <- mapM (toTransaction pw) metas
      let eiTxs = sequence mapOfEitherTx
      return $ fromRight (error ("Account not found")) eiTxs
    check txs

nodeStParams1 :: MockNodeStateParams
nodeStParams1 =
  withDefConfiguration $ \_pm ->
    withDefUpdateConfiguration $
    withCompileInfo $
      MockNodeStateParams {
        mockNodeStateTipSlotId = SlotId (EpochIndex 0) (UnsafeLocalSlotIndex 4)
      , mockNodeStateSlotStart = const $ Right getSomeTimestamp
      , mockNodeStateSecurityParameter = SecurityParameter 2160
      , mockNodeStateNextEpochSlotDuration = fromMicroseconds 200
      , mockNodeStateNtpDrift = const (V1.TimeInfo Nothing)
      , mockNodeStateSyncProgress = (Just 100, 100)
      , mockNodeStateCreationTimestamp = getSomeTimestamp
      }

nodeStParams2 :: MockNodeStateParams
nodeStParams2 =
  withDefConfiguration $ \_pm ->
    withDefUpdateConfiguration $
    withCompileInfo $
      MockNodeStateParams {
        mockNodeStateTipSlotId = SlotId (EpochIndex 1) (UnsafeLocalSlotIndex 1)
      , mockNodeStateSlotStart = const $ Right getSomeTimestamp
      , mockNodeStateSecurityParameter = SecurityParameter 2160
      , mockNodeStateNextEpochSlotDuration = fromMicroseconds 200
      , mockNodeStateNtpDrift = const (V1.TimeInfo Nothing)
      , mockNodeStateSyncProgress = (Just 100, 100)
      , mockNodeStateCreationTimestamp = getSomeTimestamp
      }

-- | Initialize active wallet in a manner suitable for generator-based testing.
-- This is different from the one in Kernel, because it is parametrised over
-- the NodeStateParameters. This is important for Transactions, because
-- dynamic TxMeta depend on the state of the Node and we want to be flexible
-- there for better testing.
bracketActiveWalletTxMeta :: MockNodeStateParams -> (Kernel.ActiveWallet -> IO a) -> IO a
bracketActiveWalletTxMeta stateParams test =
    withDefConfiguration $ \genesisConfig -> do
        bracketPassiveWalletTxMeta stateParams $ \passive ->
            Kernel.bracketActiveWallet (configProtocolMagic genesisConfig)
                                       passive
                                       diffusion
                $ \active -> test active

-- | Initialize passive wallet in a manner suitable for the unit tests
bracketPassiveWalletTxMeta :: MockNodeStateParams -> (Kernel.PassiveWallet -> IO a) -> IO a
bracketPassiveWalletTxMeta stateParams postHook = do
      Keystore.bracketTestKeystore $ \keystore -> do
          mockFInjects <- mkFInjects mempty
          Kernel.bracketPassiveWallet
            Kernel.UseInMemory
            logMessage
            keystore
            (mockNodeState stateParams)
            mockFInjects
            postHook
  where
    logMessage _ _  = return ()


-- TODO: Decide what we want to do with submitted transactions
diffusion :: Kernel.WalletDiffusion
diffusion =  Kernel.WalletDiffusion {
      walletSendTx = \_tx -> return False
    , walletGetSubscriptionStatus = return mempty
  }

getSomeTimestamp :: Pos.Core.Timestamp
getSomeTimestamp = Pos.Core.Timestamp $ fromMicroseconds 12340000
