{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}

module Test.ConnStateSpec
       ( spec
       ) where

import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TVar (modifyTVar, newTVarIO, readTVarIO, writeTVar)
import Control.Monad               (forM, replicateM)
import Control.Monad.Trans         (liftIO)
import System.Random               (mkStdGen)
import Test.Hspec                  (Spec, describe)
import Test.Hspec.QuickCheck       (prop)
import Test.QuickCheck             (Property, ioProperty)
import Test.QuickCheck.Property    (once, property, succeeded, (.&&.), (===))

import           Message.Message            (BinaryP (..))
import           Mockable.Concurrent        (delay)
import           Mockable.Production        (runProduction)
import           Network.Transport.Abstract (closeTransport, newEndPoint)
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP      as TCP
import           Node                       (SendActions (..), nodeId, startNodeExt)
import           Test.Util                  (throwLeft)


spec :: Spec
spec = describe "Node" $ do
    describe "connection state" $ do
        prop "plain"
            connectionStateTest

connectionStateTest :: Property
connectionStateTest = once . ioProperty . runProduction $ do
    transport_ <- throwLeft $ liftIO $ TCP.createTransport "127.0.0.1" "10342" TCP.defaultTCPParameters
    let transport = concrete transport_

    endpoints <- replicateM 3 $ throwLeft $ newEndPoint transport
    let prngs = mkStdGen <$> [0..2]

    testState <- liftIO $ newTVarIO (property succeeded)

    mdo
        let workerss =
                [ [ changeStateWorker nodeIds
                  , checkWorker testState nodeIds
                  ]
                , []
                , []
                ]
            initState = liftIO $ newTVarIO (0 :: Int)
            nodeIds = map nodeId nodes

        nodes <- forM (zip3 endpoints prngs workerss) $
            \(endpoint, prng, workers) ->
                startNodeExt endpoint prng BinaryP initState Nothing workers []
        return ()

    closeTransport transport

    delay 50000

    liftIO $ readTVarIO testState
  where
    changeStateWorker [_, nodeId1, nodeId2] sendActions = do
        let setStateFor nid value = do
                state <- connStateTo sendActions nid
                liftIO . atomically $ writeTVar state value

        delay 10000
        setStateFor nodeId1 1
        setStateFor nodeId2 2
    changeStateWorker _ _ = error "stateWorker: wrong number of node ids"

    checkWorker testState [_, nodeId1, nodeId2] sendActions = do
        let check nid expected = do
                state <- connStateTo sendActions nid
                value <- liftIO $ readTVarIO state
                liftIO . atomically $ modifyTVar testState (.&&. value === expected)

        -- check state is initialized on access
        check nodeId1 0
        delay 20000

        -- check state was modified
        check nodeId1 1
        check nodeId2 2
    checkWorker _ _ _ = error "checkWorker: wrong number of node ids"
