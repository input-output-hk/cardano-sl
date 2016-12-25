{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Test.NodeSpec
       ( spec

       -- * Just to remove "unsued" warning
       , parcelNo
       , expected
       ) where

import           Control.Concurrent.STM      (STM, atomically, check)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import           Control.Exception           (Exception, SomeException (..))
import           Control.Lens                (makeLenses, sans, (%=), (-=), (.=))
import           Control.Monad               (forM_, void)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.State         (StateT)
import           Data.Binary
import qualified Data.List                   as L
import qualified Data.Set                    as S
import           GHC.Generics                (Generic)
import           Mockable.Concurrent         (delay, fork)
import           Mockable.Exception          (catch, throw)
import           Mockable.Production         (Production (..))
import           Network.Transport.Abstract  (closeTransport, newEndPoint)
import           Network.Transport.Concrete  (concrete)
import qualified Network.Transport.TCP       as TCP
import           Node
import           Serokell.Util.Concurrent    (modifyTVarS)
import           System.Random
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Property, ioProperty)
import           Test.QuickCheck.Arbitrary   (Arbitrary (..))
import           Test.QuickCheck.Modifiers   (getLarge)
import           Test.QuickCheck.Property    (Testable (..), failed, reason,
                                              succeeded)

-- Spec can be found at bottom of module.
-- It's there to allow TH usage

data Parcel = Parcel
    { parcelNo :: Int
    } deriving (Eq, Ord, Show, Generic)

instance Binary Parcel

instance Arbitrary Parcel where
    arbitrary = Parcel . getLarge <$> arbitrary

data TestState = TestState
    { _fails         :: [String]
    , _expected      :: S.Set Parcel
    , _activeWorkers :: Int
    }

mkTestState :: TestState
mkTestState = TestState
    { _fails         = []
    , _expected      = S.empty
    , _activeWorkers = 0
    }

makeLenses ''TestState

instance Testable TestState where
    property TestState{..}
        | not $ null _fails      = property failed
            { reason = "Fails: \n" ++ L.intercalate "\n" (("\t" ++) <$> _fails) }

        | not $ S.null _expected = property failed
            { reason = "Missed messages: " ++ show _expected }

        | otherwise              = property succeeded

type Header = ()

modifyTestState :: TVar TestState -> StateT TestState STM () -> Production ()
modifyTestState ts how = liftIO . atomically $ modifyTVarS ts how

newWork :: TVar TestState -> String -> Production () -> Production ()
newWork testState workerName act = do
    act `catch` \(SomeException e) ->
        modifyTestState testState $ fails %=
             (("Error thrown in " ++ workerName ++ ": " ++ show e) :)
    modifyTestState testState $ activeWorkers -= 1

worker :: TVar TestState -> NodeId -> [Parcel]
           -> SendActions Header Production -> Production ()
worker testState peerId parcels sendActions = do
    newWork testState "client" $
        forM_ parcels $ sendTo sendActions peerId "ping" ()
        -- TODO: test `withConnectionTo` also

listener :: TVar TestState -> Listener Header Production
listener testState = Listener "ping" $
    ListenerActionOneMsg $ \_ _ parcel ->
        modifyTestState testState $ expected %= sans parcel

-- I guess, errors in network-transport wasn't supposed to be processed in such way
throwLeft :: Exception e => Production (Either e a) -> Production a
throwLeft = (>>= f)
  where
    f (Left e)  = throw e
    f (Right a) = return a

-- | Await for predicate to become True, with timeout
awaitSTM :: Int -> STM Bool -> Production ()
awaitSTM time predicate = do
    tvar <- liftIO $ newTVarIO False
    void . fork $ do
        delay time
        liftIO . atomically $ writeTVar tvar True
    liftIO . atomically $
        check =<< (||) <$> predicate <*> readTVar tvar

launchHeadersTest :: TVar TestState -> [Parcel] -> IO Property
launchHeadersTest testState parcels = runProduction $ do
    transport_ <- throwLeft $ liftIO $ TCP.createTransport "127.0.0.1" "10342" TCP.defaultTCPParameters
    let transport = concrete transport_
    endpoint1 <- throwLeft $ newEndPoint transport
    endpoint2 <- throwLeft $ newEndPoint transport

    let prng1 = mkStdGen 0
    let prng2 = mkStdGen 1

    -- wait for sender or receiver to complete
    -- TODO: make receiver stop automatically when sender finishes
    modifyTestState testState $ do
        activeWorkers .= 1
        expected .= S.fromList parcels

    -- launch nodes
    rec { cliNode  <- startNode endpoint1 prng1
            [worker testState servNodeId parcels] Nothing []
        ; servNode <- startNode endpoint2 prng2
            [] Nothing [listener testState]
        ; let servNodeId = nodeId servNode
        }

    -- wait for all processes to stop
    liftIO . atomically $
        check . (== 0) . _activeWorkers =<< readTVar testState

    -- wait for receiver to get everything, but not for too long
    awaitSTM 50000 $ S.null . _expected <$> readTVar testState

    -- stop nodes
    mapM_ stopNode [cliNode, servNode]
    closeTransport transport

    -- wait till port gets free
    delay 1000

    -- form test results
    liftIO . atomically $
        property <$> readTVar testState

checkDelivery :: [Parcel] -> Property
checkDelivery parcels = ioProperty $ do
    testState <- newTVarIO mkTestState
    launchHeadersTest testState parcels

spec :: Spec
spec = describe "Node" $ do
    prop "client & server - delivery" $
        checkDelivery
