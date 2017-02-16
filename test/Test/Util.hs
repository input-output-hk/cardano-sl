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
{-# LANGUAGE RankNTypes            #-}

module Test.Util
       ( makeTCPTransport
       , makeInMemoryTransport

       , timeout

       , Parcel (..)
       , Payload (..)
       , HeavyParcel (..)

       , TestState (..)
       , mkTestState
       , expected
       , fails
       , modifyTestState
       , addFail
       , newWork

       , throwLeft

       , TalkStyle (..)
       , sendAll
       , receiveAll

       , deliveryTest

       ) where

import           Control.Concurrent.STM      (STM, atomically, check)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import           Control.Exception           (Exception, SomeException (..))
import           Control.Lens                (makeLenses, (%=))
import           Control.Monad               (forM_, void)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.State         (StateT)
import           Data.Binary                 (Binary (..))
import qualified Data.ByteString             as LBS
import qualified Data.List                   as L
import qualified Data.Set                    as S
import           Data.Time.Units             (Microsecond, Millisecond, Second, TimeUnit)
import           GHC.Generics                (Generic)
import           Mockable.Class              (Mockable)
import           Mockable.Concurrent         (delay, forConcurrently, fork, cancel,
                                              async, withAsync, wait, Concurrently,
                                              Delay, Async)
import           Mockable.SharedExclusive    (newSharedExclusive, putSharedExclusive,
                                              takeSharedExclusive, SharedExclusive,
                                              readSharedExclusive, tryPutSharedExclusive)
import           Mockable.Exception          (Catch, Throw, catch, throw)
import           Mockable.Production         (Production (..))
import qualified Network.Transport           as NT (Transport)
import           Network.Transport.Abstract  (closeTransport, Transport)
import           Network.Transport.Concrete  (concrete)
import qualified Network.Transport.TCP       as TCP
import qualified Network.Transport.InMemory  as InMemory
import           Serokell.Util.Concurrent    (modifyTVarS)
import           System.Random               (mkStdGen)
import           Test.QuickCheck             (Property)
import           Test.QuickCheck.Arbitrary   (Arbitrary (..))
import           Test.QuickCheck.Gen         (choose)
import           Test.QuickCheck.Modifiers   (getLarge)
import           Test.QuickCheck.Property    (Testable (..), failed, reason, succeeded)

import           Node                        (ConversationActions (..), Listener,
                                              ListenerAction (..), Message (..),
                                              NodeAction (..), NodeId, SendActions (..),
                                              Worker, node, nodeId)
import           Node.Message                (BinaryP (..))

-- | Run a computation, but kill it if it takes more than a given number of
--   Microseconds to complete. If that happens, log using a given string
--   prefix.
timeout
    :: ( Mockable Delay m
       , Mockable Async m
       , Mockable SharedExclusive m
       , Mockable Catch m
       , Mockable Throw m
       )
    => String
    -> Microsecond
    -> m t
    -> m t
timeout str us m = do
    var <- newSharedExclusive
    let action = do
            t <- (fmap Right m) `catch` (\(e :: SomeException) -> return (Left e))
            putSharedExclusive var t
    let timeoutAction = do
            delay us
            putSharedExclusive var (Left . error $ str ++ " : timeout after " ++ show us)
    withAsync action $ \actionPromise -> do
        withAsync timeoutAction $ \timeoutPromise -> do
            choice <- readSharedExclusive var
            case choice of
                Left e -> throw e
                Right t -> return t

-- * Parcel

data Payload = Payload Int
    deriving (Eq, Ord, Show)

instance Binary Payload where
    put (Payload size) = put $ LBS.replicate size 7
    get = Payload . LBS.length <$> get

data Parcel = Parcel
    { parcelNo :: Int
    , payload  :: Payload
    } deriving (Eq, Ord, Show, Generic)

instance Binary Parcel
instance Message Parcel where
    messageName _ = "Parcel"
    formatMessage _ = "Parcel"

instance Arbitrary Parcel where
    arbitrary = Parcel
            <$> (getLarge <$> arbitrary)
            <*> pure (Payload 0)

newtype HeavyParcel = HeavyParcel
    { getHeavyParcel :: Parcel
    } deriving (Eq, Ord, Show, Binary)

instance Arbitrary HeavyParcel where
    arbitrary = mkHeavy <$> arbitrary <*> choose (0, 99000)
      where
        mkHeavy parcel size = HeavyParcel parcel { payload = Payload size }


-- * TestState

data TestState = TestState
    { _fails    :: [String]
    , _expected :: S.Set Parcel
    }

mkTestState :: TestState
mkTestState = TestState
    { _fails         = []
    , _expected      = S.empty
    }

makeLenses ''TestState

instance Testable TestState where
    property TestState{..}
        | not $ null _fails      = property failed
            { reason = "Fails: \n" ++ L.intercalate "\n" (("\t" ++) <$> _fails) }

        | not $ S.null _expected = property failed
            { reason = "Missed messages: " ++ show _expected }

        | otherwise              = property succeeded

modifyTestState :: MonadIO m => TVar TestState -> StateT TestState STM () -> m ()
modifyTestState ts how = liftIO . atomically $ modifyTVarS ts how

addFail :: MonadIO m => TVar TestState -> String -> m ()
addFail testState desc = modifyTestState testState $ fails %= (desc :)

reportingFail :: TVar TestState -> String -> Production () -> Production ()
reportingFail testState actionName act = do
    act `catch` \(SomeException e) ->
        addFail testState $ "Error thrown in " ++ actionName ++ ": " ++ show e

newWork :: TVar TestState -> String -> Production () -> Production ()
newWork testState workerName act = do
    reportingFail testState workerName act


-- * Misc

-- I guess, errors in network-transport wasn't supposed to be processed in such way ^^
throwLeft :: Exception e => Production (Either e a) -> Production a
throwLeft = (>>= f)
  where
    f (Left e)  = throw e
    f (Right a) = return a

-- | Await for predicate to become True, with timeout
awaitSTM :: TimeUnit t => t -> STM Bool -> Production ()
awaitSTM time predicate = do
    tvar <- liftIO $ newTVarIO False
    void . fork $ do
        delay time
        liftIO . atomically $ writeTVar tvar True
    liftIO . atomically $
        check =<< (||) <$> predicate <*> readTVar tvar


-- * Talk style

-- | Way to send pack of messages
data TalkStyle
    = SingleMessageStyle
    -- ^ corresponds to `sendTo` and `ListenerActionOneMsg` usage
    | ConversationStyle
    -- ^ corresponds to `withConnectionTo` and `ListenerActionConversation` usage

instance Show TalkStyle where
    show SingleMessageStyle = "single-message style"
    show ConversationStyle  = "conversation style"

sendAll
    :: ( Binary msg, Message msg, MonadIO m
       , Mockable Concurrently m
       , Mockable Delay m
       , Mockable Async m
       , Mockable Throw m
       , Mockable Catch m
       , Mockable SharedExclusive m
       )
    => TalkStyle
    -> SendActions BinaryP () m
    -> NodeId
    -> [msg]
    -> m ()
sendAll SingleMessageStyle sendActions peerId msgs =
    --void $ forConcurrently msgs $ sendTo sendActions peerId
    timeout "sendAll" 30000000 $ forM_ msgs $ sendTo sendActions peerId

sendAll ConversationStyle sendActions peerId msgs =
    timeout "sendAll" 30000000 $
        void . withConnectionTo sendActions @_ @Bool peerId $ \peerData cactions -> forM_ msgs $
        \msg -> do
            send cactions msg
            _ <- recv cactions
            pure ()

receiveAll
    :: ( Binary msg, Message msg, MonadIO m
       , Mockable Delay m
       , Mockable Async m
       , Mockable SharedExclusive m
       , Mockable Throw m
       , Mockable Catch m
       )
    => TalkStyle
    -> (msg -> m ())
    -> ListenerAction BinaryP () m
receiveAll SingleMessageStyle handler =
    ListenerActionOneMsg $ \_ _ _ -> timeout "receiveAll" 30000000 . handler
-- For conversation style, we send a response for every message received.
-- The sender awaits a response for each message. This ensures that the
-- sender doesn't finish before the conversation SYN/ACK completes.
receiveAll ConversationStyle  handler =
    ListenerActionConversation @_ @_ @_ @Bool $ \_ _ cactions ->
        let loop = do mmsg <- recv cactions
                      case mmsg of
                          Nothing -> pure ()
                          Just msg -> do
                              handler msg
                              send cactions True
                              loop
        in  timeout "receiveAll" 30000000 loop

makeInMemoryTransport :: IO NT.Transport
makeInMemoryTransport = InMemory.createTransport

makeTCPTransport
    :: String
    -> String
    -> String
    -> (forall t . IO (TCP.QDisc t))
    -> IO NT.Transport
makeTCPTransport bind hostAddr port qdisc = do
    let tcpParams = TCP.defaultTCPParameters {
              TCP.tcpReuseServerAddr = True
            , TCP.tcpReuseClientAddr = True
            , TCP.tcpNewQDisc = qdisc
            }
    choice <- TCP.createTransport bind hostAddr port tcpParams
    case choice of
        Left err -> error (show err)
        Right transport -> return transport

-- * Test template

deliveryTest :: NT.Transport
             -> TVar TestState
             -> [NodeId -> Worker BinaryP () Production]
             -> [Listener BinaryP () Production]
             -> IO Property
deliveryTest transport_ testState workers listeners = runProduction $ do

    let transport = concrete transport_

    let prng1 = mkStdGen 0
    let prng2 = mkStdGen 1

    serverAddressVar <- newSharedExclusive
    clientFinished <- newSharedExclusive
    serverFinished <- newSharedExclusive

    let server = node transport prng1 BinaryP () $ \serverNode -> do
            NodeAction listeners $ \_ -> do
                -- Give our address to the client.
                putSharedExclusive serverAddressVar (nodeId serverNode)
                -- Don't stop until the client has finished.
                takeSharedExclusive clientFinished
                -- Wait for the expected values to come, with 5 second timeout.
                awaitSTM (5 :: Second) $ S.null . _expected <$> readTVar testState
                -- Allow the client to stop.
                putSharedExclusive serverFinished ()

    let client = node transport prng2 BinaryP () $ \clientNode ->
            NodeAction [] $ \sendActions -> do
                serverAddress <- takeSharedExclusive serverAddressVar
                void . forConcurrently workers $ \worker ->
                    worker serverAddress sendActions
                -- Tell the server that we're done.
                putSharedExclusive clientFinished ()
                -- Wait until the server has finished.
                takeSharedExclusive serverFinished

    withAsync server $ \serverPromise -> do
        withAsync client $ \clientPromise -> do
            timeout "waiting for client to finish" 30000000 (wait clientPromise)
            timeout "waiting for server to finish" 30000000 (wait serverPromise)

    -- form test results
    liftIO . atomically $
        property <$> readTVar testState
