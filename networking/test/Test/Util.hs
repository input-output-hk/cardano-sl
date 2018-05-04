{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

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

       , throwLeft

       , sendAll
       , receiveAll

       , deliveryTest

       ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (forConcurrently, wait, withAsync)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import           Control.Concurrent.STM (STM, atomically, check, registerDelay)
import           Control.Concurrent.STM.TVar (TVar, readTVar)
import           Control.Exception (Exception, SomeException (..), catch, finally, throwIO)
import           Control.Lens (makeLenses)
import           Control.Monad (forM_, void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.State.Strict (StateT)
import           Data.Binary (Binary (..))
import qualified Data.ByteString as LBS
import qualified Data.List as L
import qualified Data.Set as S
import           Data.Time.Units (Microsecond, Second, TimeUnit, toMicroseconds)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import qualified Network.Transport as NT (Transport)
import qualified Network.Transport.InMemory as InMemory
import qualified Network.Transport.TCP as TCP
import           Serokell.Util.Concurrent (modifyTVarS)
import           System.Random (mkStdGen)
import           Test.QuickCheck (Property)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
import           Test.QuickCheck.Gen (choose)
import           Test.QuickCheck.Modifiers (getLarge)
import           Test.QuickCheck.Property (Testable (..), failed, reason, succeeded)

import           Node (Conversation (..), ConversationActions (..), Listener (..), Message (..),
                       NodeAction (..), NodeEnvironment, NodeId, converseWith, noReceiveDelay, node,
                       nodeId, simpleNodeEndPoint)
import           Node.Conversation (Converse)
import           Node.Message.Binary (BinaryP, binaryPacking)
import           Pos.Util.Trace (wlogTrace)

-- | Run a computation, but kill it if it takes more than a given number of
--   Microseconds to complete. If that happens, log using a given string
--   prefix.
--
--   TODO use System.Timeout?
timeout
    :: String
    -> Microsecond
    -> IO t
    -> IO t
timeout str us m = do
    var <- newEmptyMVar
    let action = do
            t <- (fmap Right m) `catch` (\(e :: SomeException) -> return (Left e))
            putMVar var t
    let timeoutAction = do
            threadDelay (fromIntegral us)
            putMVar var (Left . error $ str ++ " : timeout after " ++ show us)
    withAsync action $ \_ -> do
        withAsync timeoutAction $ \_ -> do
            choice <- readMVar var
            case choice of
                Left e  -> throwIO e
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
    messageCode _ = 0
    formatMessage _ = "Parcel"

instance Arbitrary Parcel where
    arbitrary = Parcel
            <$> (getLarge <$> arbitrary)
            <*> pure (Payload 0)

-- | A parcel with a special Arbitrary instance giving a payload of length
--   at least 1k, at most 100k.
newtype HeavyParcel = HeavyParcel
    { getHeavyParcel :: Parcel
    } deriving (Eq, Ord, Show, Binary)

instance Arbitrary HeavyParcel where
    arbitrary = mkHeavy <$> arbitrary <*> choose (1000, 100000)
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

-- * Misc

-- I guess, errors in network-transport wasn't supposed to be processed in such way ^^
throwLeft :: Exception e => IO (Either e a) -> IO a
throwLeft = (>>= f)
  where
    f (Left e)  = throwIO e
    f (Right a) = return a

-- | Await for predicate to become True, with timeout
awaitSTM :: TimeUnit t => t -> STM Bool -> IO ()
awaitSTM time predicate = do
    tvar <- registerDelay (fromIntegral (toMicroseconds time))
    atomically (((||) <$> predicate <*> readTVar tvar) >>= check)

sendAll
    :: ( Binary msg
       , Message msg
       )
    => Converse BinaryP ()
    -> NodeId
    -> [msg]
    -> IO ()
sendAll converse peerId msgs =
    timeout "sendAll" 30000000 $
        void . converseWith converse peerId $
            \_ -> Conversation $ \cactions -> forM_ msgs $
                \msg -> do
                    send cactions msg
                    (_ :: Maybe Bool) <- recv cactions maxBound
                    pure ()

receiveAll
    :: ( Binary msg
       , Message msg
       )
    => (msg -> IO ())
    -> Listener BinaryP ()
-- For conversation style, we send a response for every message received.
-- The sender awaits a response for each message. This ensures that the
-- sender doesn't finish before the conversation SYN/ACK completes.
receiveAll handler =
    Listener @_ @_ @Bool $ \_ _ cactions ->
        let loop = do mmsg <- recv cactions maxBound
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
    -> Word32
    -> IO NT.Transport
makeTCPTransport bind hostAddr port qdisc mtu = do
    let tcpParams = TCP.defaultTCPParameters {
              TCP.tcpReuseServerAddr = True
            , TCP.tcpReuseClientAddr = True
            , TCP.tcpNewQDisc = qdisc
            , TCP.tcpMaxReceiveLength = mtu
            , TCP.tcpNoDelay = True
            }
    choice <- TCP.createTransport (TCP.Addressable (TCP.TCPAddrInfo bind port ((,) hostAddr))) tcpParams
    case choice of
        Left err        -> error (show err)
        Right transport -> return transport

-- * Test template

deliveryTest :: NT.Transport
             -> NodeEnvironment
             -> TVar TestState
             -> [NodeId -> Converse BinaryP () -> IO ()]
             -> [Listener BinaryP ()]
             -> IO Property
deliveryTest transport nodeEnv testState workers listeners = do

    let logTrace = wlogTrace ""

    let prng1 = mkStdGen 0
    let prng2 = mkStdGen 1

    serverAddressVar <- newEmptyMVar
    clientFinished <- newEmptyMVar
    serverFinished <- newEmptyMVar

    let server = node logTrace (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) prng1 binaryPacking () nodeEnv $ \serverNode -> do
            NodeAction (const listeners) $ \_ -> do
                -- Give our address to the client.
                putMVar serverAddressVar (nodeId serverNode)
                -- Don't stop until the client has finished.
                takeMVar clientFinished
                -- Wait for the expected values to come, with 5 second timeout.
                awaitSTM (5 :: Second) $ S.null . _expected <$> readTVar testState
                -- Allow the client to stop.
                putMVar serverFinished ()

    let client = node logTrace (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay) prng2 binaryPacking () nodeEnv $ \_ ->
            NodeAction (const []) $ \converse -> do
                serverAddress <- takeMVar serverAddressVar
                let act = void . forConcurrently workers $ \worker ->
                        worker serverAddress converse
                -- Tell the server that we're done.
                act `finally` putMVar clientFinished ()
                -- Wait until the server has finished.
                takeMVar serverFinished

    withAsync server $ \serverPromise -> do
        withAsync client $ \clientPromise -> do
            timeout "waiting for client to finish" 30000000 (wait clientPromise)
            timeout "waiting for server to finish" 30000000 (wait serverPromise)

    -- form test results
    liftIO . atomically $
        property <$> readTVar testState
