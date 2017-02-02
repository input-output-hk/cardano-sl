{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (unless)
import           GHC.Generics (Generic)
import           Control.Monad.IO.Class (liftIO)
import           Data.String (fromString)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Binary (Binary)
import           Network.Transport.Abstract
import           Network.Transport.Concrete
import qualified Network.Transport.TCP as TCP
import qualified Network.Transport.Concrete.TCP as TCP
import           Node
import           Node.Message
import           Node.Util.Monitor (setupMonitor)
import           System.Environment (getArgs)
import           System.Random (mkStdGen)
import           Data.Time.Units
import           Mockable.Concurrent (delay, async, wait, cancel)
import           Mockable.SharedAtomic
import           Mockable.Production

-- |
-- = Abuse demonstration number 1.
--
-- The client will ping some server as fast as possible with rather large
-- payloads (circa 16 megabytes).
--
-- The client will handle these with single-message listeners that force the
-- length of the bytes and record the total amount seen so far.
--
-- If an unbounded QDisc is chosen, then the server's heap will grow without
-- bound and the OS will kill it. This is confirmed on an  Intel i5, 4 cores
-- at 2.50GHz each, 8Gb RAM, where the server is given 1 capability and the
-- client given 4.
-- If, on the same machine, a one-place QDisc is chosen, the maximum
-- residency is ~60mb and the program chugs along just fine.

data Ping = Ping ByteString
deriving instance Generic Ping
instance Binary Ping
instance Message Ping where
    messageName _ = fromString "Ping"
    formatMessage _ = fromString "Ping"

-- ~16mb
payloadSize :: Integral a => a
payloadSize = 2^24

payload :: ByteString
payload = fromString (take payloadSize (repeat '0'))

main :: IO ()
main = do

    choice : rest <- getArgs

    case choice of
        "server" -> case rest of
            [serverPort, "unbounded"] -> runProduction $ server serverPort Unbounded
            [serverPort, "one_place"] -> runProduction $ server serverPort OnePlace
            _ -> error "Second argument for a server must be a port, third must be 'unbounded' or 'one_place'"
        "client" -> case rest of
            [serverPort, clientPort] -> runProduction $ client serverPort clientPort
            _ -> error "Arguments for a client must be the server port followed by client port"
        _ -> error "First argument must be server or client"

data QDiscChoice = OnePlace | Unbounded

makeQDisc :: QDiscChoice -> IO (TCP.QDisc t)
makeQDisc choice = case choice of
    OnePlace -> TCP.simpleOnePlaceQDisc
    Unbounded -> TCP.simpleUnboundedQDisc

server :: String -> QDiscChoice -> Production ()
server port qdiscChoice = do

    let qdisc = makeQDisc qdiscChoice

    Right transport_ <-
        liftIO $ TCP.createTransport "0.0.0.0" "127.0.0.1" port (TCP.defaultTCPParameters { TCP.tcpQDisc = qdisc })
    let transport = concrete transport_
    let prng = mkStdGen 0
    totalBytes <- newSharedAtomic 0

    liftIO . putStrLn $ "Starting server on port " ++ show port

    node transport prng BinaryP () $ \node -> do
        -- Set up the EKG monitor.
        NodeAction [listener totalBytes] $ \saction -> do
            setupMonitor 8000 runProduction node
            -- Just wait for user interrupt
            liftIO . putStrLn $ "Server running. Press any key to stop."
            liftIO getChar

    closeTransport transport

    total <- modifySharedAtomic totalBytes $ \bs -> return (bs, bs)

    liftIO . putStrLn $ "Server processed " ++ show total ++ " bytes"

    where

    -- The server listener just forces the whole bytestring then discards.
    listener :: SharedAtomicT Production Integer -> ListenerAction BinaryP () Production
    listener totalBytes = ListenerActionOneMsg $ \() peer sactions (Ping body) -> do
        -- Retain the body for a few seconds.
        let !len = BS.length body
        modifySharedAtomic totalBytes $ \total ->
            let !newTotal = fromIntegral len + total
            in  return (newTotal, ())
        --liftIO . putStrLn $ "Server heard message of length " ++ show (BS.length body)

client :: String -> String -> Production ()
client serverPort clientPort = do

    Right transport_ <-
        liftIO $ TCP.createTransport "0.0.0.0" "127.0.0.1" clientPort TCP.defaultTCPParameters
    let transport = concrete transport_
    --let transport = concrete transport_
    let prng = mkStdGen 1
    -- Assume the server's end point identifier is 0. It always will be.
    let serverAddress = NodeId (TCP.encodeEndPointAddress "127.0.0.1" serverPort 0)

    liftIO . putStrLn $ "Starting client on port " ++ show clientPort

    totalBytes <- node transport prng BinaryP () $ \node ->
        NodeAction [] $ \saction -> do
            -- Track total bytes sent, and a bool indicating whether we should
            -- stop, so that we don't have to resort to cancelling the threads
            -- (which may leave some bytes missing from the total).
            totalBytes <- newSharedAtomic (0, False)
            -- 4 threads will spam.
            spammer1 <- async $ spamServer serverAddress totalBytes saction
            spammer2 <- async $ spamServer serverAddress totalBytes saction
            spammer3 <- async $ spamServer serverAddress totalBytes saction
            spammer4 <- async $ spamServer serverAddress totalBytes saction
            liftIO . putStrLn $ "Client is spamming the server. Press any key to stop."
            liftIO getChar
            -- Signal the threads to not try another send.
            -- They'll eventually stop.
            _ <- modifySharedAtomic totalBytes $ \(bs, _) -> return ((bs, True), ())
            wait spammer1
            wait spammer2
            wait spammer3
            wait spammer4
            modifySharedAtomic totalBytes $ \total -> return (total, fst total)
            
    closeTransport transport

    liftIO . putStrLn $ "Client sent " ++ show totalBytes ++ " bytes"

spamServer
    :: NodeId
    -> SharedAtomicT Production (Integer, Bool)
    -> SendActions BinaryP () Production
    -> Production ()
spamServer server totalBytes sactions = do
    sendTo sactions server (Ping payload)
    stop <- modifySharedAtomic totalBytes $ \(total, stop) ->
        let !newTotal = total + payloadSize
        in  return ((newTotal, stop), stop)
    unless stop (spamServer server totalBytes sactions)
