{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Main where


import BasePrelude
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.SecretSharing as Secret
import Data.Time
import qualified SlaveThread as Slave


main :: IO ()
main = runNodes [node0, node1]

type NodeId = Int
type Hash = Digest SHA256

----------------------------------------------------------------------------
-- Transactions, blocks
----------------------------------------------------------------------------

-- | Transaction input
data TxIn = TxIn {
  txInHash :: Hash,        -- ^ Which transaction's output is used
  txInIndex :: Int }       -- ^ Index of the output in transaction's outputs

-- | Transaction output
data TxOut = TxOut {
  txOutValue :: Word64 }   -- ^ Output value

-- | Transaction
data Tx = Tx {
  txInputs :: [TxIn],
  txOutputs :: [TxOut],
  txHash :: Hash }         -- ^ Hash of the transaction

-- | An entry in a block
data BlockEntry

  -- | Transaction
  = ETx Tx

  -- | Hash of random string U that a node has committed to
  | EUHash NodeId Hash
  -- | An encrypted piece of secret-shared U that the first node sent to
  -- the second node (and encrypted with the second node's pubkey)
  | EUShare NodeId NodeId Secret.Share

-- | Block
type Block = [BlockEntry]

----------------------------------------------------------------------------
-- Messages that nodes send to each other
----------------------------------------------------------------------------

data Message
  = MBlockEntry BlockEntry
  | MPing

----------------------------------------------------------------------------
-- Network simulation
----------------------------------------------------------------------------

-- | A node is given a function to send messages. A node also provides a
-- callback which can be used to send messages to the node (and the callback
-- knows who sent it a message).
type Node = (NodeId -> Message -> IO ()) -> IO (NodeId -> Message -> IO ())

node0 :: Node
node0 sendTo = do
  Slave.fork $ inSlot $ \epoch slot -> do
    threadDelay 100057
    printf "epoch %d slot %d: 0: pinging 1\n" epoch slot
    sendTo 1 MPing
  return $ \from msg -> do
    case msg of
      MPing -> do
        putStrLn ("0: pinged by " ++ show from)
      _ -> do
        putStrLn "0: unknown message"

node1 :: Node
node1 sendTo = do
  Slave.fork $ inSlot $ \epoch slot -> do
    threadDelay 350235
    printf "epoch %d slot %d: 1: pinging 0\n" epoch slot
    sendTo 0 MPing
  return $ \from msg -> do
    case msg of
      MPing -> do
        putStrLn ("1: pinged by " ++ show from)
      _ -> do
        putStrLn "1: unknown message"

runNodes :: [Node] -> IO ()
runNodes nodes = do
  -- The system shall start working in a bit of time (not exactly right now –
  -- due to the way inSlot implemented, it'd be nice to wait a bit)
  writeIORef systemStart . (addUTCTime (slotDuration/2)) =<< getCurrentTime
  tid <- Slave.fork $ mdo
    nodeCallbacks <-
      let send from to msg = (nodeCallbacks !! to) from msg
      in  sequence [node (send nid) | (nid, node) <- zip [0..] nodes]
    forever $ threadDelay 1000000
  forever (threadDelay 1000000) `onException` killThread tid

k :: Integral a => a
k = 3

slotDuration :: Num a => a
slotDuration = 2

systemStart :: IORef UTCTime
systemStart = unsafePerformIO $ newIORef undefined
{-# NOINLINE systemStart #-}

-- | Run something at the beginning of every slot. The first parameter is
-- epoch number (starting from 0) and the second parameter is slot number in
-- the epoch (from 0 to 6k-1).
inSlot :: (Int -> Int -> IO ()) -> IO ()
inSlot f = do
  start <- readIORef systemStart
  forever $ do
    -- Wait until the next slot begins
    t <- getCurrentTime
    let untilNext = slotDuration - mod' (diffUTCTime t start) slotDuration
    let currentAbsoluteSlot = div' (diffUTCTime t start) slotDuration
    threadDelay (ceiling (untilNext * 1000000))
    -- Do stuff
    let (epoch, slot) = (currentAbsoluteSlot+1) `divMod` (6*k)
    f epoch slot

{-
When an epoch starts, each of N active nodes:

  • generates `u`, a random bitvector of length R log τ, where τ is the amount of satoshis in the system
  • secret-shares `u` (by splitting it into N pieces) and encrypts each piece with corresponding node's pubkey; the secret can be recovered with at least N−T available pieces
  • posts encrypted shares and a commitment to `u` to the blockchain

If some node becomes inactive, other nodes will be able to recover its `u` by exchanging decrypted pieces of secret-shared `u` they've been sent.

After K slots all nodes are guaranteed to have a common prefix; each node computes the random satoshi index from all available `u`s to find out who has won the leader election and can generate the next block.
-}
