{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main where


import BasePrelude
import Crypto.Hash
import qualified Crypto.SecretSharing as Secret
import qualified Crypto.SecretSharing.Internal as Secret
import Data.Time
import qualified SlaveThread as Slave
import System.Random
import qualified Data.Binary as Bin
import qualified Data.Set as Set
import Data.Set (Set)

main :: IO ()
-- Here's how to run a simple system with two nodes pinging each other:
-- main = runNodes [node_ping 1, node_ping 0]
main = return ()

type NodeId = Int
type Hash = Digest SHA256

----------------------------------------------------------------------------
-- Transactions, blocks
----------------------------------------------------------------------------

-- | Transaction input
data TxIn = TxIn {
  txInHash :: Hash,        -- ^ Which transaction's output is used
  txInIndex :: Int }       -- ^ Index of the output in transaction's outputs
  deriving (Eq, Ord, Show)

-- | Transaction output
data TxOut = TxOut {
  txOutValue :: Word64 }   -- ^ Output value
  deriving (Eq, Ord, Show)

-- | Transaction
data Tx = Tx {
  txInputs :: [TxIn],
  txOutputs :: [TxOut],
  txHash :: Hash }         -- ^ Hash of the transaction
  deriving (Eq, Ord, Show)

-- | An entry in a block
data Entry

    -- | Transaction
  = ETx Tx

    -- | Hash of random string U that a node has committed to
  | EUHash NodeId Hash
    -- | An encrypted piece of secret-shared U that the first node sent to
    -- the second node (and encrypted with the second node's pubkey)
  | EUShare NodeId NodeId (Encrypted Secret.Share)
    -- | Leaders for a specific epoch
  | ELeaders Int [NodeId]

  deriving (Eq, Ord, Show)

deriving instance Ord Secret.ByteShare
deriving instance Ord Secret.Share
  
-- | Block
type Block = [Entry]

displayEntry :: Entry -> String
displayEntry (ETx tx) =
  "transaction " ++ show tx
displayEntry (EUHash nid h) =
  printf "[%d]'s commitment = %s" nid (show h)
displayEntry (EUShare from to share) =
  printf "[%d]'s share for [%d] = %s" from to (show share)
displayEntry (ELeaders epoch leaders) =
  printf "leaders for epoch %d = %s" epoch (show leaders)

----------------------------------------------------------------------------
-- Very advanced crypto
----------------------------------------------------------------------------

data Encrypted a = Enc NodeId a
  deriving (Eq, Ord, Show)

-- | “Encrypt” data with node's pubkey
encrypt :: NodeId -> a -> Encrypted a
encrypt = Enc

-- | “Decrypt” data with node's private key
decrypt :: NodeId -> Encrypted a -> Maybe a
decrypt nid (Enc nid' a) = if nid == nid' then Just a else Nothing

----------------------------------------------------------------------------
-- Messages that nodes send to each other
----------------------------------------------------------------------------

data Message
  = MEntry Entry
  | MBlock Block
  | MPing
  deriving (Eq, Ord, Show)

displayMessage :: Message -> String
displayMessage MPing = "ping"
displayMessage (MEntry e) = displayEntry e
displayMessage (MBlock es) = printf "block with %d entries" (length es)

----------------------------------------------------------------------------
-- Network simulation
----------------------------------------------------------------------------

{- |
A node is given:

* Its ID
* A function to send messages

A node also provides a callback which can be used to send messages to the node (and the callback knows who sent it a message).
-}
type Node
  =  NodeId
  -> (NodeId -> Message -> IO ())
  -> IO (NodeId -> Message -> IO ())

node_ping :: Int -> Node
node_ping pingNum = \myNum sendTo -> do
  void $ Slave.fork $ inSlot $ \_epoch _slot -> do
    logInfo myNum (printf "pinging [%d]" pingNum)
    sendTo pingNum MPing
  return $ \from msg -> do
    case msg of
      MPing -> do
        logInfo myNum (printf "pinged by [%d]" from)
      _ -> do
        logInfo myNum (printf "unknown message from [%d]" from)

runNodes :: [Node] -> IO ()
runNodes nodes = do
  -- The system shall start working in a bit of time (not exactly right now –
  -- due to the way inSlot implemented, it'd be nice to wait a bit)
  writeIORef systemStart . (addUTCTime (slotDuration/2)) =<< getCurrentTime
  tid <- Slave.fork $ mdo
    runLogPrinting
    void $ Slave.fork $ inSlot' $ \epoch slot -> do
      when (slot == 0) $ logRaw (printf "====== EPOCH %d ======" epoch)
      logRaw (printf "--- slot %d ---" slot)
    nodeCallbacks <-
      let send from to msg = (nodeCallbacks !! to) from msg
      in  sequence [node nid (send nid) | (nid, node) <- zip [0..] nodes]
    forever $ threadDelay 1000000
  forever (threadDelay 1000000) `onException` killThread tid

systemStart :: IORef UTCTime
systemStart = unsafePerformIO $ newIORef undefined
{-# NOINLINE systemStart #-}

{- |
Run something at the beginning of every slot. The first parameter is epoch number (starting from 0) and the second parameter is slot number in the epoch (from 0 to epochLen-1).

It loops, so you might want to use 'Slave.fork' with it.

There's a slight delay so that messages from nodes wouldn't interfere with slot and epoch delimiters:

    --- slot 0 ---

    ====== EPOCH 0 ======

The version without delay is called inSlot'.
-}
inSlot :: (Int -> Int -> IO ()) -> IO ()
inSlot f = inSlot' (\x y -> threadDelay 20000 >> f x y)

inSlot' :: (Int -> Int -> IO ()) -> IO ()
inSlot' f = do
  start <- readIORef systemStart
  forever $ do
    -- Wait until the next slot begins
    now <- getCurrentTime
    let untilNext = slotDuration - mod' (diffUTCTime now start) slotDuration
    let currentAbsoluteSlot = div' (diffUTCTime now start) slotDuration
    threadDelay (ceiling (untilNext * 1000000))
    -- Do stuff
    let (epoch, slot) = (currentAbsoluteSlot+1) `divMod` epochSlots
    f epoch slot

{-
If some node becomes inactive, other nodes will be able to recover its `u` by exchanging decrypted pieces of secret-shared `u` they've been sent.

After K slots all nodes are guaranteed to have a common prefix; each node computes the random satoshi index from all available `u`s to find out who has won the leader election and can generate the next block.
-}

fullNode :: Node
fullNode = \myId sendTo -> do
  -- The node maintains a list of entries that aren't included into any block
  -- yet
  entries <- newIORef (mempty :: Set Entry)
  let createEntry x = modifyIORef' entries (Set.insert x)

  let createBlock :: IO Block
      createBlock = Set.toList <$>
        atomicModifyIORef' entries (\es -> (mempty, es))

  void $ Slave.fork $ inSlot $ \epoch slot -> do
    -- For now we just send messages to everyone instead of letting them
    -- propagate, implementing peers, etc.
    let sendEveryone x = for_ [0..n-1] $ \i -> sendTo i x

    -- If this is the first epoch ever, we haven't agreed on who will mine
    -- blocks in this epoch, so let's just say that the 0th node is the
    -- master node. In slot 0, node 0 will announce who will mine blocks in
    -- the next epoch; in other slots it will just mine new blocks.
    when (myId == 0 && epoch == 0) $ do
      when (slot == 0) $ do
        leaders <- replicateM epochSlots (randomRIO (0, n-1))
        createEntry $ ELeaders (epoch+1) leaders
        logInfo myId "generated random leaders for epoch 1 (as master node)"
      blk <- createBlock
      sendEveryone (MBlock blk)
      if null blk then
        logInfo myId "created a block (empty)"
      else do
        logInfo myId "created a block:"
        for_ blk $ \e -> logInfo myId ("  * " ++ displayEntry e)

    -- When the epoch starts, we do the following:
    --   • generate U, a random bitvector that will be used as a seed to
    --     the PRNG that will choose leaders (nodes who will mine each block
    --     in the next epoch). For now the seed is actually just a Word64.
    --   • secret-share U and encrypt each piece with corresponding
    --     node's pubkey; the secret can be recovered with at least
    --     N−T available pieces
    --   • post encrypted shares and a commitment to U to the blockchain
    --     (so that later on we wouldn't be able to cheat by using
    --     a different U)
    when (slot == 0) $ do
      u <- randomIO :: IO Word64
      shares <- Secret.encode (n-t) n (Bin.encode u)
      for_ (zip shares [0..]) $ \(share, i) ->
        sendEveryone (MEntry (EUShare myId i (encrypt i share)))
      sendEveryone (MEntry (EUHash myId (hashlazy (Bin.encode u))))

    -- According to @gromak (who isn't sure about this, but neither am I):
    -- https://input-output-rnd.slack.com/archives/paper-pos/p1474991379000006
    --
    -- > We send commitments during the first slot and they are put into the
    -- first block. Then we wait for K periods so that all nodes agree upon
    -- the same first block. But we see that it’s not enough because they can
    -- agree upon dishonest block. That’s why we need to wait for K more
    -- blocks. So all this *commitment* phase takes 2K blocks.
    --
    -- So, what happens now is that we 

  return $ \from msg -> do
    return ()
    -- logInfo myId (printf "message from [%d]: %s" from (displayMessage msg))

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

n :: Integral a => a
n = 3

t :: Integral a => a
t = 0

k :: Integral a => a
k = 3

slotDuration :: Num a => a
slotDuration = 2  -- seconds

epochSlots :: Integral a => a
epochSlots = 6*k

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

logChan :: Chan String
logChan = unsafePerformIO newChan
{-# NOINLINE logChan #-}

runLogPrinting :: IO ()
runLogPrinting = void $ Slave.fork $
  forever (putStrLn =<< readChan logChan)

logInfo :: NodeId -> String -> IO ()
logInfo nid s = logRaw (printf "[%d] %s" nid s)

logRaw :: String -> IO ()
logRaw = writeChan logChan
