{-# LANGUAGE TemplateHaskell #-}
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
import Data.Map (Map)
import Data.Default
import Control.Lens
import Control.Monad.State


type NodeId = Int
type Hash = Digest SHA256

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

logError :: NodeId -> String -> IO ()
logError nid s = logRaw (printf "[%d] ERROR: %s" nid s)

logRaw :: String -> IO ()
logRaw = writeChan logChan

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

n :: Integral a => a
n = 3

t :: Integral a => a
t = 0

k :: Integral a => a
k = 3

slotDuration :: Fractional a => a
slotDuration = 1  -- seconds

epochSlots :: Integral a => a
epochSlots = 6*k

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
displayEntry (EUShare n_from n_to share) =
  printf "[%d]'s share for [%d] = %s" n_from n_to (show share)
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
  return $ \n_from msg -> do
    case msg of
      MPing -> do
        logInfo myNum (printf "pinged by [%d]" n_from)
      _ -> do
        logInfo myNum (printf "unknown message from [%d]" n_from)

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
      let send n_from n_to msg = (nodeCallbacks !! n_to) n_from msg
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

{- ==================== TODO ====================

Timing issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* What to do about blocks delivered a bit late? E.g. let's assume that a block was generated in slot X, but received by another node in slot Y. What are the conditions on Y under which the block should (and shouldn't) be accepted?

* What to do about extremely delayed entries that are the same as ones we already received before (but already included into one of the previous blocks?) How does Bitcoin deal with it?

* We should distinguish between new blocks and old blocks; new blocks aren't trusted, old blocks are.

* Off-by-one errors: should we trust blocks that are K slots old (or older), or only ones that are K+1 slots old or older?

* Let's say that we receive a transaction, and then we receive a block containing that transaction. We remove the transaction from our list of pending transactions. Later (before K slots pass) it turns out that that block was bad, and we discard it; then we should add the transaction back. Right? If this is how it works, then it means that somebody can prevent the transaction from being included into the blockchain for the duration of K−1 slots – right? How easy/probable/important is it in practice?

Validation issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Blocks should build on each other. We should discard shorter histories.

* We should validate entries that we receive

* We should validate blocks that we receive; in particular, we should check that blocks we receive are generated by nodes who had the right to generate them

Other issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Create a typo synonym for epoch number?

* We should be able to query blocks from other nodes, like in Bitcoin (if e.g. we've been offline for several slots or even epochs) but this isn't implemented yet. In fact, most stuff from Bitcoin isn't implemented.

-}

data FullNodeState = FullNodeState {
  -- | List of entries that the node has received but that aren't included
  -- into any block yet
  _pendingEntries :: Set Entry,
  -- | Leaders for epochs (currently it just stores leaders for all epochs,
  -- but we really only need the leader list for this epoch and the next
  -- epoch)
  _epochLeaders :: Map Int [NodeId],
  -- | Blocks
  _blocks :: [Block] }

makeLenses ''FullNodeState

instance Default FullNodeState where
  def = FullNodeState {
    _pendingEntries = mempty,
    _epochLeaders = mempty,
    _blocks = [] }

{-
If some node becomes inactive, other nodes will be able to recover its U by exchanging decrypted pieces of secret-shared U they've been sent.

After K slots all nodes are guaranteed to have a common prefix; each node computes the random satoshi index from all available Us to find out who has won the leader election and can generate the next block.
-}

fullNode :: Node
fullNode = \myId sendTo -> do
  nodeState <- newIORef (def :: FullNodeState)
  let withNodeState act = atomicModifyIORef' nodeState (swap . runState act)

  -- Empty the list of pending entries and create a block
  let createBlock :: IO Block
      createBlock = withNodeState $ do
        es <- pendingEntries <<.= mempty
        return (Set.toList es)

  -- This will run at the beginning of each slot:
  void $ Slave.fork $ inSlot $ \epoch slot -> do
    -- For now we just send messages to everyone instead of letting them
    -- propagate, implementing peers, etc.
    let sendEveryone x = for_ [0..n-1] $ \i -> sendTo i x

    -- Create a block and send it to everyone
    let createAndSendBlock = do
          blk <- createBlock
          sendEveryone (MBlock blk)
          if null blk then
            logInfo myId "created an empty block"
          else do
            logInfo myId "created a block:"
            for_ blk $ \e -> logInfo myId ("  * " ++ displayEntry e)

    -- If this is the first epoch ever, we haven't agreed on who will mine
    -- blocks in this epoch, so let's just say that the 0th node is the
    -- master node. In slot 0, node 0 will announce who will mine blocks in
    -- the next epoch; in other slots it will just mine new blocks.
    when (myId == 0 && epoch == 0) $ do
      when (slot == 0) $ do
        leaders <- replicateM epochSlots (randomRIO (0, n-1))
        withNodeState $ do
          pendingEntries %= Set.insert (ELeaders (epoch+1) leaders)
        logInfo myId "generated random leaders for epoch 1 (as master node)"
      createAndSendBlock

    -- When the epoch starts, we do the following:
    --   * generate U, a random bitvector that will be used as a seed to
    --     the PRNG that will choose leaders (nodes who will mine each block
    --     in the next epoch). For now the seed is actually just a Word64.
    --   * secret-share U and encrypt each piece with corresponding
    --     node's pubkey; the secret can be recovered with at least
    --     N−T available pieces
    --   * post encrypted shares and a commitment to U to the blockchain
    --     (so that later on we wouldn't be able to cheat by using
    --     a different U)
    when (slot == 0) $ do
      u <- randomIO :: IO Word64
      shares <- Secret.encode (n-t) n (Bin.encode u)
      for_ (zip shares [0..]) $ \(share, i) ->
        sendEveryone (MEntry (EUShare myId i (encrypt i share)))
      sendEveryone (MEntry (EUHash myId (hashlazy (Bin.encode u))))

    -- If we are the epoch leader, we should generate a block
    do leader <- withNodeState $ preuse (epochLeaders . ix epoch . ix slot)
       when (leader == Just myId) $
         createAndSendBlock

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

  -- This is our message handling function:
  return $ \n_from msg -> do
    case msg of

      -- An entry has been received: add it to the list of unprocessed entries
      MEntry e -> do
        withNodeState $ do
          pendingEntries %= Set.insert e

      -- A block has been received: remove all pending entries we have that
      -- are in this block, then add the block to our local blockchain and
      -- use info from the block
      MBlock es -> do
        withNodeState $ do
          pendingEntries %= (Set.\\ Set.fromList es)
          blocks %= (es:)
        -- TODO: using withNodeState several times here might break
        -- atomicity, I dunno
        for_ es $ \e -> case e of
          ELeaders epoch leaders -> do
            mbLeaders <- withNodeState $ use (epochLeaders . at epoch)
            case mbLeaders of
              Nothing -> withNodeState $
                           epochLeaders . at epoch .= Just leaders
              Just _  -> logError myId $
                printf "we already know leaders for epoch %d, \
                       \but we received a block with ELeaders for the \
                       \same epoch"
                  epoch
            withNodeState $ epochLeaders . at epoch .= Just leaders
          -- TODO: process other types of entries
          _ -> return ()

      -- We were pinged
      MPing -> logInfo myId (printf "received a ping from [%d]" n_from)

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

main :: IO ()
-- Here's how to run a simple system with two nodes pinging each other:
-- main = runNodes [node_ping 1, node_ping 0]
main = runNodes [fullNode, fullNode, fullNode]
