{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main where


import           Control.Lens                  (at, ix, makeLenses, preuse, use, (%=),
                                                (.=), (<<.=))
import           Crypto.Hash                   (Digest, SHA256, hashlazy)
import qualified Crypto.SecretSharing.Internal as Secret (ByteShare (..), Share (..),
                                                          encode)
import qualified Data.Binary                   as Bin (encode)
import           Data.Default                  (Default, def)
import           Data.Fixed                    (div', mod')
import           Data.IORef                    (IORef, atomicModifyIORef', newIORef,
                                                readIORef, writeIORef)
import qualified Data.Set                      as Set (fromList, insert, toList, (\\))
import           Data.Time                     (UTCTime, addUTCTime, diffUTCTime,
                                                getCurrentTime)
import           Formatting                    (Format, int, sformat, shown, stext, (%))
import qualified Prelude
import           Protolude                     hiding ((%))
import qualified SlaveThread                   as Slave (fork)
import           System.IO.Unsafe              (unsafePerformIO)
import           System.Random                 (randomIO, randomRIO)
import           Unsafe                        (unsafeIndex)


----------------------------------------------------------------------------
-- Utility types
----------------------------------------------------------------------------

type Hash = Digest SHA256

newtype NodeId = NodeId {getNodeId :: Int}
    deriving (Eq, Ord, Enum)

instance Prelude.Show NodeId where
    show (NodeId x) = "#" ++ show x

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

node :: Format r (NodeId -> r)
node = shown

logChan :: Chan Text
logChan = unsafePerformIO newChan
{-# NOINLINE logChan #-}

runLogPrinting :: IO ()
runLogPrinting = void $ Slave.fork $
    forever (putStrLn =<< readChan logChan)

logInfo :: NodeId -> Text -> IO ()
logInfo nid s = logRaw (sformat (node%" "%stext) nid s)

logError :: NodeId -> Text -> IO ()
logError nid s = logRaw (sformat (node%" ERROR: "%stext) nid s)

logRaw :: Text -> IO ()
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
    txInHash  :: Hash,       -- ^ Which transaction's output is used
    txInIndex :: Int }       -- ^ Index of the output in transaction's outputs
    deriving (Eq, Ord, Show)

-- | Transaction output
data TxOut = TxOut {
    txOutValue :: Word64 }   -- ^ Output value
    deriving (Eq, Ord, Show)

-- | Transaction
data Tx = Tx {
    txInputs  :: [TxIn],
    txOutputs :: [TxOut],
    txHash    :: Hash }      -- ^ Hash of the transaction
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

displayEntry :: Entry -> Text
displayEntry (ETx tx) =
    "transaction " <> show tx
displayEntry (EUHash nid h) =
    sformat (node%"'s commitment = "%shown) nid h
displayEntry (EUShare n_from n_to share) =
    sformat (node%"'s share for "%node%" = "%shown) n_from n_to share
displayEntry (ELeaders epoch leaders) =
    sformat ("leaders for epoch "%int%" = "%shown) epoch leaders

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

displayMessage :: Message -> Text
displayMessage MPing       = "ping"
displayMessage (MEntry e)  = displayEntry e
displayMessage (MBlock es) = sformat ("block with "%int%" entries") (length es)

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

node_ping :: NodeId -> Node
node_ping pingNum = \myNum sendTo -> do
    void $ Slave.fork $ inSlot $ \_epoch _slot -> do
        logInfo myNum (sformat ("pinging "%node) pingNum)
        sendTo pingNum MPing
    return $ \n_from message -> do
        case message of
            MPing -> do
                logInfo myNum (sformat ("pinged by "%node) n_from)
            _ -> do
                logInfo myNum (sformat ("unknown message from "%node) n_from)

runNodes :: [Node] -> IO ()
runNodes nodes = do
    -- The system shall start working in a bit of time (not exactly right now
    -- – due to the way inSlot implemented, it'd be nice to wait a bit)
    writeIORef systemStart . (addUTCTime (slotDuration/2)) =<< getCurrentTime
    tid <- Slave.fork $ do
        runLogPrinting
        void $ Slave.fork $ inSlot' $ \epoch slot -> do
            when (slot == 0) $
                logRaw (sformat ("====== EPOCH "%int%" ======") epoch)
            logRaw (sformat ("--- slot "%int%" ---") slot)
        rec nodeCallbacks <-
                let send n_from n_to message = do
                        let f = unsafeIndex nodeCallbacks (getNodeId n_to)
                        f n_from message
                in  sequence [nodeFun nid (send nid)
                               | (i, nodeFun) <- zip [0..] nodes
                               , let nid = NodeId i]
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
        let untilNext = slotDuration
                      - mod' (diffUTCTime now start) slotDuration
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
    _epochLeaders   :: Map Int [NodeId],
    -- | Blocks
    _blocks         :: [Block] }

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
        let sendEveryone x = for_ [NodeId 0 .. NodeId (n-1)] $ \i ->
                                 sendTo i x

        -- Create a block and send it to everyone
        let createAndSendBlock = do
                blk <- createBlock
                sendEveryone (MBlock blk)
                if null blk then
                    logInfo myId "created an empty block"
                else do
                    logInfo myId "created a block:"
                    for_ blk $ \e -> logInfo myId ("  * " <> displayEntry e)

        -- If this is the first epoch ever, we haven't agreed on who will
        -- mine blocks in this epoch, so let's just say that the 0th node is
        -- the master node. In slot 0, node 0 will announce who will mine
        -- blocks in the next epoch; in other slots it will just mine new
        -- blocks.
        when (myId == NodeId 0 && epoch == 0) $ do
            when (slot == 0) $ do
                leaders <- map NodeId <$>
                           replicateM epochSlots (randomRIO (0, n-1))
                withNodeState $ do
                    pendingEntries %= Set.insert (ELeaders (epoch+1) leaders)
                logInfo myId "generated random leaders for epoch 1 \
                             \(as master node)"
            createAndSendBlock

        -- When the epoch starts, we do the following:
        --   * generate U, a random bitvector that will be used as a seed to
        --     the PRNG that will choose leaders (nodes who will mine each
        --     block in the next epoch). For now the seed is actually just a
        --     Word64.
        --   * secret-share U and encrypt each piece with corresponding
        --     node's pubkey; the secret can be recovered with at least
        --     N−T available pieces
        --   * post encrypted shares and a commitment to U to the blockchain
        --     (so that later on we wouldn't be able to cheat by using
        --     a different U)
        when (slot == 0) $ do
            u <- randomIO :: IO Word64
            shares <- Secret.encode (n-t) n (Bin.encode u)
            for_ (zip shares [NodeId 0..]) $ \(share, i) ->
                sendEveryone (MEntry (EUShare myId i (encrypt i share)))
            sendEveryone (MEntry (EUHash myId (hashlazy (Bin.encode u))))

        -- If we are the epoch leader, we should generate a block
        do leader <- withNodeState $
                       preuse (epochLeaders . ix epoch . ix slot)
           when (leader == Just myId) $
               createAndSendBlock

        -- According to @gromak (who isn't sure about this, but neither am I):
        -- https://input-output-rnd.slack.com/archives/paper-pos/p1474991379000006
        --
        -- > We send commitments during the first slot and they are put into
        -- the first block. Then we wait for K periods so that all nodes
        -- agree upon the same first block. But we see that it’s not enough
        -- because they can agree upon dishonest block. That’s why we need to
        -- wait for K more blocks. So all this *commitment* phase takes 2K
        -- blocks.

    -- This is our message handling function:
    return $ \n_from message -> case message of
        -- An entry has been received: add it to the list of unprocessed
        -- entries
        MEntry e -> do
            withNodeState $ do
                pendingEntries %= Set.insert e

        -- A block has been received: remove all pending entries we have
        -- that are in this block, then add the block to our local
        -- blockchain and use info from the block
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
                          "we already know leaders for epoch " <> show epoch
                          <> "but we received a block with ELeaders "
                          <> "for the same epoch"
                    withNodeState $ epochLeaders . at epoch .= Just leaders
                -- TODO: process other types of entries
                _ -> return ()

        -- We were pinged
        MPing -> logInfo myId (sformat ("received a ping from "%node) n_from)

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

main :: IO ()
-- Here's how to run a simple system with two nodes pinging each other:
-- main = runNodes [node_ping 1, node_ping 0]
main = runNodes [fullNode, fullNode, fullNode]
