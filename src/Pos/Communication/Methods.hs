-- | Wrappers on top of communication methods.

module Pos.Communication.Methods
       ( announceBlock
       , announceTx
       , announceTxs
       , sendTx
       , announceCommitment
       , announceOpening
       , announceShares
       , announceVssCertificate
       ) where

import           Control.TimeWarp.Logging (logDebug)
import           Control.TimeWarp.Rpc     (Message, NetworkAddress)
import           Control.TimeWarp.Timed   (fork_)
import           Data.Binary              (Binary)
import           Data.List.NonEmpty       (NonEmpty ((:|)))
import           Formatting               (build, sformat, (%))
import           Serokell.Util.Text       (listBuilderJSON, mapJson)
import           Universum

import           Pos.Communication.Types  (SendBlockHeader (..), SendCommitment (..),
                                           SendOpening (..), SendShares (..), SendTx (..),
                                           SendTxs (..), SendVssCertificate (..))
import           Pos.Crypto               (PublicKey, Share)
import           Pos.DHT                  (sendToNeighbors, sendToNode)
import           Pos.Ssc.Class.Types      (SscTypes)
import           Pos.Statistics           (statlogSentBlockHeader, statlogSentTx)
import           Pos.Types                (MainBlockHeader, Opening, SignedCommitment, Tx,
                                           VssCertificate)
import           Pos.Util                 (logWarningWaitLinear, messageName')
import           Pos.WorkMode             (WorkMode)

sendToNeighborsSafe :: (Binary r, Message r, WorkMode m) => r -> m ()
sendToNeighborsSafe msg = do
    let msgName = messageName' msg
    let action = () <$ sendToNeighbors msg
    fork_ $
        logWarningWaitLinear 10 ("Sending " <> msgName <> " to neighbors") action

-- | Announce new block to all known peers. Intended to be used when
-- block is created.
announceBlock
    :: (SscTypes ssc, WorkMode m)
    => MainBlockHeader ssc -> m ()
announceBlock header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    statlogSentBlockHeader $ Right header
    sendToNeighborsSafe . SendBlockHeader $ header

-- | Announce new transaction to all known peers. Intended to be used when
-- tx is created.
announceTx :: WorkMode m => Tx -> m ()
announceTx tx = do
    logDebug $ sformat ("Announcing tx to others:\n"%build) tx
    statlogSentTx tx
    sendToNeighborsSafe . SendTx $ tx

-- | Announce known transactions to all known peers. Intended to be used
-- to relay transactions.
announceTxs :: WorkMode m => [Tx] -> m ()
announceTxs [] = pure ()
announceTxs txs@(tx:txs') = do
    logDebug $
        sformat ("Announcing txs to others:\n" %build) $ listBuilderJSON txs
    mapM_ statlogSentTx txs
    sendToNeighborsSafe . SendTxs $ tx :| txs'

-- | Send Tx to given address.
sendTx :: WorkMode m => NetworkAddress -> Tx -> m ()
sendTx addr = sendToNode addr . SendTx

----------------------------------------------------------------------------
-- Relaying MPC messages
----------------------------------------------------------------------------

-- TODO: add statlogging for everything, see e.g. announceTxs

announceCommitment :: WorkMode m => PublicKey -> SignedCommitment -> m ()
announceCommitment pk comm = do
    -- TODO: show the commitment
    logDebug $ sformat
        ("Announcing "%build%"'s commitment to others: <TODO SHOW COMM>") pk
    sendToNeighborsSafe $ SendCommitment pk comm

announceOpening :: WorkMode m => PublicKey -> Opening -> m ()
announceOpening pk open = do
    logDebug $ sformat
        ("Announcing "%build%"'s opening to others: "%build) pk open
    sendToNeighborsSafe $ SendOpening pk open

announceShares :: WorkMode m => PublicKey -> HashMap PublicKey Share -> m ()
announceShares pk shares = do
    logDebug $ sformat
        ("Announcing "%build%"'s shares to others:\n"%mapJson) pk shares
    sendToNeighborsSafe $ SendShares pk shares

announceVssCertificate :: WorkMode m => PublicKey -> VssCertificate -> m ()
announceVssCertificate pk cert = do
    -- TODO: show the certificate
    logDebug $ sformat
        ("Announcing "%build%"'s VSS certificate to others: <TODO SHOW CERT>") pk
    void . sendToNeighbors $ SendVssCertificate pk cert

----------------------------------------------------------------------------
-- Legacy
--
----------------------------------------------------------------------------

-- {- |
-- Run something at the beginning of every slot. The first parameter is epoch
-- number (starting from 0) and the second parameter is slot number in the epoch
-- (from 0 to epochLen-1).

-- The 'Bool' parameter says whether a delay should be introduced. It's useful
-- for nodes (so that node logging messages would come after “EPOCH n” logging
-- messages).
-- -}
-- inSlot :: WorkMode m => Bool -> (Int -> Int -> m ()) -> m ()
-- inSlot extraDelay f = fork_ $ do
--     start <- liftIO $ readIORef systemStart
--     let getAbsoluteSlot :: WorkMode m => m Int
--         getAbsoluteSlot = do
--             now <- virtualTime
--             return (div' (now - start) slotDuration)
--     -- Wait until the next slot begins
--     nextSlotStart <- do
--         absoluteSlot <- getAbsoluteSlot
--         return (start + fromIntegral (absoluteSlot + 1) * slotDuration)
--     -- Now that we're synchronised with slots, start repeating
--     -- forever. 'repeatForever' has slight precision problems, so we delay
--     -- everything by 50ms.
--     wait (till nextSlotStart)
--     repeatForever slotDuration handler $ do
--         wait (for 50 ms)
--         when extraDelay $ wait (for 50 ms)
--         absoluteSlot <- getAbsoluteSlot
--         let (epoch, slot) = absoluteSlot `divMod` epochSlots
--         f epoch slot
--   where
--     handler e = do
--         logError $ sformat
--             ("error was caught, restarting in 5 seconds: "%build) e
--         return $ sec 5

-- {- ==================== TODO ====================

-- Timing issues
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- * What to do about blocks delivered a bit late? E.g. let's assume that a
--   block was generated in slot X, but received by another node in slot Y. What
--   are the conditions on Y under which the block should (and shouldn't) be
--   accepted?

-- * Let's say that we receive a transaction, and then we receive a block
--   containing that transaction. We remove the transaction from our list of
--   pending transactions. Later (before K slots pass) it turns out that that
--   block was bad, and we discard it; then we should add the transaction
--   back. Right? If this is how it works, then it means that somebody can
--   prevent the transaction from being included into the blockchain for the
--   duration of K−1 slots – right? How easy/probable/important is it in
--   practice?

-- Other issues
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- * We should exclude extremely delayed entries that are the same as ones we
--   already received before, but already included into one of the previous
--   blocks.

-- -}

-- {-
-- If some node becomes inactive, other nodes will be able to recover its U by
-- exchanging decrypted pieces of secret-shared U they've been sent.

-- After K slots all nodes are guaranteed to have a common prefix; each node
-- computes the random satoshi index from all available Us to find out who has
-- won the leader election and can generate the next block.
-- -}

-- -- | Set up logger, open state.
-- fullNodeWrapper :: WorkMode m => (NodeState -> Node m) -> Node m
-- fullNodeWrapper nf =
--     \self key n pkeys sendTo ->
--         setLoggerName (LoggerName (toS (sformat nodeF self))) $ do
--             st <- openMemState
--             nf st self key n pkeys sendTo

-- fullNode :: WorkMode m => Node m
-- fullNode = fullNodeWrapper $ \_ _ _ _ _ _ -> do
--     This will run at the beginning of each slot:
--     inSlot True $ \epoch slot -> do
--         -- For now we just send messages to everyone instead of letting them
--         -- propagate, implementing peers, etc.
--         let sendEveryone x = for_ [NodeId 0 .. NodeId (n - 1)] $ \i ->
--                                  sendTo i x

--         -- Create a block and send it to everyone
--         let createAndSendBlock = do
--                 blk <- update st CreateBlock
--                 sendEveryone (MBlock blk)
--                 if null blk then
--                     logInfo "created an empty block"
--                 else
--                     logInfo $ T.intercalate "\n" $
--                         "created a block:" :
--                         map (\e -> "  * " <> displayEntry e) blk

--         -- If this is the first epoch ever, we haven't agreed on who will
--         -- mine blocks in this epoch, so let's just say that the 0th node is
--         -- the master node. In slot 0, node 0 will announce who will mine
--         -- blocks in the next epoch; in other slots it will just mine new
--         -- blocks.
--         when (self == NodeId 0 && epoch == 0) $ do
--             when (slot == 0) $ do
--                 leaders <- liftIO $ map NodeId <$>
--                            replicateM epochSlots (randomRIO (0, n - 1))
--                 update st $ AddLeaders epoch leaders
--                 logInfo "generated random leaders for epoch 1 \
--                         \(as master node)"
--             createAndSendBlock

--         -- When the epoch starts, we do the following:
--         --   * generate U, a random bitvector that will be used as a seed to
--         --     the PRNG that will choose leaders (nodes who will mine each
--         --     block in the next epoch). For now the seed is actually just a
--         --     Word64.
--         --   * secret-share U and encrypt each piece with corresponding
--         --     node's pubkey; the secret can be recovered with at least
--         --     N−T available pieces
--         --   * post encrypted shares and a commitment to U to the blockchain
--         --     (so that later on we wouldn't be able to cheat by using
--         --     a different U)
--         when (slot == 0) $ do
--             -- u <- liftIO (randomIO :: IO Word64)
--             return ()
--             -- let pk = VssPublicKey ()
--             -- let (_, shares) = shareSecret (replicate n pk) t (Secret $ toS (Bin.encode u))
--             -- for_ (zip shares [NodeId 0..]) $ \(share, i) -> do
--             --     encShare <- pure share
--             --     sendEveryone (MEntry (EUShare self i encShare))
--             -- sendEveryone (MEntry $ EUHash self $ hashRaw $ toS $ Bin.encode u)

--         -- If we are the epoch leader, we should generate a block
--         do leader <- query st $ GetLeader epoch slot
--            when (leader == Just self) $
--                createAndSendBlock
