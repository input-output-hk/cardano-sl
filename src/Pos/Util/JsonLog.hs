{-# LANGUAGE TypeFamilies #-}

-- | Some types for json logging.
module Pos.Util.JsonLog
       ( JLEvent(..)
       , JLTxS (..)
       , JLTxR (..)
       , JLMemPool (..)
       , JLBlock (..)
       , jlCreatedBlock
       , jlAdoptedBlock
       , fromJLSlotId
       , JsonLogConfig(..)
       , HasJsonLogConfig(..)
       , jsonLogConfigFromHandle
       , jsonLogDefault
       , fromJLSlotIdUnsafe
       ) where

import           Universum                     hiding (catchAll)

import           Control.Monad.Except          (MonadError)
import           Data.Aeson.TH                 (deriveJSON)
import           Data.Aeson.Types              (ToJSON)
import           Formatting                    (sformat)
import           JsonLog.JsonLogT              (JsonLogConfig (..))
import qualified JsonLog.JsonLogT              as JL
import           Mockable.Class                (Mockable (..))
import           Mockable.Exception            (Catch)
import           Pos.Binary.Block              ()
import           Pos.Binary.Core               ()
import           Pos.Block.Core                (BiSsc, Block, mainBlockTxPayload)
import           Pos.Block.Core.Genesis.Lens   (genBlockEpoch)
import           Pos.Block.Core.Main.Lens      (mainBlockSlot)
import           Pos.Communication.Relay.Logic (InvReqDataFlowLog)
import           Pos.Core                      (SlotId (..), gbHeader, gbhPrevBlock,
                                                getSlotIndex, headerHash,
                                                mkLocalSlotIndex)
import           Pos.Crypto                    (hash, hashHexF)
import           Pos.Ssc.Class.Helpers         (SscHelpersClass)
import           Pos.Txp.Core                  (txpTxs)
import           Pos.Txp.MemState.Types        (MemPoolModifyReason)
import           Pos.Types                     (EpochIndex (..), HeaderHash, headerHashF)
import           Serokell.Aeson.Options        (defaultOptions)
import           System.Wlog.CanLog            (WithLogger)

type BlockId = Text
type TxId = Text
type JLSlotId = (Word64, Word16)

-- | Json log of one block with corresponding 'BlockId'.
data JLBlock = JLBlock
    { jlHash      :: BlockId
    , jlPrevBlock :: BlockId
    , jlTxs       :: [TxId]
    , jlSlot      :: JLSlotId
    } deriving Show

-- | Json log of one transaction sent from the (light) wallet.
data JLTxS = JLTxS
    { jlsNodeId :: Text
    , jlsTxId   :: Text
    , jlsInvReq :: InvReqDataFlowLog
    } deriving Show

-- | Json log of one transaction being received by a node.
data JLTxR = JLTxR
    { jlrTxId  :: Text
    , jlrError :: Maybe Text
    } deriving Show

-- | Get 'SlotId' from 'JLSlotId'.
fromJLSlotId :: MonadError Text m => JLSlotId -> m SlotId
fromJLSlotId (ep, sl) = SlotId (EpochIndex ep) <$> mkLocalSlotIndex sl

-- | Get 'SlotId' from 'JLSlotId'.
fromJLSlotIdUnsafe :: JLSlotId -> SlotId
fromJLSlotIdUnsafe x = case fromJLSlotId x of
    Right y -> y
    Left  _ -> error "illegal slot id"

-- | Json log of one mempool modification.
data JLMemPool = JLMemPool
    { -- | Reason for modifying the mempool
      jlmReason      :: MemPoolModifyReason
      -- | Queue length when trying to modify the mempool (not including this
      --   modifier, so it could be 0).
    , jlmQueueLength :: Int
      -- | Time spent waiting for the lock (microseconds)
    , jlmWait        :: Integer
      -- | Time spent doing the modification (microseconds, while holding the lock).
    , jlmModify      :: Integer
      -- | Size of the mempool before the modification.
    , jlmSizeBefore  :: Int
      -- | Size of the mempool after the modification.
    , jlmSizeAfter   :: Int
      -- | How much memory was allocated during the modification.
    , jlmAllocated   :: Int
    } deriving Show

-- | Json log event.
data JLEvent = JLCreatedBlock JLBlock
             | JLAdoptedBlock BlockId
             | JLTpsStat Int
             | JLTxSent JLTxS
             | JLTxReceived JLTxR
             | JLMemPoolEvent JLMemPool
  deriving (Show, Generic)

$(deriveJSON defaultOptions ''JLBlock)
$(deriveJSON defaultOptions ''JLEvent)
$(deriveJSON defaultOptions ''JLTxS)
$(deriveJSON defaultOptions ''JLTxR)
$(deriveJSON defaultOptions ''JLMemPool)

-- | Return event of created block.
jlCreatedBlock :: BiSsc ssc => Block ssc -> JLEvent
jlCreatedBlock block = JLCreatedBlock $ JLBlock {..}
  where
    jlHash = showHeaderHash $ headerHash block
    jlPrevBlock = showHeaderHash $ case block of
        Left  gB -> view gbhPrevBlock (gB ^. gbHeader)
        Right mB -> view gbhPrevBlock (mB ^. gbHeader)
    jlSlot = (getEpochIndex $ siEpoch slot, getSlotIndex $ siSlot slot)
    jlTxs = case block of
              Left _   -> []
              Right mB -> map fromTx . toList $ mB ^. mainBlockTxPayload . txpTxs
    slot :: SlotId
    slot = case block of
        Left  gB -> let slotZero = case mkLocalSlotIndex 0 of
                                        Right sz -> sz
                                        Left _   -> error "impossible branch"
                    in SlotId (gB ^. genBlockEpoch) slotZero
        Right mB -> mB ^. mainBlockSlot
    fromTx = sformat hashHexF . hash

showHeaderHash :: HeaderHash -> Text
showHeaderHash = sformat headerHashF

-- | Returns event of created 'Block'.
jlAdoptedBlock :: SscHelpersClass ssc => Block ssc -> JLEvent
jlAdoptedBlock = JLAdoptedBlock . showHeaderHash . headerHash

jsonLogConfigFromHandle :: MonadIO m => Handle -> m JsonLogConfig
jsonLogConfigFromHandle h = do
    v <- newMVar h
    return $ JsonLogConfig v (\_ -> return True)

class HasJsonLogConfig ctx where
    jsonLogConfig :: Lens' ctx JsonLogConfig

jsonLogDefault
    :: (ToJSON a, MonadReader ctx m, HasJsonLogConfig ctx, Mockable Catch m,
        MonadIO m, WithLogger m)
    => a -> m ()
jsonLogDefault x = do
    jlc <- view jsonLogConfig
    JL.jsonLogDefault jlc x
