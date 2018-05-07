{-# LANGUAGE TypeFamilies #-}

-- | Some types for json logging.
module Pos.Util.JsonLog
       ( JLEvent(..)
       , JLTxS (..)
       , JLTxR (..)
       , JLMemPool (..)
       , JLBlock (..)
       , JLTimedEvent(..)
       , jlCreatedBlock
       , jlAdoptedBlock
       , appendJL
       , fromJLSlotId
       , JsonLogConfig(..)
       , HasJsonLogConfig(..)
       , jsonLogConfigFromHandle
       , jsonLogDefault
       , fromJLSlotIdUnsafe
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Aeson (encode)
import           Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Lazy as LBS
import qualified Ether
import           Formatting (sformat)
import           JsonLog.JsonLogT (JsonLogConfig (..))
import qualified JsonLog.JsonLogT as JL
import           JsonLog.CanJsonLog (CanJsonLog)
import           Mockable (realTime)
import           System.Wlog (WithLogger)

import           Pos.Block.BHelpers ()
import           Pos.Core.Block (Block, mainBlockTxPayload)
import           Pos.Core.Block.Genesis (genBlockEpoch)
import           Pos.Core.Block.Main (mainBlockSlot)
import           Pos.Core (HasConfiguration, SlotId (..), gbHeader, gbhPrevBlock,
                           getSlotIndex, headerHash, mkLocalSlotIndex)
import           Pos.Crypto (hash, hashHexF)
import           Pos.Core.Txp (txpTxs)
import           Pos.Core (EpochIndex (..), HeaderHash, headerHashF)
import           Pos.Util.JsonLog.Events (JLEvent (..) , JLTxS (..) , JLTxR (..) , JLMemPool (..),
                                          JLBlock (..) , JLTimedEvent (..), JLSlotId)

-- | Get 'SlotId' from 'JLSlotId'.
fromJLSlotId :: (HasConfiguration, MonadError Text m) => JLSlotId -> m SlotId
fromJLSlotId (ep, sl) = SlotId (EpochIndex ep) <$> mkLocalSlotIndex sl

-- | Get 'SlotId' from 'JLSlotId'.
fromJLSlotIdUnsafe :: HasConfiguration => JLSlotId -> SlotId
fromJLSlotIdUnsafe x = case fromJLSlotId x of
    Right y -> y
    Left  _ -> error "illegal slot id"


-- | Return event of created block.
jlCreatedBlock :: HasConfiguration => Block -> JLEvent
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

-- | Append event into log by given 'FilePath'.
appendJL :: (MonadIO m) => FilePath -> JLEvent -> m ()
appendJL path ev = liftIO $ do
  time <- realTime -- TODO: Do we want to mock time in logs?
  LBS.appendFile path . encode $ JLTimedEvent (fromIntegral time) ev

-- | Returns event of created 'Block'.
jlAdoptedBlock :: HasConfiguration => Block -> JLEvent
jlAdoptedBlock = JLAdoptedBlock . showHeaderHash . headerHash

jsonLogConfigFromHandle :: MonadIO m => Handle -> m JsonLogConfig
jsonLogConfigFromHandle h = do
    v <- newMVar h
    return $ JsonLogConfig v (\_ -> return True)

class HasJsonLogConfig ctx where
    jsonLogConfig :: Lens' ctx JsonLogConfig

jsonLogDefault
    :: (ToJSON a, MonadReader ctx m, HasJsonLogConfig ctx, MonadCatch m,
        MonadIO m, WithLogger m)
    => a -> m ()
jsonLogDefault x = do
    jlc <- view jsonLogConfig
    JL.jsonLogDefault jlc x

deriving instance CanJsonLog (t m) => CanJsonLog (Ether.TaggedTrans tag t m)

-- Required for @Explorer@ @BListener@ and @ExtraContext@ redirect
deriving instance CanJsonLog m => CanJsonLog (Ether.TaggedTrans tag IdentityT m)
deriving instance CanJsonLog m => CanJsonLog (Ether.ReaderT tag r m)
