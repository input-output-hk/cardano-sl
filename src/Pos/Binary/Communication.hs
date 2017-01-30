{-# LANGUAGE ScopedTypeVariables #-}
-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication () where

import           Data.Binary.Get                  (getByteString, getWord8, isolate,
                                                   label)
import           Data.Binary.Put                  (putByteString, putLazyByteString,
                                                   putWord8, runPut)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import           Data.Reflection                  (Reifies, reflect)
import           Formatting                       (formatToString, int, sformat, (%))
import           Node.Message                     (MessageName (..))
import           Serokell.Data.Memory.Units       (Byte)
import           Universum                        hiding (putByteString)

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Block.Network.Types          (MsgBlock (..), MsgGetBlocks (..),
                                                   MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Types          (SysStartRequest (..),
                                                   SysStartResponse (..))
import           Pos.Communication.Types.Protocol (HandlerSpec (..), PeerId (..),
                                                   VerInfo (..))
import           Pos.Delegation.Types             (ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model.Types              (meaningPartLength)
import           Pos.Ssc.Class.Types              (Ssc (..))
import           Pos.Txp.Types                    (TxMsgTag (..))
import           Pos.Update.Network.Types         (ProposalMsgTag (..), VoteMsgTag (..))

deriving instance Bi MessageName

----------------------------------------------------------------------------
-- System start
----------------------------------------------------------------------------

instance Bi SysStartRequest where
    put _ = put (0 :: Word8)
    get = SysStartRequest <$ do
              (i :: Word8) <- get
              when (i /= 0) $
                 fail "SysStartRequest: 0 expected"

instance Bi SysStartResponse where
    put (SysStartResponse t) = put t
    get = SysStartResponse <$> get

----------------------------------------------------------------------------
-- Blocks
----------------------------------------------------------------------------

instance Bi MsgGetHeaders where
    put (MsgGetHeaders f t) = put f >> put t
    get = MsgGetHeaders <$> get <*> get

instance Bi MsgGetBlocks where
    put (MsgGetBlocks f t) = put f >> put t
    get = MsgGetBlocks <$> get <*> get

instance Ssc ssc => Bi (MsgHeaders ssc) where
    put (MsgHeaders b) = put b
    get = MsgHeaders <$> get

instance (Ssc ssc, Reifies s Byte) => Bi (MsgBlock s ssc) where
    -- We encode block size and then the block itself so that we'd be able to
    -- reject the block if it's of the wrong size without consuming the whole
    -- block. Unfortunately, @binary@ doesn't provide a method to limit byte
    -- consumption – only a method to ensure that the /exact/ number of bytes
    -- is consumed. Thus we need to know the actual block size in advance.
    put (MsgBlock b) = do
        -- NB: When serializing, we don't check that the size of the
        -- serialized block is smaller than the allowed size. Note that
        -- we *depend* on this behavior in e.g. 'handleGetBlocks' in
        -- "Pos.Block.Network.Listeners". Grep for #put_checkBlockSize.
        let serialized = runPut (put b)
        put (BSL.length serialized :: Int64)
        putLazyByteString serialized
    get = do
        blockSize :: Int64 <- get
        let maxBlockSize = reflect (Proxy @s)
        if fromIntegral blockSize <= maxBlockSize
            -- TODO: this will fail on 32-bit machines if we have blocks
            -- bigger than 2 GB, because 'isolate' takes 'Int' and not
            -- 'Int64'. I don't think we'll have blocks that big any soon
            -- (famous last words...). Anyway, if you want to fix it, you can
            -- copy the code of 'isolate' and fix the types there.
            then isolate (fromIntegral blockSize) (MsgBlock <$> get)
            else fail $ formatToString
                     ("get@MsgBlock: block ("%int%" bytes) is bigger "%
                      "than maxBlockSize ("%int%" bytes)")
                     blockSize maxBlockSize

----------------------------------------------------------------------------
-- Transaction processing
----------------------------------------------------------------------------

instance Bi TxMsgTag where
    put TxMsgTag = pure ()
    get = pure TxMsgTag

----------------------------------------------------------------------------
-- Delegation/PSK
----------------------------------------------------------------------------

instance Bi SendProxySK where
    put (SendProxySKEpoch pSk)  = putWord8 0 >> put pSk
    put (SendProxySKSimple pSk) = putWord8 1 >> put pSk
    get = label "SendProxySK" $ getWord8 >>= \case
        0 -> SendProxySKEpoch <$> get
        1 -> SendProxySKSimple <$> get
        t -> fail $ "get@SendProxySK: unknown tag " <> show t

instance Bi ConfirmProxySK where
    put (ConfirmProxySK pSk proof) = put pSk >> put proof
    get = liftA2 ConfirmProxySK get get

--instance Bi CheckProxySKConfirmed where
--    put (CheckProxySKConfirmed pSk) = put pSk
--    get = CheckProxySKConfirmed <$> get
--
--instance Bi CheckProxySKConfirmedRes where
--    put (CheckProxySKConfirmedRes res) = put res
--    get = CheckProxySKConfirmedRes <$> get
--
----------------------------------------------------------------------------
-- Update system
----------------------------------------------------------------------------

instance Bi ProposalMsgTag where
    put ProposalMsgTag = pure ()
    get = pure ProposalMsgTag

instance Bi VoteMsgTag where
    put VoteMsgTag = pure ()
    get = pure VoteMsgTag

instance Bi HandlerSpec where
    put OneMsgHandler   = putWord8 0
    put (ConvHandler m) = putWord8 1 <> put m
    get = getWord8 >>= getImpl
      where
        getImpl 0 = pure OneMsgHandler
        getImpl 1 = ConvHandler <$> get
        getImpl _ = fail "Unknown HandlerSpec type"

instance Bi VerInfo where
    put VerInfo {..} = put vIMagic
                    <> put vIBlockVersion
                    <> put vIInHandlers
                    <> put vIOutHandlers
    get = VerInfo <$> get <*> get <*> get <*> get

peerIdLength :: Int
peerIdLength = meaningPartLength

instance Bi PeerId where
    put (PeerId b) = if BS.length b /= peerIdLength
                        then panic $ sformat ("Wrong PeerId length "%int) (BS.length b)
                        else putByteString b
    get = PeerId <$> getByteString peerIdLength
