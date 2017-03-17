{-# LANGUAGE ScopedTypeVariables #-}
-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication () where

import           Universum

import           Data.Binary.Get                  (getByteString, getWord8, label)
import           Data.Binary.Put                  (putByteString, putWord8)
import           Data.Bits                        (Bits (..))
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import           Formatting                       (int, sformat, (%))
import           Node.Message                     (MessageName (..))

import           Pos.Binary.Class                 (Bi (..), UnsignedVarInt (..),
                                                   decodeFull, encodeStrict,
                                                   getRemainingByteString,
                                                   getSmallWithLength, getWithLength,
                                                   putSmallWithLength, putWithLength)
import           Pos.Block.Network.Types          (MsgBlock (..), MsgGetBlocks (..),
                                                   MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Types          (SysStartRequest (..),
                                                   SysStartResponse (..))
import           Pos.Communication.Types.Protocol (HandlerSpec (..), PeerId (..),
                                                   VerInfo (..))
import           Pos.Delegation.Types             (ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model.Types              (meaningPartLength)
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)
import           Pos.Ssc.Class.Types              (Ssc (..))
import           Pos.Txp.Network.Types            (TxMsgTag (..))
import           Pos.Update.Network.Types         (ProposalMsgTag (..), VoteMsgTag (..))


deriving instance Bi MessageName

----------------------------------------------------------------------------
-- System start
----------------------------------------------------------------------------

instance Bi SysStartRequest where
    put _ = put (1 :: Word8)
    get = label "SysStartRequest" $ do
        (i :: Word8) <- get
        when (i /= 1) $
           fail "SysStartRequest: 1 expected"
        return SysStartRequest

instance Bi SysStartResponse where
    put (SysStartResponse t) = put t
    get = label "SysStartResponse" $ SysStartResponse <$> get

----------------------------------------------------------------------------
-- Blocks
----------------------------------------------------------------------------

instance Bi MsgGetHeaders where
    put (MsgGetHeaders f t) = put f >> put t
    get = label "MsgGetHeaders" $ MsgGetHeaders <$> get <*> get

instance Bi MsgGetBlocks where
    put (MsgGetBlocks f t) = put f >> put t
    get = label "MsgGetBlocks" $ MsgGetBlocks <$> get <*> get

instance Ssc ssc => Bi (MsgHeaders ssc) where
    put (MsgHeaders b) = put b
    get = label "MsgHeaders" $ MsgHeaders <$> get

instance SscHelpersClass ssc => Bi (MsgBlock ssc) where
    -- We encode block size and then the block itself so that we'd be able to
    -- reject the block if it's of the wrong size without consuming the whole
    -- block.
    put (MsgBlock b) =
        -- NB: When serializing, we don't check that the size of the
        -- serialized block is smaller than the allowed size. Note that
        -- we *depend* on this behavior in e.g. 'handleGetBlocks' in
        -- "Pos.Block.Network.Listeners". Grep for #put_checkBlockSize.
        putWithLength (put b)
    get = label "MsgBlock" $ getWithLength $ MsgBlock <$> get

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
    put (SendProxySKLight pSk) = putWord8 0 >> put pSk
    put (SendProxySKHeavy pSk) = putWord8 1 >> put pSk
    get = label "SendProxySK" $ getWord8 >>= \case
        0 -> SendProxySKLight <$> get
        1 -> SendProxySKHeavy <$> get
        t -> fail $ "get@SendProxySK: unknown tag " <> show t

instance Bi ConfirmProxySK where
    put (ConfirmProxySK pSk proof) = put pSk >> put proof
    get = label "ConfirmProxySK" $ liftA2 ConfirmProxySK get get

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

-- Encoding of HandlerSpec is as follow:
--
-- | Type                                        | Size     | Value     | Following data |
-- |---------------------------------------------|----------|-----------|----------------|
-- | OneMessageHandler                           | Fixed    | 0000 0000 | none           |
-- | ConvHandler m where m : UnsignedVarInt < 64 | Fixed    | 01xx xxxx | none           |
-- | ConvHandler m where m : Unknown             | Variable | 0000 0001 | EncodeString   |
-- | UnknownHandler w8 bs                        | Variable | w8        | bs             |
instance Bi HandlerSpec where
    put OneMsgHandler = putWord8 0
    put (ConvHandler (MessageName m)) =
        case decodeFull $ BSL.fromStrict m of
            Right (UnsignedVarInt a)
                | a < 64 -> putWord8 (0x40 .|. (fromIntegral (a :: Word) .&. 0x3f))
            _ -> putWord8 1 >> putSmallWithLength (put m)
    put (UnknownHandler t b) =
        putWord8 t >> putSmallWithLength (putByteString b)
    get = label "HandlerSpec" $ getWord8 >>= \case
        0                        -> pure OneMsgHandler
        1                        -> getSmallWithLength (ConvHandler <$> get)
        t | (t .&. 0xc0) == 0x40 ->
            pure . ConvHandler . MessageName . encodeStrict $
            UnsignedVarInt (fromIntegral (t .&. 0x3f) :: Word)
          | otherwise            ->
            getSmallWithLength (UnknownHandler t <$> getRemainingByteString)

instance Bi VerInfo where
    put VerInfo {..} = put vIMagic
                    <> put vIBlockVersion
                    <> put vIInHandlers
                    <> put vIOutHandlers
    get = label "VerInfo" $ VerInfo <$> get <*> get <*> get <*> get

peerIdLength :: Int
peerIdLength = meaningPartLength

instance Bi PeerId where
    put (PeerId b) = if BS.length b /= peerIdLength
                        then error $ sformat ("Wrong PeerId length "%int) (BS.length b)
                        else putByteString b
    get = label "PeerId" $ PeerId <$> getByteString peerIdLength
