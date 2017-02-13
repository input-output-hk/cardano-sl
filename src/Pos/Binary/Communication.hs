{-# LANGUAGE ScopedTypeVariables #-}
-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication () where

import           Data.Binary.Get                  (getByteString, getWord8, label)
import           Data.Binary.Put                  (putByteString, putWord8)
import qualified Data.ByteString                  as BS
import           Data.Reflection                  (Reifies, reflect)
import           Formatting                       (int, sformat, (%))
import           Node.Message                     (MessageName (..))
import           Serokell.Data.Memory.Units       (Byte)
import           Universum                        hiding (putByteString)

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Block.Network.Types          (MsgBlock (..), MsgGetBlocks (..),
                                                   MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Types          (SysStartRequest (..),
                                                   SysStartResponse (..))
import           Pos.Communication.Types.Protocol (HandlerSpec (..), NOP (..),
                                                   PeerId (..), VerInfo (..))
import           Pos.Delegation.Types             (ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model.Types              (meaningPartLength)
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)
import           Pos.Ssc.Class.Types              (Ssc (..))
import           Pos.Txp.Types                    (TxMsgTag (..))
import           Pos.Update.Network.Types         (ProposalMsgTag (..), VoteMsgTag (..))
import           Pos.Util.Binary                  (getRemainingByteString, getWithLength,
                                                   getWithLengthLimited, putWithLength)

deriving instance Bi MessageName

instance Bi NOP where
    put _ = put (0 :: Word8)
    get = NOP <$ do
              (i :: Word8) <- get
              when (i /= 0) $
                 fail "NOP: 0 expected"

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

instance (SscHelpersClass ssc, Reifies s Byte) => Bi (MsgBlock s ssc) where
    -- We encode block size and then the block itself so that we'd be able to
    -- reject the block if it's of the wrong size without consuming the whole
    -- block.
    put (MsgBlock b) =
        -- NB: When serializing, we don't check that the size of the
        -- serialized block is smaller than the allowed size. Note that
        -- we *depend* on this behavior in e.g. 'handleGetBlocks' in
        -- "Pos.Block.Network.Listeners". Grep for #put_checkBlockSize.
        putWithLength (put b)
    get = label "MsgBlock" $ do
        let maxBlockSize = reflect (Proxy @s)
        getWithLengthLimited (fromIntegral maxBlockSize) (MsgBlock <$> get)

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

instance Bi HandlerSpec where
    put OneMsgHandler =
        putWord8 0 <>
        putWithLength (return ())
    put (ConvHandler m) =
        putWord8 1 <>
        putWithLength (put m)
    put (UnknownHandler t b) =
        putWord8 t <>
        putWithLength (putByteString b)
    get = label "HandlerSpec" $ getWord8 >>= \case
        0 -> getWithLength (pure OneMsgHandler)
        1 -> getWithLength (ConvHandler <$> get)
        t -> getWithLength (UnknownHandler t <$> getRemainingByteString)

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
                        then panic $ sformat ("Wrong PeerId length "%int) (BS.length b)
                        else putByteString b
    get = label "PeerId" $ PeerId <$> getByteString peerIdLength
