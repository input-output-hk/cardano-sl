{-# LANGUAGE ScopedTypeVariables #-}
-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication () where

import           Data.Binary.Get         (getInt32be, getWord8, label)
import           Data.Binary.Put         (putInt32be, putWord8)
import           Node.Message            (MessageName (..))
import           Universum

import           Pos.Binary.Class        (Bi (..))
import           Pos.Block.Network.Types (MsgBlock (..), MsgGetBlocks (..),
                                          MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Types (SysStartRequest (..), SysStartResponse (..),
                                          VersionReq (..), VersionResp (..))
import           Pos.Delegation.Types    (CheckProxySKConfirmed (..),
                                          CheckProxySKConfirmedRes (..),
                                          ConfirmProxySK (..), SendProxySK (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Txp.Types           (TxMsgTag (..))
import           Pos.Update.Types        (ProposalMsgTag (..), VoteMsgTag (..))

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

instance Bi (MsgGetHeaders ssc) where
    put (MsgGetHeaders f t) = put f >> put t
    get = MsgGetHeaders <$> get <*> get

instance Bi (MsgGetBlocks ssc) where
    put (MsgGetBlocks f t) = put f >> put t
    get = MsgGetBlocks <$> get <*> get

instance Ssc ssc => Bi (MsgHeaders ssc) where
    put (MsgHeaders b) = put b
    get = MsgHeaders <$> get

instance Ssc ssc => Bi (MsgBlock ssc) where
    put (MsgBlock b) = put b
    get = MsgBlock <$> get

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

instance Bi CheckProxySKConfirmed where
    put (CheckProxySKConfirmed pSk) = put pSk
    get = CheckProxySKConfirmed <$> get

instance Bi CheckProxySKConfirmedRes where
    put (CheckProxySKConfirmedRes res) = put res
    get = CheckProxySKConfirmedRes <$> get

----------------------------------------------------------------------------
-- Versioning
----------------------------------------------------------------------------

instance Bi VersionReq where
    put VersionReq = pass
    get = pure VersionReq

instance Bi VersionResp where
    put VersionResp{..} =  putInt32be vRespMagic
                        *> put vRespProtocolVersion
    get = label "GenericBlockHeader" $ VersionResp <$> getInt32be <*> get

----------------------------------------------------------------------------
-- Update system
----------------------------------------------------------------------------

instance Bi ProposalMsgTag where
    put ProposalMsgTag = pure ()
    get = pure ProposalMsgTag

instance Bi VoteMsgTag where
    put VoteMsgTag = pure ()
    get = pure VoteMsgTag
