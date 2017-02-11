{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Communication.Limits
    ( MessageLimited (..)
    , LimitedLengthExt (..)
    , Limiter (..)
    , LimitedLength
    ) where

import           Control.Lens                     (each, ix)
import           Data.Binary                      (Get)
import           Data.Binary.Get                  (lookAhead, getWord8)
import           Data.Proxy                       (Proxy (..))
import           Data.Reflection                  (Reifies, reflect)
import           Node.Message                     (Message)
import           Serokell.Data.Memory.Units       (Byte)
import           Universum

import           Pos.Binary.Class                 (Bi (..))
import qualified Pos.Binary.Class                 as Bi
import           Pos.Block.Network.Types          (MsgBlock)
import           Pos.Constants                    (genesisMaxTxSize, genesisMaxReqSize,
                                                   genesisMaxInvSize)
import           Pos.Communication.Types.Relay    (DataMsg, InvMsg, ReqMsg)
import           Pos.DB.Class                     (MonadDB)
import qualified Pos.DB.GState                    as GState
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents)
import           Pos.Txp.Types.Communication      (TxMsgContents)
import           Pos.Update.Core.Types            (UpdateProposal, UpdateVote)

-- | Specifies type of limit on incoming message size.
-- Useful when the type has several limits and choice depends on constructor.
class Limiter l where
    limitGet :: l -> Get a -> Get a

instance Limiter Byte where
    limitGet = Bi.limitGet . fromIntegral

-- | Limit depends on value of first byte, which should be in range @0..3@.
instance Limiter (Byte, Byte, Byte, Byte) where
    limitGet limits parser = do
        tag <- fromIntegral <$> lookAhead getWord8
        case (limits ^.. each) ^? ix tag of
            Nothing -> -- Such limiter is used in `DataMsg GtMsgContents` only
                       fail ("get@DataMsg: invalid tag: " ++ show tag)
            Just limit -> limitGet limit parser

-- | Specifies limit on message length.
-- Deserialization would fail if incoming data size exceeded this limit.
-- At serialisation stage message size is __not__ checked.
class (Message a, Limiter (LimitType a)) => MessageLimited a where
    type LimitType a :: *
    getMsgLenLimit :: MonadDB ssc m => Proxy a -> m (LimitType a)

instance Message (MsgBlock ssc) =>
         MessageLimited (MsgBlock ssc) where
    type LimitType (MsgBlock ssc) = Byte
    getMsgLenLimit _ = GState.getMaxBlockSize

instance Message (InvMsg key tag) =>
         MessageLimited (InvMsg key tag) where
    type LimitType (InvMsg key tag) = Byte
    getMsgLenLimit _ = return genesisMaxReqSize

instance Message (ReqMsg key tag) =>
         MessageLimited (ReqMsg key tag) where
    type LimitType (ReqMsg key tag) = Byte
    getMsgLenLimit _ = return genesisMaxInvSize

instance Message (DataMsg TxMsgContents) =>
         MessageLimited (DataMsg TxMsgContents) where
    type LimitType (DataMsg TxMsgContents) = Byte
    getMsgLenLimit _ = return genesisMaxTxSize

instance Message (DataMsg UpdateVote) =>
         MessageLimited (DataMsg UpdateVote) where
    type LimitType (DataMsg UpdateVote) = Byte
    getMsgLenLimit _ = return undefined

instance Message (DataMsg (UpdateProposal, [UpdateVote])) =>
         MessageLimited (DataMsg (UpdateProposal, [UpdateVote])) where
    type LimitType (DataMsg (UpdateProposal, [UpdateVote])) = Byte
    getMsgLenLimit _ = return undefined

instance Message (DataMsg GtMsgContents) =>
         MessageLimited (DataMsg GtMsgContents) where
    type LimitType (DataMsg GtMsgContents) = (Byte, Byte, Byte, Byte)
    getMsgLenLimit _ = return undefined

-- | Sets size limit to deserialization instances via @s@ parameter
-- (using "Data.Reflection"). Grep for 'reify' and 'reflect' to see
-- usage examples.
-- @l@ parameter specifies type of limit and is generally determined by @a@
newtype LimitedLengthExt s l a = LimitedLength
    { withLimitedLength :: a
    } deriving (Eq, Ord, Show)

type LimitedLength s = LimitedLengthExt s Byte

instance (Bi a, Reifies s l, Limiter l) => Bi (LimitedLengthExt s l a) where
    put (LimitedLength a) = put a
    get = do
        let maxBlockSize = reflect (Proxy @s)
        limitGet maxBlockSize $ LimitedLength <$> get
