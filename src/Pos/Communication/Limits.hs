{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Communication.Limits
    ( MessageLimited (..)
    , MessageLimitedPure (..)
    , LimitedLengthExt (..)
    , Limiter (..)
    , LimitedLength
    , Limit (..)
    , MaxSize (..)
    , mcCommitmentMsgLenLimit
    , mcSharesMsgLenLimit
    , updateVoteNumLimit
    , commitmentsNumLimit
    ) where

import           Control.Lens                     (each, ix)
import qualified Crypto.PVSS                      as PVSS
import           Crypto.Hash                      (Blake2s_224, Blake2s_256)
import           Data.Binary                      (Get)
import           Data.Binary.Get                  (lookAhead, getWord8)
import           Data.Proxy                       (Proxy (..))
import           Data.Reflection                  (Reifies, reflect)
import           GHC.Exts                         (IsList (..))
import           Serokell.Data.Memory.Units       (Byte)
import qualified Test.QuickCheck                  as T
import           Universum

import           Pos.Binary.Class                 (Bi (..))
import qualified Pos.Binary.Class                 as Bi
import           Pos.Block.Network.Types          (MsgBlock)
import qualified Pos.Constants                    as Const
import           Pos.Communication.Types.Relay    (DataMsg (..), InvMsg, ReqMsg)
import           Pos.Crypto                       (Signature, PublicKey,
                                                   SecretSharingExtra (..), SecretProof,
                                                   VssPublicKey, EncShare, AbstractHash,
                                                   Share)
import           Pos.DB.Class                     (MonadDB)
import qualified Pos.DB.GState                    as GState
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..))
import           Pos.Ssc.GodTossing.Core.Types    (Commitment (..))
import           Pos.Types.Coin                   (coinPortionToDouble)
import           Pos.Txp.Types.Communication      (TxMsgContents)
import           Pos.Update.Core.Types            (UpdateProposal, UpdateVote (..))
import           Pos.Util.Binary                  (AsBinary (..))

-- | Specifies limit for given type @t@.
newtype Limit t = Limit Byte
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance Functor Limit where
    fmap _ (Limit x) = Limit x

instance Monoid (Limit a) where
    mempty = Limit 0
    Limit a `mappend` Limit b = Limit $ a + b

instance Applicative Limit where
    pure = panic "Applicative.pure Limit"
    (Limit a) <*> (Limit b) = Limit $ a + b

commitmentsNumLimit :: Int
commitmentsNumLimit = round $ 1 / coinPortionToDouble Const.genesisMpcThd

updateVoteNumLimit :: Int
updateVoteNumLimit = join traceShow $ round $ 1 / coinPortionToDouble Const.genesisUpdateVoteThd

vectorOf :: IsList l => Int -> Limit (Item l) -> Limit l
vectorOf k (Limit x) =
    Limit $ encodedListLength + x * (fromIntegral k)
  where
    -- should be enough for most reasonable cases
    encodedListLength = 8

coerce :: Limit a -> Limit b
coerce (Limit x) = Limit x

-- | Specifies type of limit on incoming message size.
-- Useful when the type has several limits and choice depends on constructor.
class Limiter l where
    limitGet :: l -> Get a -> Get a

instance Limiter (Limit t) where
    limitGet (Limit l) = Bi.limitGet $ fromIntegral l

-- | Limit depends on value of first byte, which should be in range @0..3@.
instance Limiter (Limit t, Limit t, Limit t, Limit t) where
    limitGet limits parser = do
        tag <- fromIntegral <$> lookAhead getWord8
        case (limits ^.. each) ^? ix tag of
            Nothing -> -- Such limiter is used in `DataMsg GtMsgContents` only
                       fail ("get@DataMsg: invalid tag: " ++ show tag)
            Just limit -> limitGet limit parser

-- | Specifies limit on message length.
-- Deserialization would fail if incoming data size exceeded this limit.
-- At serialisation stage message size is __not__ checked.
class Limiter (LimitType a) => MessageLimited a where
    type LimitType a :: *
    getMsgLenLimit :: MonadDB ssc m => Proxy a -> m (LimitType a)

-- | Pure analogy to `MessageLimited`.
class MessageLimitedPure a where
    msgLenLimit :: Limit a

instance MessageLimited (MsgBlock ssc) where
    type LimitType (MsgBlock ssc) = Limit (MsgBlock ssc)
    getMsgLenLimit _ = Limit <$> GState.getMaxBlockSize

instance MessageLimited (InvMsg key tag) where
    type LimitType (InvMsg key tag) = Limit (InvMsg key tag)
    getMsgLenLimit _ = return msgLenLimit

instance MessageLimited (ReqMsg key tag) where
    type LimitType (ReqMsg key tag) = Limit (ReqMsg key tag)
    getMsgLenLimit _ = return msgLenLimit

instance MessageLimited (DataMsg TxMsgContents) where
    type LimitType (DataMsg TxMsgContents) = Limit (DataMsg TxMsgContents)
    getMsgLenLimit _ = return msgLenLimit

instance MessageLimited (DataMsg UpdateVote) where
    type LimitType (DataMsg UpdateVote) = Limit (DataMsg UpdateVote)
    getMsgLenLimit _ = return msgLenLimit

instance MessageLimited (DataMsg (UpdateProposal, [UpdateVote])) where
    type LimitType (DataMsg (UpdateProposal, [UpdateVote])) =
        Limit (DataMsg (UpdateProposal, [UpdateVote]))
    getMsgLenLimit _ = return msgLenLimit

mcCommitmentMsgLenLimit :: Limit GtMsgContents
mcCommitmentMsgLenLimit = MCCommitment <$> msgLenLimit

mcSharesMsgLenLimit :: Limit GtMsgContents
mcSharesMsgLenLimit =
    MCShares <$> msgLenLimit <*> vectorOf commitmentsNumLimit msgLenLimit

instance MessageLimited (DataMsg GtMsgContents) where
    type LimitType (DataMsg GtMsgContents) =
        ( Limit (DataMsg GtMsgContents)
        , Limit (DataMsg GtMsgContents)
        , Limit (DataMsg GtMsgContents)
        , Limit (DataMsg GtMsgContents)
        )
    getMsgLenLimit _ =
        return $ each %~ fmap DataMsg $
            ( mcCommitmentMsgLenLimit
            , Limit Const.genesisMaxMCOpeningSize
            , mcSharesMsgLenLimit
            , Limit Const.genesisMaxMCVssCertificateSize
            )

instance MessageLimitedPure (InvMsg key tag) where
    msgLenLimit = Limit Const.genesisMaxReqSize

instance MessageLimitedPure (ReqMsg key tag) where
    msgLenLimit = Limit Const.genesisMaxReqSize

instance MessageLimitedPure Commitment where
    msgLenLimit =
        Commitment <$> msgLenLimit <*> msgLenLimit
                   <*> (vectorOf commitmentsNumLimit msgLenLimit)

instance MessageLimitedPure SecretSharingExtra where
    msgLenLimit =
        SecretSharingExtra <$> msgLenLimit
                           <*> vectorOf commitmentsNumLimit msgLenLimit

instance MessageLimitedPure UpdateProposal where
    msgLenLimit = 212

instance MessageLimitedPure UpdateVote where
    msgLenLimit =
        UpdateVote <$> msgLenLimit <*> msgLenLimit <*> msgLenLimit
                   <*> msgLenLimit

instance MessageLimitedPure (DataMsg UpdateVote) where
    msgLenLimit = DataMsg <$> msgLenLimit

instance MessageLimitedPure (DataMsg (UpdateProposal, [UpdateVote])) where
    msgLenLimit = DataMsg <$>
        ((,) <$> msgLenLimit <*> vectorOf updateVoteNumLimit msgLenLimit)

instance MessageLimitedPure TxMsgContents where
    msgLenLimit = Limit Const.genesisMaxTxSize

instance MessageLimitedPure (DataMsg TxMsgContents) where
    msgLenLimit = DataMsg <$> msgLenLimit

instance ( MessageLimitedPure a
         , MessageLimitedPure b
         )
         => MessageLimitedPure (a, b) where
    msgLenLimit = (,) <$> msgLenLimit <*> msgLenLimit

instance ( MessageLimitedPure a
         , MessageLimitedPure b
         , MessageLimitedPure c
         )
         => MessageLimitedPure (a, b, c) where
    msgLenLimit = (,,) <$> msgLenLimit <*> msgLenLimit <*> msgLenLimit

instance MessageLimitedPure (Signature a) where
    msgLenLimit = 64

instance MessageLimitedPure PublicKey where
    msgLenLimit = 32

instance MessageLimitedPure Bool where
    msgLenLimit = 1

instance MessageLimitedPure a => MessageLimitedPure (AsBinary a) where
    msgLenLimit = coerce (msgLenLimit :: Limit a) + 8

instance MessageLimitedPure SecretProof where
    msgLenLimit = 64

instance MessageLimitedPure VssPublicKey where
    msgLenLimit = 33

instance MessageLimitedPure EncShare where
    msgLenLimit = 101

instance MessageLimitedPure Share where
    msgLenLimit = 101 --4+33+64

instance MessageLimitedPure PVSS.Commitment where
    msgLenLimit = 33

instance MessageLimitedPure PVSS.ExtraGen where
    msgLenLimit = 33

instance MessageLimitedPure (AbstractHash Blake2s_224 a) where
    msgLenLimit = 28

instance MessageLimitedPure (AbstractHash Blake2s_256 a) where
    msgLenLimit = 32

-- | Sets size limit to deserialization instances via @s@ parameter
-- (using "Data.Reflection"). Grep for 'reify' and 'reflect' to see
-- usage examples.
-- @l@ parameter specifies type of limit and is generally determined by @a@
newtype LimitedLengthExt s l a = LimitedLength
    { withLimitedLength :: a
    } deriving (Eq, Ord, Show)

type LimitedLength s a = LimitedLengthExt s (Limit a) a

instance (Bi a, Reifies s l, Limiter l) => Bi (LimitedLengthExt s l a) where
    put (LimitedLength a) = put a
    get = do
        let maxBlockSize = reflect (Proxy @s)
        limitGet maxBlockSize $ LimitedLength <$> get

-- | Wrapper for `Arbitrary` instances to indicate that
-- where an alternative exists, maximal available size is choosen.
-- This is required at first place to generate lists of max available size.
newtype MaxSize a = MaxSize
    { getOfMaxSize :: a
    } deriving (Eq, Ord, Show, Bi, MessageLimitedPure)

instance T.Arbitrary (MaxSize Commitment) where
    arbitrary = MaxSize <$>
        (Commitment <$> T.arbitrary <*> T.arbitrary
                    <*> (fromList <$> T.vector commitmentsNumLimit))

instance T.Arbitrary (MaxSize SecretSharingExtra) where
    arbitrary = do
        SecretSharingExtra gen commitments <- T.arbitrary
        let commitments' = alignLength commitmentsNumLimit commitments
        return $ MaxSize $ SecretSharingExtra gen commitments'
      where
        alignLength n = take n . cycle
