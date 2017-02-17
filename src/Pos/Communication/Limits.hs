{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Communication.Limits
    ( Limit (..)
    , Limiter (..)
    , MessageLimited (..)
    , MessageLimitedPure (..)

    , LimitedLengthExt (..)
    , LimitedLength
    , reifyMsgLimit

    , MaxSize (..)

    , updateVoteNumLimit
    , commitmentsNumLimit
    , upDataNumLimit
    , appNameLenLimit

    , mcCommitmentMsgLenLimit
    , mcSharesMsgLenLimit
    ) where

import           Control.Lens                     (both, each, ix)
import qualified Crypto.PVSS                      as PVSS
import           Crypto.Hash                      (Blake2s_224, Blake2s_256)
import           Data.Binary                      (Get)
import           Data.Binary.Get                  (lookAhead, getWord8)
import           Data.Proxy                       (Proxy (..))
import           Data.Reflection                  (Reifies (..), reify)
import           GHC.Exts                         (IsList (..))
import           Serokell.Data.Memory.Units       (Byte)
import qualified Test.QuickCheck                  as T
import           Universum

import           Pos.Binary.Class                 (Bi (..))
import qualified Pos.Binary.Class                 as Bi
import           Pos.Block.Network.Types          (MsgBlock)
import qualified Pos.Constants                    as Const
import           Pos.Communication.Types.Relay    (DataMsg (..), InvMsg, ReqMsg,
                                                   InvOrData)
import           Pos.Crypto                       (Signature, PublicKey,
                                                   SecretSharingExtra (..), SecretProof,
                                                   VssPublicKey, EncShare, AbstractHash,
                                                   Share)
import           Pos.DB.Class                     (MonadDB)
import qualified Pos.DB.GState                    as GState
import           Pos.Ssc.GodTossing.Arbitrary     ()
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..))
import           Pos.Ssc.GodTossing.Core.Types    (Commitment (..))
import           Pos.Types                        (ApplicationName, BlockVersion,
                                                   SoftwareVersion (..),
                                                   coinPortionToDouble)
import           Pos.Txp.Types.Communication      (TxMsgContents)
import           Pos.Update.Core.Types            (UpdateProposal (..), UpdateVote (..),
                                                   BlockVersionData, UpAttributes,
                                                   SystemTag, UpdateData)
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

-- | Upper bound on number of `PVSS.Commitment`s in single `Commitment`.
commitmentsNumLimit :: Int
commitmentsNumLimit = round $ 1 / coinPortionToDouble Const.genesisMpcThd

-- | Upper bound on number of votes carried with single `UpdateProposal`.
updateVoteNumLimit :: Int
updateVoteNumLimit = round $ 1 / coinPortionToDouble Const.genesisUpdateVoteThd

-- | Upper bound on size of `upData` in `UpdateProposal`.
upDataNumLimit :: Int
upDataNumLimit = 1

-- | Upper bound on size of `ApplicationName`
appNameLenLimit :: Int
appNameLenLimit = 30

vectorOf :: IsList l => Int -> Limit (Item l) -> Limit l
vectorOf k (Limit x) =
    Limit $ encodedListLength + x * (fromIntegral k)
  where
    -- should be enough for most reasonable cases
    encodedListLength = 20

vector :: (IsList l, MessageLimitedPure (Item l)) => Int -> Limit l
vector k = vectorOf k msgLenLimit

multiMap
    :: (IsList l, Item l ~ (k, l0), IsList l0,
        MessageLimitedPure k, MessageLimitedPure (Item l0))
    => Int -> Limit l
multiMap k =
    -- max message length is reached when each key has single value
    vectorOf k $ (,) <$> msgLenLimit <*> vector 1

coerce :: Limit a -> Limit b
coerce (Limit x) = Limit x

-- | Specifies type of limit on incoming message size.
-- Useful when the type has several limits and choice depends on constructor.
class Limiter l where
    limitGet :: l -> Get a -> Get a
    addLimit :: Byte -> l -> l

instance Limiter (Limit t) where
    limitGet (Limit l) = Bi.limitGet $ fromIntegral l
    addLimit a = mappend (Limit a)

-- | Bounds `InvOrData`.
instance Limiter l => Limiter (Limit t, l) where
    limitGet (invLimit, dataLimits) parser = do
        lookAhead getWord8 >>= \case
            0   -> limitGet invLimit parser
            1   -> limitGet dataLimits parser
            tag -> fail ("get@InvOrData: invalid tag: " ++ show tag)

    addLimit a (l1, l2) = (a `addLimit` l1, a `addLimit` l2)

-- | Bounds `DataMsg`.
-- Limit depends on value of first byte, which should be in range @0..3@.
instance Limiter (Limit t, Limit t, Limit t, Limit t) where
    limitGet limits parser = do
        tag <- fromIntegral <$> lookAhead getWord8
        case (limits ^.. each) ^? ix tag of
            Nothing -> fail ("get@DataMsg: invalid tag: " ++ show tag)
            Just limit -> limitGet limit parser

    addLimit a = both %~ addLimit a

-- | Specifies limit on message length.
-- Deserialization would fail if incoming data size exceeded this limit.
-- At serialisation stage message size is __not__ checked.
class Limiter (LimitType a) => MessageLimited a where
    type LimitType a :: *
    getMsgLenLimit :: MonadDB ssc m => Proxy a -> m (LimitType a)

-- | Pure analogy to `MessageLimited`.
--
-- All instances are encouraged to be covered with tests
-- (using `Test.Pos.Util.msgLenLimitedTest`).
--
-- If you're going to add instance and have no idea regarding limit value,
-- and your type's size is essentially bounded (doesn't contain list-like
-- structures and doesn't depend on global parameters), you can do as follows:
-- 1) Create instance with limit @1@.
-- 2) Add test case, run - it would fail and report actual size.
-- 3) Insert that value into instance.
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

noLimit :: Limit a
noLimit = 1000000

mcCommitmentMsgLenLimit :: Limit GtMsgContents
mcCommitmentMsgLenLimit = MCCommitment <$> msgLenLimit

mcSharesMsgLenLimit :: Limit GtMsgContents
mcSharesMsgLenLimit =
    MCShares <$> msgLenLimit <*> multiMap commitmentsNumLimit

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
            , noLimit
            , mcSharesMsgLenLimit
            , noLimit
            )

instance MessageLimited (DataMsg contents)
      => MessageLimited (InvOrData tag key contents) where
    type LimitType (InvOrData tag key contents) =
        ( LimitType (InvMsg key tag)
        , LimitType (DataMsg contents)
        )
    getMsgLenLimit _ = do
        invLim  <- getMsgLenLimit $ Proxy @(InvMsg key tag)
        dataLim <- getMsgLenLimit $ Proxy @(DataMsg contents)
        -- 1 byte is added because of `Either`
        return (1 `addLimit` invLim, 1 `addLimit` dataLim)

instance MessageLimitedPure (InvMsg key tag) where
    msgLenLimit = Limit Const.genesisMaxReqSize

instance MessageLimitedPure (ReqMsg key tag) where
    msgLenLimit = Limit Const.genesisMaxReqSize

instance MessageLimitedPure Commitment where
    msgLenLimit =
        Commitment <$> msgLenLimit <*> msgLenLimit
                   <*> multiMap commitmentsNumLimit

instance MessageLimitedPure SecretSharingExtra where
    msgLenLimit =
        SecretSharingExtra <$> msgLenLimit <*> vector commitmentsNumLimit

instance MessageLimitedPure UpdateProposal where
    msgLenLimit =
        UpdateProposal <$> msgLenLimit <*> msgLenLimit <*> msgLenLimit
                       <*> vector upDataNumLimit <*> msgLenLimit

instance MessageLimitedPure UpdateVote where
    msgLenLimit =
        UpdateVote <$> msgLenLimit <*> msgLenLimit <*> msgLenLimit
                   <*> msgLenLimit

instance MessageLimitedPure SoftwareVersion where
    msgLenLimit = SoftwareVersion <$> msgLenLimit <*> msgLenLimit

instance MessageLimitedPure (DataMsg UpdateVote) where
    msgLenLimit = DataMsg <$> msgLenLimit

instance MessageLimitedPure (DataMsg (UpdateProposal, [UpdateVote])) where
    msgLenLimit = DataMsg <$>
        ((,) <$> msgLenLimit <*> vector updateVoteNumLimit)

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
    msgLenLimit = coerce (msgLenLimit :: Limit a) + 20

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

instance MessageLimitedPure BlockVersion where
    msgLenLimit = 5

instance MessageLimitedPure BlockVersionData where
    msgLenLimit = 65

instance MessageLimitedPure ApplicationName where
    msgLenLimit = fromIntegral appNameLenLimit

instance MessageLimitedPure Word32 where
    msgLenLimit = 4

instance MessageLimitedPure SystemTag where
    msgLenLimit = 6

instance MessageLimitedPure UpAttributes where
#ifdef DEV_MODE
    msgLenLimit = 0  -- real value
#else
    msgLenLimit = 10000  -- for backward compatibility
#endif

instance MessageLimitedPure UpdateData where
    msgLenLimit = 128

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

-- | Used to provide type @s@, which carries limit on length
-- of message @a@ (via Data.Reflection).
reifyMsgLimit
    :: forall a m b ssc. (MonadDB ssc m, MessageLimited a)
    => Proxy a
    -> (forall s. Reifies s (LimitType a) => Proxy s -> m b)
    -> m b
reifyMsgLimit _ f = do
    lengthLimit <- getMsgLenLimit $ Proxy @a
    reify lengthLimit f

-- | Wrapper for `Arbitrary` instances to indicate that
-- where an alternative exists, maximal available size is choosen.
-- This is required at first place to generate lists of max available size.
newtype MaxSize a = MaxSize
    { getOfMaxSize :: a
    } deriving (Eq, Ord, Show, Bi, Functor, MessageLimitedPure)

-- | Generates multimap which has given number of keys, each assisiated
-- with single value
aMultimap
    :: (Eq k, Hashable k, T.Arbitrary k, T.Arbitrary v)
    => Int -> T.Gen (HashMap k (NonEmpty v))
aMultimap k =
    let pairs = (,) <$> T.arbitrary <*> ((:|) <$> T.arbitrary <*> pure [])
    in  fromList <$> T.vectorOf k pairs

instance T.Arbitrary (MaxSize Commitment) where
    arbitrary = MaxSize <$>
        (Commitment <$> T.arbitrary <*> T.arbitrary
                    <*> aMultimap commitmentsNumLimit)

instance T.Arbitrary (MaxSize SecretSharingExtra) where
    arbitrary = do
        SecretSharingExtra gen commitments <- T.arbitrary
        let commitments' = alignLength commitmentsNumLimit commitments
        return $ MaxSize $ SecretSharingExtra gen commitments'
      where
        alignLength n = take n . cycle

instance T.Arbitrary (MaxSize GtMsgContents) where
    arbitrary = MaxSize <$>
        T.oneof [aCommitment, aOpening, aShares, aVssCert]
      where
        aCommitment = MCCommitment <$>
            ((,,) <$> T.arbitrary
                  <*> (getOfMaxSize <$> T.arbitrary)
                  <*> T.arbitrary)
        aOpening = MCOpening <$> T.arbitrary <*> T.arbitrary
        aShares =
            MCShares <$> T.arbitrary
                     <*> aMultimap commitmentsNumLimit
        aVssCert = MCVssCertificate <$> T.arbitrary

instance T.Arbitrary (MaxSize (DataMsg GtMsgContents)) where
    arbitrary = fmap DataMsg <$> T.arbitrary
