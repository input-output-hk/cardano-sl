{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Communication.Limits
    (
      module Pos.Communication.Limits.Types

    , updateVoteNumLimit
    , commitmentsNumLimit

    , mcCommitmentMsgLenLimit
    , mcOpeningLenLimit
    , mcSharesMsgLenLimit
    , mcVssCertificateLenLimit
    ) where

import           Control.Lens                       (each)
import           Crypto.Hash                        (Blake2s_224, Blake2s_256)
import qualified Crypto.PVSS                        as PVSS
import           GHC.Exts                           (IsList (..))
import qualified Test.QuickCheck                    as T
import           Universum

import           Pos.Binary.Class                   (AsBinary (..))
import           Pos.Block.Network.Types            (MsgBlock, MsgGetHeaders (..),
                                                     MsgHeaders (..))
import           Pos.Communication.Types.Relay      (DataMsg (..))
import qualified Pos.Constants                      as Const
import           Pos.Crypto                         (AbstractHash, EncShare, PublicKey,
                                                     SecretProof, SecretSharingExtra (..),
                                                     Share, Signature, VssPublicKey)
import qualified Pos.DB.Limits                      as DB
import           Pos.Ssc.GodTossing.Arbitrary       ()
import           Pos.Ssc.GodTossing.Core.Types      (Commitment (..))
import           Pos.Ssc.GodTossing.Types.Message   (GtMsgContents (..))
import           Pos.Txp.Network.Types              (TxMsgContents)
import           Pos.Types                          (coinPortionToDouble)
import           Pos.Update.Core.Types              (UpdateProposal (..), UpdateVote (..))

-- Reexports
import           Pos.Communication.Limits.Instances ()
import           Pos.Communication.Limits.Types

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

-- | Upper bound on number of `PVSS.Commitment`s in single `Commitment`.
commitmentsNumLimit :: Int
commitmentsNumLimit = round $ 1 / coinPortionToDouble Const.genesisMpcThd

-- | Upper bound on number of votes carried with single `UpdateProposal`.
updateVoteNumLimit :: Int
updateVoteNumLimit = round $ 1 / coinPortionToDouble Const.genesisUpdateVoteThd

mcCommitmentMsgLenLimit :: Limit GtMsgContents
mcCommitmentMsgLenLimit = MCCommitment <$> msgLenLimit

mcOpeningLenLimit :: Limit GtMsgContents
mcOpeningLenLimit = 63

mcSharesMsgLenLimit :: Limit GtMsgContents
mcSharesMsgLenLimit =
    MCShares <$> msgLenLimit <+> multiMap commitmentsNumLimit

mcVssCertificateLenLimit :: Limit GtMsgContents
mcVssCertificateLenLimit = 169

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance MessageLimited (MsgBlock ssc) where
    type LimitType (MsgBlock ssc) = Limit (MsgBlock ssc)
    getMsgLenLimit _ = Limit <$> DB.getMaxBlockSize

instance MessageLimited MsgGetHeaders where
    type LimitType MsgGetHeaders = Limit MsgGetHeaders
    getMsgLenLimit _ = return $
        MsgGetHeaders <$> vector maxGetHeadersNum <+> msgLenLimit
      where
        maxGetHeadersNum = ceiling $
            log ((fromIntegral :: Int -> Double) Const.blkSecurityParam) + 5

instance MessageLimited (MsgHeaders ssc) where
    type LimitType (MsgHeaders ssc) = Limit (MsgHeaders ssc)
    getMsgLenLimit _ = do
        headerLimit <- Limit <$> DB.getMaxHeaderSize
        return $
            MsgHeaders <$> vectorOf Const.recoveryHeadersMessage headerLimit

instance MessageLimited (DataMsg TxMsgContents) where
    type LimitType (DataMsg TxMsgContents) = Limit (DataMsg TxMsgContents)
    getMsgLenLimit _ = do
        txLimit <- Limit <$> DB.getMaxTxSize
        return $ DataMsg <$> txLimit

instance MessageLimited (DataMsg UpdateVote) where
    type LimitType (DataMsg UpdateVote) = Limit (DataMsg UpdateVote)
    getMsgLenLimit _ = return msgLenLimit

instance MessageLimited (DataMsg (UpdateProposal, [UpdateVote])) where
    type LimitType (DataMsg (UpdateProposal, [UpdateVote])) =
        Limit (DataMsg (UpdateProposal, [UpdateVote]))
    getMsgLenLimit _ = do
        proposalLimit <- Limit <$> DB.getMaxProposalSize
        return $
            DataMsg <$> ((,) <$> proposalLimit <+> vector updateVoteNumLimit)

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
            , mcOpeningLenLimit
            , mcSharesMsgLenLimit
            , mcVssCertificateLenLimit
            )

instance MessageLimitedPure Commitment where
    msgLenLimit =
        Commitment <$> msgLenLimit <+> msgLenLimit
                   <+> multiMap commitmentsNumLimit

instance MessageLimitedPure SecretSharingExtra where
    msgLenLimit =
        SecretSharingExtra <$> msgLenLimit <+> vector commitmentsNumLimit

instance MessageLimitedPure UpdateVote where
    msgLenLimit =
        UpdateVote <$> msgLenLimit <+> msgLenLimit <+> msgLenLimit
                   <+> msgLenLimit

instance MessageLimitedPure (DataMsg UpdateVote) where
    msgLenLimit = DataMsg <$> msgLenLimit

instance MessageLimitedPure (Signature a) where
    msgLenLimit = 64

instance MessageLimitedPure PublicKey where
    msgLenLimit = 64

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

-- instance MessageLimitedPure Word32 where
    -- msgLenLimit = 4

-- instance MessageLimitedPure SystemTag where
    -- msgLenLimit = 6

-- instance MessageLimitedPure UpdateData where
    -- msgLenLimit = 128

----------------------------------------------------------------------------
-- Arbitrary
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Generates multimap which has given number of keys, each associated
-- with a single value
aMultimap
    :: (Eq k, Hashable k, T.Arbitrary k, T.Arbitrary v)
    => Int -> T.Gen (HashMap k (NonEmpty v))
aMultimap k =
    let pairs = (,) <$> T.arbitrary <*> ((:|) <$> T.arbitrary <*> pure [])
    in  fromList <$> T.vectorOf k pairs

-- | Given a limit for a list item, generate limit for a list with N elements
vectorOf :: IsList l => Int -> Limit (Item l) -> Limit l
vectorOf k (Limit x) =
    Limit $ encodedListLength + x * (fromIntegral k)
  where
    -- should be enough for most reasonable cases
    encodedListLength = 20

-- | Generate limit for a list of messages with N elements
vector :: (IsList l, MessageLimitedPure (Item l)) => Int -> Limit l
vector k = vectorOf k msgLenLimit

multiMap
    :: (IsList l, Item l ~ (k, l0), IsList l0,
        MessageLimitedPure k, MessageLimitedPure (Item l0))
    => Int -> Limit l
multiMap k =
    -- max message length is reached when each key has single value
    vectorOf k $ (,) <$> msgLenLimit <+> vector 1

coerce :: Limit a -> Limit b
coerce (Limit x) = Limit x
