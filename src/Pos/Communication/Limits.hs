{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Communication.Limits
       (
         module Pos.Communication.Limits.Types

       , updateVoteNumLimit
       , commitmentsNumLimit
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet              as CC
import           Crypto.Hash                        (Blake2b_224, Blake2b_256)
import qualified Crypto.PVSS                        as PVSS
import           Data.Coerce                        (coerce)
import           GHC.Exts                           (IsList (..))

import           Pos.Binary.Class                   (AsBinary (..))
import           Pos.Block.Core                     (Block, BlockHeader)
import           Pos.Block.Network.Types            (MsgBlock (..), MsgGetBlocks (..),
                                                     MsgGetHeaders (..), MsgHeaders (..),
                                                     MsgSubscribe (..))
import           Pos.Communication.Types.Relay      (DataMsg (..))
import qualified Pos.Constants                      as Const
import           Pos.Core                           (BlockVersionData (..),
                                                     coinPortionToDouble)
import           Pos.Crypto                         (AbstractHash, EncShare,
                                                     ProxyCert (..), ProxySecretKey (..),
                                                     ProxySignature (..), PublicKey,
                                                     SecretProof, SecretSharingExtra (..),
                                                     Share, Signature (..), VssPublicKey)
import qualified Pos.DB.Class                       as DB
import           Pos.Delegation.Types               (ProxySKLightConfirmation)
import           Pos.Ssc.GodTossing.Core.Types      (Commitment (..), InnerSharesMap,
                                                     Opening, SignedCommitment,
                                                     VssCertificate)
import           Pos.Ssc.GodTossing.Types.Message   (MCCommitment (..), MCOpening (..),
                                                     MCShares (..), MCVssCertificate (..))
import           Pos.Txp.Core                       (TxAux)
import           Pos.Txp.Network.Types              (TxMsgContents (..))
import           Pos.Types                          (EpochIndex)
import           Pos.Update.Core.Types              (UpdateProposal (..), UpdateVote (..))

-- Reexports
import           Pos.Communication.Limits.Instances ()
import           Pos.Communication.Limits.Types

----------------------------------------------------------------------------
-- Instances (MessageLimited[Pure])
----------------------------------------------------------------------------

----------------------------------------------------------------------------
---- Core and lower
----------------------------------------------------------------------------

instance MessageLimitedPure CC.XSignature where
    msgLenLimit = 64

instance MessageLimitedPure (Signature a) where
    msgLenLimit = Signature <$> msgLenLimit

instance MessageLimitedPure PublicKey where
    msgLenLimit = 64

-- Sometimes 'AsBinary a' is serialized with some overhead compared to
-- 'a'. This overhead is estimated as at most 20.
maxAsBinaryOverhead :: Limit a
maxAsBinaryOverhead = 20

instance MessageLimitedPure a => MessageLimitedPure (AsBinary a) where
    msgLenLimit = coerce (msgLenLimit @a) + maxAsBinaryOverhead

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

instance MessageLimitedPure (AbstractHash Blake2b_224 a) where
    msgLenLimit = 28

instance MessageLimitedPure (AbstractHash Blake2b_256 a) where
    msgLenLimit = 32

instance MessageLimitedPure EpochIndex where
    msgLenLimit = 10

-----------------------------------------------------------------
-- Delegation
-----------------------------------------------------------------

instance MessageLimitedPure (ProxyCert w) where
    msgLenLimit = ProxyCert <$> msgLenLimit

instance MessageLimitedPure w => MessageLimitedPure (ProxySecretKey w) where
    msgLenLimit = ProxySecretKey <$> msgLenLimit <+> msgLenLimit
                                 <+> msgLenLimit <+> msgLenLimit

instance MessageLimitedPure w => MessageLimitedPure (ProxySignature w a) where
    msgLenLimit = ProxySignature <$> msgLenLimit <+> msgLenLimit

instance MessageLimitedPure w => MessageLimited (ProxySecretKey w)
instance MessageLimitedPure w => MessageLimited (ProxySignature w a)

instance MessageLimited ProxySKLightConfirmation

----------------------------------------------------------------------------
---- GodTossing
----------------------------------------------------------------------------

-- | Upper bound on number of `PVSS.Commitment`s in single
-- `Commitment`.  Actually it's a maximum number of participants in
-- GodTossing. So it also limits number of shares, for instance.
commitmentsNumLimit :: DB.MonadGState m => m Int
commitmentsNumLimit =
    -- succ is just in case
    succ . ceiling . recip . coinPortionToDouble . bvdMpcThd <$>
    DB.gsAdoptedBVData

instance MessageLimited SecretSharingExtra where
    getMsgLenLimit _ = do
        numLimit <- commitmentsNumLimit
        return $ SecretSharingExtra <$> msgLenLimit <+> vector numLimit

instance MessageLimited (AsBinary SecretSharingExtra) where
    getMsgLenLimit _ =
        coerce . (maxAsBinaryOverhead +) <$>
        getMsgLenLimit (Proxy @SecretSharingExtra)

instance MessageLimited Commitment where
    getMsgLenLimit _ = do
        extraLimit <- getMsgLenLimit (Proxy @(AsBinary SecretSharingExtra))
        numLimit <- commitmentsNumLimit
        return $
            Commitment <$> extraLimit <+> msgLenLimit <+> multiMap numLimit

instance MessageLimited SignedCommitment where
    getMsgLenLimit _ = do
        commLimit <- getMsgLenLimit (Proxy @Commitment)
        return $ (,,) <$> msgLenLimit <+> commLimit <+> msgLenLimit

instance MessageLimitedPure Opening where
    msgLenLimit = 33

instance MessageLimited InnerSharesMap where
    getMsgLenLimit _ = do
        numLimit <- commitmentsNumLimit
        return $ multiMap numLimit

-- There is some precaution in this limit. 171 means that epoch is
-- extremely large. It shouldn't happen in practice, but adding few
-- bytes to the limit is harmless.
instance MessageLimitedPure VssCertificate where
    msgLenLimit = 171

instance MessageLimitedPure MCOpening where
    msgLenLimit = MCOpening <$> msgLenLimit <+> msgLenLimit

instance MessageLimitedPure MCVssCertificate where
    msgLenLimit = MCVssCertificate <$> msgLenLimit

instance MessageLimited MCOpening
instance MessageLimited MCVssCertificate

instance MessageLimited MCCommitment where
    getMsgLenLimit _ = fmap MCCommitment
          <$> getMsgLenLimit (Proxy @SignedCommitment)

instance MessageLimited MCShares where
    getMsgLenLimit _ = (MCShares <$> msgLenLimit <+>)
          <$> getMsgLenLimit (Proxy @InnerSharesMap)

----------------------------------------------------------------------------
---- Data msg
----------------------------------------------------------------------------

-- Binary instances for DataMsg are now separated because we want
-- to do additional checks in get, but data payload is serialized
-- in straightforward way

instance MessageLimitedPure a => MessageLimitedPure (DataMsg a) where
    msgLenLimit = DataMsg <$> msgLenLimit

instance MessageLimited a => MessageLimited (DataMsg a) where
    --type LimitType (DataMsg a) = LimitType a
    getMsgLenLimit _ = (fmap . fmap) DataMsg (getMsgLenLimit (Proxy @a))

----------------------------------------------------------------------------
---- Txp
----------------------------------------------------------------------------

instance MessageLimited TxAux where
    -- FIXME Integer -> Word32
    getMsgLenLimit _ = Limit . fromIntegral <$> DB.gsMaxTxSize

instance MessageLimited TxMsgContents where
    getMsgLenLimit _ = fmap TxMsgContents <$> getMsgLenLimit (Proxy @TxAux)

----------------------------------------------------------------------------
---- Update System
----------------------------------------------------------------------------

-- | Upper bound on number of votes carried with single `UpdateProposal`.
updateVoteNumLimit :: DB.MonadGState m => m Int
updateVoteNumLimit =
    -- succ is just in case
    succ . ceiling . recip . coinPortionToDouble . bvdUpdateVoteThd <$>
    DB.gsAdoptedBVData

instance MessageLimitedPure UpdateVote where
    msgLenLimit =
        UpdateVote <$> msgLenLimit <+> msgLenLimit <+> msgLenLimit
                   <+> msgLenLimit

instance MessageLimited UpdateVote

instance MessageLimited UpdateProposal where
    -- FIXME Integer -> Word32
    getMsgLenLimit _ = Limit . fromIntegral <$> DB.gsMaxProposalSize

instance MessageLimited (UpdateProposal, [UpdateVote]) where
    getMsgLenLimit _ = do
        proposalLimit <- getMsgLenLimit (Proxy @UpdateProposal)
        voteNumLimit <- updateVoteNumLimit
        return ((,) <$> proposalLimit <+> vector voteNumLimit)

----------------------------------------------------------------------------
---- Blocks/headers
----------------------------------------------------------------------------

instance MessageLimitedPure MsgGetBlocks where
    msgLenLimit = MsgGetBlocks <$> msgLenLimit <+> msgLenLimit

instance MessageLimited MsgGetBlocks

instance MessageLimited (BlockHeader ssc) where
    -- FIXME Integer -> Word32
    getMsgLenLimit _ = Limit . fromIntegral <$> DB.gsMaxHeaderSize

instance MessageLimited (Block ssc) where
    -- FIXME Integer -> Word32
    getMsgLenLimit _ = Limit . fromIntegral <$> DB.gsMaxBlockSize

instance MessageLimited (MsgBlock ssc) where
    getMsgLenLimit _ = do
        blkLimit <- getMsgLenLimit (Proxy @(Block ssc))
        return $ MsgBlock <$> blkLimit

instance MessageLimitedPure MsgGetHeaders where
    msgLenLimit = MsgGetHeaders <$> vector maxGetHeadersNum <+> msgLenLimit
      where
        maxGetHeadersNum = ceiling $
            log (fromIntegral Const.blkSecurityParam) + (5 :: Double)

instance MessageLimited MsgGetHeaders

instance MessageLimited (MsgHeaders ssc) where
    getMsgLenLimit _ = do
        headerLimit <- getMsgLenLimit (Proxy @(BlockHeader ssc))
        return $
            MsgHeaders <$> vectorOf Const.recoveryHeadersMessage headerLimit

instance MessageLimitedPure MsgSubscribe where
    msgLenLimit = 0

instance MessageLimited MsgSubscribe

----------------------------------------------------------------------------
-- Arbitrary
----------------------------------------------------------------------------

-- TODO [CSL-859]
-- These instances were assuming that commitment limit is constant, but
-- it's not, because threshold can change.
-- P. S. Also it would be good to move them somewhere (and clean-up
-- this module), because currently it's quite messy (I think). @gromak
-- By messy I mean at least that it contains some 'Arbitrary' stuff, which we
-- usually put somewhere outside. Also I don't like that it knows about
-- GodTossing (I think instances for GodTossing should be in GodTossing),
-- but it can wait.

-- instance T.Arbitrary (MaxSize Commitment) where
--     arbitrary = MaxSize <$>
--         (Commitment <$> T.arbitrary <*> T.arbitrary
--                     <*> aMultimap commitmentsNumLimit)
--
-- instance T.Arbitrary (MaxSize SecretSharingExtra) where
--     arbitrary = do
--         SecretSharingExtra gen commitments <- T.arbitrary
--         let commitments' = alignLength commitmentsNumLimit commitments
--         return $ MaxSize $ SecretSharingExtra gen commitments'
--       where
--         alignLength n = take n . cycle
--
-- instance T.Arbitrary (MaxSize MCCommitment) where
--     arbitrary = fmap MaxSize $ MCCommitment <$>
--             ((,,) <$> T.arbitrary
--                   <*> (getOfMaxSize <$> T.arbitrary)
--                   <*> T.arbitrary)
--
-- instance T.Arbitrary (MaxSize MCOpening) where
--     arbitrary = fmap MaxSize $ MCOpening <$> T.arbitrary <*> T.arbitrary
--
-- instance T.Arbitrary (MaxSize MCShares) where
--     arbitrary = fmap MaxSize $ MCShares <$> T.arbitrary
--                      <*> aMultimap commitmentsNumLimit
--
-- instance T.Arbitrary (MaxSize MCVssCertificate) where
--     arbitrary = fmap MaxSize $ MCVssCertificate <$> T.arbitrary
--
-- instance T.Arbitrary (MaxSize (DataMsg MCCommitment)) where
--     arbitrary = fmap DataMsg <$> T.arbitrary
--
-- instance T.Arbitrary (MaxSize (DataMsg MCOpening)) where
--     arbitrary = fmap DataMsg <$> T.arbitrary
--
-- instance T.Arbitrary (MaxSize (DataMsg MCShares)) where
--     arbitrary = fmap DataMsg <$> T.arbitrary
--
-- instance T.Arbitrary (MaxSize (DataMsg MCVssCertificate)) where
--     arbitrary = fmap DataMsg <$> T.arbitrary

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- -- | Generates multimap which has given number of keys, each associated
-- -- with a single value
-- aMultimap
--     :: (Eq k, Hashable k, T.Arbitrary k, T.Arbitrary v)
--     => Int -> T.Gen (HashMap k (NonEmpty v))
-- aMultimap k =
--     let pairs = (,) <$> T.arbitrary <*> ((:|) <$> T.arbitrary <*> pure [])
--     in  fromList <$> T.vectorOf k pairs

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
