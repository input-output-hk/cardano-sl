{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeFamilies  #-}

module Pos.Communication.Limits
       ( module Pos.Communication.Limits.Types
       , HasAdoptedBlockVersionData (..)
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Crypto.Hash.IO (HashAlgorithm, hashDigestSize)
import qualified Crypto.SCRAPE as Scrape
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (..))
import           GHC.Exts (IsList (..))
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class (AsBinary (..))
import           Pos.Block.Configuration (HasBlockConfiguration, recoveryHeadersMessage)
import           Pos.Block.Network (MsgBlock (..), MsgGetBlocks (..), MsgGetHeaders (..),
                                    MsgHeaders (..))
import           Pos.Communication.Types.Protocol (MsgSubscribe (..), MsgSubscribe1 (..))
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (BlockVersionData (..), EpochIndex, VssCertificate, coinPortionToDouble)
import           Pos.Core.Block (Block, BlockHeader (..), GenesisBlock, GenesisBlockHeader,
                                 MainBlock, MainBlockHeader)
import           Pos.Core.Configuration (HasConfiguration, blkSecurityParam)
import           Pos.Core.Delegation (HeavyDlgIndex (..), LightDlgIndices (..))
import           Pos.Core.Ssc (Commitment (..), InnerSharesMap, Opening (..), SignedCommitment)
import           Pos.Core.Txp (TxAux)
import           Pos.Core.Update (UpdateProposal (..), UpdateVote (..))
import           Pos.Crypto (AbstractHash, DecShare, EncShare, ProxyCert (..), ProxySecretKey (..),
                             PublicKey, Secret, SecretProof (..), Signature (..), VssPublicKey)
import           Pos.Ssc.Message (MCCommitment (..), MCOpening (..), MCShares (..),
                                  MCVssCertificate (..))
import           Pos.Txp.Network.Types (TxMsgContents (..))

-- Reexports
import           Pos.Communication.Limits.Instances ()
import           Pos.Communication.Limits.Types

class HasAdoptedBlockVersionData m where
    adoptedBVData :: m BlockVersionData

----------------------------------------------------------------------------
-- Instances (MessageLimited[Pure])
----------------------------------------------------------------------------

----------------------------------------------------------------------------
---- Core and lower
----------------------------------------------------------------------------

instance Applicative m => MessageLimited CC.XSignature m where
    getMsgLenLimit _ = pure 66

instance Applicative m => MessageLimited (Signature a) m where
    getMsgLenLimit _ = fmap Signature <$> getMsgLenLimit Proxy

instance Applicative m => MessageLimited PublicKey m where
    getMsgLenLimit _ = pure 66

-- Sometimes 'AsBinary a' is serialized with some overhead compared to
-- 'a'. This is tricky to estimate as CBOR uses a number of bytes at
-- the beginning of a BS to encode the length, and the encoding of the length
-- depends on the length itself. This overhead is (conservatively) estimated
-- as at most 64.
maxAsBinaryOverhead :: Limit a
maxAsBinaryOverhead = 64

instance (Functor m, MessageLimited a m) => MessageLimited (AsBinary a) m where
    getMsgLenLimit _ = (((+) maxAsBinaryOverhead) . coerce) <$> (getMsgLenLimit (Proxy :: Proxy a))

instance Applicative m => MessageLimited VssPublicKey m where
    getMsgLenLimit _ = pure 35

instance Applicative m => MessageLimited Secret m where
    getMsgLenLimit _ = pure 35

instance Applicative m => MessageLimited EncShare m where
    getMsgLenLimit _ = pure 103

instance Applicative m => MessageLimited DecShare m where
    getMsgLenLimit _ = pure 103 --4+35+64       TODO: might be outdated

instance Applicative m => MessageLimited Scrape.Commitment m where
    getMsgLenLimit _ = pure 35

instance Applicative m => MessageLimited Scrape.ExtraGen m where
    getMsgLenLimit _ = pure 35

instance Applicative m => MessageLimited Scrape.Proof m where
    getMsgLenLimit _ = pure 35

instance (Applicative m, HashAlgorithm algo) => MessageLimited (AbstractHash algo a) m where
    getMsgLenLimit _ = pure $ fromInteger $
        toInteger (hashDigestSize (error "getMsgLenLimit AbstractHash" :: algo))
        + 4

instance Applicative m => MessageLimited EpochIndex m where
    getMsgLenLimit _ = pure 12

-----------------------------------------------------------------
-- Delegation
-----------------------------------------------------------------

instance Applicative m => MessageLimited HeavyDlgIndex m where
    getMsgLenLimit _ = fmap HeavyDlgIndex <$> getMsgLenLimit Proxy

instance Applicative m => MessageLimited LightDlgIndices m where
    getMsgLenLimit _ = fmap LightDlgIndices <$> getMsgLenLimit Proxy

instance Applicative m => MessageLimited (ProxyCert w) m where
    getMsgLenLimit _ = fmap ProxyCert <$> getMsgLenLimit Proxy

instance (Applicative m, MessageLimited w m) => MessageLimited (ProxySecretKey w) m where
    getMsgLenLimit _ = f <$> getMsgLenLimit Proxy
                         <*> getMsgLenLimit Proxy
                         <*> getMsgLenLimit Proxy
                         <*> getMsgLenLimit Proxy
      where
        f a b c d = UncheckedProxySecretKey <$> a <+> b <+> c <+> d

----------------------------------------------------------------------------
---- SSC
----------------------------------------------------------------------------

commitmentsNumLimit
    :: ( Functor m, HasAdoptedBlockVersionData m )
    => m Int
commitmentsNumLimit = succ . ceiling . recip . coinPortionToDouble . bvdMpcThd
    <$> adoptedBVData

instance (HasAdoptedBlockVersionData m, Monad m) => MessageLimited SecretProof m where
    getMsgLenLimit _ = do
        numLimit <- commitmentsNumLimit
        parproofsLimit <- getMsgLenLimit (Proxy @Scrape.ParallelProofs)
        a <- getMsgLenLimit Proxy
        b <- getMsgLenLimit Proxy
        return $ SecretProof
                   <$> a
                   <+> b
                   <+> parproofsLimit
                   <+> vector numLimit

instance (HasAdoptedBlockVersionData m, Monad m) => MessageLimited (AsBinary SecretProof) m where
    getMsgLenLimit _ = do
        a <- getMsgLenLimit (Proxy @SecretProof)
        return $ coerce (maxAsBinaryOverhead + a)
        -- coerce . (maxAsBinaryOverhead +) <$>
        -- getMsgLenLimit (Proxy @SecretProof)

instance (HasAdoptedBlockVersionData m, Monad m) => MessageLimited Scrape.ParallelProofs m where
    getMsgLenLimit _ = do
        -- ParallelProofs =
        --   • Challenge (has size 32)
        --   • as many proofs as there are participants
        --     (each proof has size 32)
        numLimit <- fromIntegral <$> commitmentsNumLimit
        return $ 32 + numLimit * 32 + 100 -- 100 just in case; something like
                                          -- 20 should be enough

instance (HasAdoptedBlockVersionData m, Monad m) => MessageLimited Commitment m where
    getMsgLenLimit _ = do
        proofLimit <- getMsgLenLimit (Proxy @SecretProof)
        numLimit <- commitmentsNumLimit
        return $
            Commitment <$> proofLimit <+> multiMap numLimit

{-
-- SignedCommitment ~ (,,) ...
--
-- but (,,) already has a MessageLimited instance.
instance (HasAdoptedBlockVersionData m, Monad m) => MessageLimited SignedCommitment m where
    getMsgLenLimit _ = do
        commLimit <- getMsgLenLimit (Proxy @Commitment)
        a <- getMsgLenLimit Proxy
        b <- getMsgLenLimit Proxy
        return $ (,,) <$> a <+> commLimit <+> b
        -- return $ (,,) <$> getMsgLenLimit Proxy <+> commLimit <+> getMsgLenLimit Proxy
-}

instance (Applicative m) => MessageLimited Opening m where
    getMsgLenLimit _ = pure 37 -- 35 for `Secret` + 2 for the `AsBinary` wrapping

instance (HasAdoptedBlockVersionData m, Monad m) => MessageLimited InnerSharesMap m where
    getMsgLenLimit _ = do
        numLimit <- commitmentsNumLimit
        return $ multiMap numLimit

-- There is some precaution in this limit. 179 means that epoch is
-- extremely large. It shouldn't happen in practice, but adding few
-- bytes to the limit is harmless.
instance (Applicative m) => MessageLimited VssCertificate m where
    getMsgLenLimit _ = pure 179

instance (Applicative m) => MessageLimited MCOpening m where
    getMsgLenLimit _ = f <$> getMsgLenLimit Proxy <*> getMsgLenLimit Proxy
      where
        f a b = MCOpening <$> a <+> b

instance (Applicative m) => MessageLimited MCVssCertificate m where
    getMsgLenLimit _ = fmap MCVssCertificate <$> getMsgLenLimit Proxy

instance (HasAdoptedBlockVersionData m, Monad m) => MessageLimited MCCommitment m where
    getMsgLenLimit _ = fmap MCCommitment
          <$> getMsgLenLimit (Proxy @SignedCommitment)

instance (HasAdoptedBlockVersionData m, Monad m) => MessageLimited MCShares m where
    getMsgLenLimit _ = f <$> getMsgLenLimit Proxy <*> getMsgLenLimit (Proxy @InnerSharesMap)
      where
        f a b = MCShares <$> a <+> b


----------------------------------------------------------------------------
---- Data msg
----------------------------------------------------------------------------

-- Binary instances for DataMsg are now separated because we want
-- to do additional checks in get, but data payload is serialized
-- in straightforward way

instance (MessageLimited a m, Functor m) => MessageLimited (DataMsg a) m where
    --type LimitType (DataMsg a) = LimitType a
    getMsgLenLimit _ = (fmap . fmap) DataMsg (getMsgLenLimit (Proxy @a))

----------------------------------------------------------------------------
---- Txp
----------------------------------------------------------------------------

maxTxSize
    :: ( Functor m, HasAdoptedBlockVersionData m )
    => m Byte
maxTxSize = bvdMaxTxSize <$> adoptedBVData

instance (HasAdoptedBlockVersionData m, Functor m) => MessageLimited TxAux m where
    -- FIXME Integer -> Word32
    getMsgLenLimit _ = Limit . fromIntegral <$> maxTxSize

instance (HasAdoptedBlockVersionData m, Functor m) => MessageLimited TxMsgContents m where
    getMsgLenLimit _ = fmap TxMsgContents <$> getMsgLenLimit (Proxy @TxAux)

----------------------------------------------------------------------------
---- Update System
----------------------------------------------------------------------------

updateVoteNumLimit
    :: ( Functor m, HasAdoptedBlockVersionData m )
    => m Int
updateVoteNumLimit = succ . ceiling . recip . coinPortionToDouble . bvdUpdateVoteThd
    <$> adoptedBVData

maxProposalSize
    :: ( Functor m, HasAdoptedBlockVersionData m )
    => m Byte
maxProposalSize = bvdMaxProposalSize <$> adoptedBVData

instance (Applicative m) => MessageLimited UpdateVote m where
    -- Add `1` byte of serialization overhead.
    getMsgLenLimit _ = f <$> getMsgLenLimit Proxy
                         <*> getMsgLenLimit Proxy
                         <*> getMsgLenLimit Proxy
                         <*> getMsgLenLimit Proxy
      where
        -- It's alright to use an unsafe constructor here because we don't
        -- create an actual vote, only count bytes
        f a b c d = (UncheckedUpdateVote <$> a <+> b <+> c <+> d) + 1

instance (HasAdoptedBlockVersionData m, Functor m) => MessageLimited UpdateProposal m where
    -- FIXME Integer -> Word32
    getMsgLenLimit _ = Limit . fromIntegral <$> maxProposalSize

-- Overlapping because there's an instance for (a, b) but that one doesn't
-- express what we want: a limit on the size of the list of votes.
instance {-# OVERLAPPING #-} (HasAdoptedBlockVersionData m, Monad m) => MessageLimited (UpdateProposal, [UpdateVote]) m where
    getMsgLenLimit _ = do
        proposalLimit <- getMsgLenLimit (Proxy @UpdateProposal)
        voteNumLimit <- updateVoteNumLimit
        return ((,) <$> proposalLimit <+> vector voteNumLimit)

----------------------------------------------------------------------------
---- Blocks/headers
----------------------------------------------------------------------------

maxBlockSize
    :: ( Functor m, HasAdoptedBlockVersionData m )
    => m Byte
maxBlockSize = bvdMaxBlockSize <$> adoptedBVData

maxHeaderSize
    :: ( Functor m, HasAdoptedBlockVersionData m )
    => m Byte
maxHeaderSize = bvdMaxHeaderSize <$> adoptedBVData

instance (Applicative m) => MessageLimited MsgGetBlocks m where
    getMsgLenLimit _ = f <$> getMsgLenLimit Proxy <*> getMsgLenLimit Proxy
      where
        f a b = MsgGetBlocks <$> a <+> b

instance (HasAdoptedBlockVersionData m, Applicative m) => MessageLimited MainBlockHeader m where
    -- FIXME Integer -> Word32
    getMsgLenLimit _ = Limit . fromIntegral <$> maxHeaderSize

-- TODO this is probably wrong, but we need it in order to get
--   MessageLimited BlockHeader m ~ MessageLimited (Either GenesisBlockHeader MainBlockHeader) m
-- because there is an instance
--   MessageLimited (Either a b) m
instance (HasAdoptedBlockVersionData m, Applicative m) => MessageLimited GenesisBlockHeader m where
    -- FIXME Integer -> Word32
    getMsgLenLimit _ = Limit . fromIntegral <$> maxHeaderSize

instance (HasAdoptedBlockVersionData m, Applicative m) => MessageLimited MainBlock m where
    -- FIXME Integer -> Word32
    getMsgLenLimit _ = Limit . fromIntegral <$> maxBlockSize

instance ( HasAdoptedBlockVersionData m
         , Applicative m
         )
         => MessageLimited BlockHeader m where
    getMsgLenLimit _ = f <$> getMsgLenLimit Proxy <*> getMsgLenLimit Proxy
      where
      maxLimit (Limit l1) (Limit l2) = Limit (max l1 l2)
      f limA limB = 1 +
          maxLimit (BlockHeaderGenesis <$> limA)
                   (BlockHeaderMain <$> limB)

-- TODO this is probably wrong, but we need it in order to get
--   MessageLimited Block m ~ MessageLimited (Either GenesisBlock MainBlock) m
-- because there is an instance
--   MessageLimited (Either a b) m
instance (HasAdoptedBlockVersionData m, Applicative m) => MessageLimited GenesisBlock m where
    -- FIXME Integer -> Word32
    getMsgLenLimit _ = Limit . fromIntegral <$> maxBlockSize

instance (HasAdoptedBlockVersionData m, Monad m) => MessageLimited MsgBlock m where
    getMsgLenLimit _ = do
        blkLimit <- getMsgLenLimit (Proxy @Block)
        return $ MsgBlock <$> blkLimit

instance (HasConfiguration, Applicative m) => MessageLimited MsgGetHeaders m where
    getMsgLenLimit _ = f <$> getMsgLenLimit Proxy
      where
        f a = MsgGetHeaders <$> vector maxGetHeadersNum <+> a
        maxGetHeadersNum = ceiling $
            log (fromIntegral blkSecurityParam) + (5 :: Double)

instance (HasBlockConfiguration, HasNodeConfiguration, HasAdoptedBlockVersionData m, Monad m) => MessageLimited MsgHeaders m where
    getMsgLenLimit _ = do
        headerLimit <- getMsgLenLimit (Proxy @BlockHeader)
        return $
            MsgHeaders <$> vectorOf recoveryHeadersMessage headerLimit

-- TODO: Update once we move to CBOR.
instance (Applicative m) => MessageLimited MsgSubscribe m where
    getMsgLenLimit _ = pure 0

-- TODO: Update once we move to CBOR.
instance (Applicative m) => MessageLimited MsgSubscribe1 m where
    getMsgLenLimit _ = pure 0

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Given a limit for a list item, generate limit for a list with N elements
vectorOf :: Int -> Limit (Item l) -> Limit l
vectorOf k (Limit x) =
    Limit $ encodedListLength + x * (fromIntegral k)
  where
    -- should be enough for most reasonable cases
    encodedListLength = 20

-- | Generate limit for a list of messages with N elements
vector :: (MessageLimited (Item l) Identity) => Int -> Limit l
vector k = vectorOf k (runIdentity (getMsgLenLimit Proxy))

multiMap
    :: (Item l ~ (k, l0), MessageLimited k Identity, MessageLimited (Item l0) Identity)
    => Int -> Limit l
multiMap k =
    -- max message length is reached when each key has single value
    vectorOf k $ (,) <$> (runIdentity (getMsgLenLimit Proxy)) <+> vector 1
