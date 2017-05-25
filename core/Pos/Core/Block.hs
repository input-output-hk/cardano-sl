{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | This module contains some general definitions related to blocks
-- and headers. The heart of this module is 'Blockchain' type class.

module Pos.Core.Block
       ( Blockchain (..)
       , BlockchainHelpers (..)
       , GenericBlockHeader (..)
       , GenericBlock (..)

       -- * Smart constructors
       , mkGenericHeader
       , recreateGenericHeader
       , mkGenericBlock
       , recreateGenericBlock

       -- * Lenses
       -- ** Header
       , gbhPrevBlock
       , gbhBodyProof
       , gbhConsensus
       , gbhExtra

       -- ** Block
       , gbBody
       , gbHeader
       , gbExtra
       , gbPrevBlock
       , gbBodyProof
       , gbConsensus
       ) where

import           Universum

import           Control.Lens         (makeLenses)
import           Control.Monad.Except (MonadError (throwError))

import           Pos.Core.Class       (HasHeaderHash (..), HasPrevBlock (..))
import           Pos.Core.Constants   (genesisHash)
import           Pos.Core.Types       (HeaderHash)

----------------------------------------------------------------------------
-- Blockchain class
----------------------------------------------------------------------------

-- | Blockchain type class generalizes some functionality common for
-- different blockchains.
class Blockchain p where
    -- | Proof of data stored in the body. Ensures immutability.
    data BodyProof p :: *
    -- | Consensus data which can be used to check consensus properties.
    data ConsensusData p :: *
    -- | Whatever extra data.
    type ExtraHeaderData p :: *
    type ExtraHeaderData p = ()
    -- | Block header used in this blockchain.
    type BBlockHeader p :: *
    type BBlockHeader p = GenericBlockHeader p
    -- | Hash of 'BBlockHeader'. This is something like @Hash (BBlockHeader p)@.
    type BHeaderHash p :: *
    type BHeaderHash p = HeaderHash

    -- | Body contains payload and other heavy data.
    data Body p :: *
    -- | Whatever extra data.
    type ExtraBodyData p :: *
    type ExtraBodyData p = ()
    -- | Block used in this blockchain.
    type BBlock p :: *
    type BBlock p = GenericBlock p

    mkBodyProof :: Body p -> BodyProof p
    checkBodyProof :: Body p -> BodyProof p -> Bool
    default checkBodyProof :: Eq (BodyProof p) => Body p -> BodyProof p -> Bool
    checkBodyProof body proof = mkBodyProof body == proof

-- | Extension of 'Blockchain' type class with helper functions.
class Blockchain p => BlockchainHelpers p where
    -- | Verify consistency of block header. This function should do
    -- all checks which can be done without any extra data.
    verifyBBlockHeader :: MonadError Text m => GenericBlockHeader p -> m ()
    -- | Verify consistency of block. This function should do
    -- all checks which can be done without any extra data.
    verifyBBlock :: MonadError Text m => GenericBlock p -> m ()

----------------------------------------------------------------------------
-- Generic types
----------------------------------------------------------------------------

-- | Header of block contains some kind of summary. There are various
-- benefits which people get by separating header from other data.
--
-- The constructor has `Unsafe' prefix in its name, because there in
-- general there may be some invariants which must hold for the
-- contents of header.
data GenericBlockHeader b = UnsafeGenericBlockHeader
    { -- | Pointer to the header of the previous block.
      _gbhPrevBlock :: !(BHeaderHash b)
    , -- | Proof of body.
      _gbhBodyProof :: !(BodyProof b)
    , -- | Consensus data to verify consensus algorithm.
      _gbhConsensus :: !(ConsensusData b)
    , -- | Any extra data.
      _gbhExtra     :: !(ExtraHeaderData b)
    } deriving (Generic)

deriving instance
    ( Show (BHeaderHash b)
    , Show (BodyProof b)
    , Show (ConsensusData b)
    , Show (ExtraHeaderData b)
    ) => Show (GenericBlockHeader b)

deriving instance
    ( Eq (BHeaderHash b)
    , Eq (BodyProof b)
    , Eq (ConsensusData b)
    , Eq (ExtraHeaderData b)
    ) => Eq (GenericBlockHeader b)

instance
    ( NFData (BHeaderHash b)
    , NFData (BodyProof b)
    , NFData (ConsensusData b)
    , NFData (ExtraHeaderData b)
    ) => NFData (GenericBlockHeader b)

-- | In general Block consists of header and body. It may contain
-- extra data as well.
--
-- The constructor has `Unsafe' prefix in its name, because there are
-- some invariants which must hold for the contents of block. For
-- instance, for generic block proof of body must correspond to the
-- body itself. Also there may be other invariants specific for
-- particular blockchains.
data GenericBlock b = UnsafeGenericBlock
    { _gbHeader :: !(GenericBlockHeader b)
    , _gbBody   :: !(Body b)
    , _gbExtra  :: !(ExtraBodyData b)
    } deriving (Generic)

deriving instance
    ( Show (GenericBlockHeader b)
    , Show (Body b)
    , Show (ExtraBodyData b)
    ) => Show (GenericBlock b)

deriving instance
    ( Eq (BHeaderHash b)
    , Eq (Body b)
    , Eq (BodyProof b)
    , Eq (ConsensusData b)
    , Eq (ExtraBodyData b)
    , Eq (ExtraHeaderData b)
    ) => Eq (GenericBlock b)

-- Derived partially in Instances
--instance
--    ( NFData (GenericBlockHeader b)
--    , NFData (Body b)
--    , NFData (ExtraBodyData b)
--    ) => NFData (GenericBlock b)

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

-- | Smart constructor for 'GenericBlockHeader'.
mkGenericHeader
    :: forall b m.
       ( HasHeaderHash (BBlockHeader b)
       , BlockchainHelpers b
       , BHeaderHash b ~ HeaderHash
       , MonadError Text m
       )
    => Maybe (BBlockHeader b)
    -> Body b
    -> (BHeaderHash b -> BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> m (GenericBlockHeader b)
mkGenericHeader prevHeader body consensus extra =
    recreateGenericHeader h proof (consensus h proof) extra
  where
    h :: HeaderHash
    h = maybe genesisHash headerHash prevHeader
    proof = mkBodyProof body

-- | Smart constructor for 'GenericBlockHeader' which allows to recreate it.
recreateGenericHeader
    :: forall b m.
       ( BlockchainHelpers b
       , MonadError Text m
       )
    => BHeaderHash b
    -> BodyProof b
    -> ConsensusData b
    -> ExtraHeaderData b
    -> m (GenericBlockHeader b)
recreateGenericHeader _gbhPrevBlock _gbhBodyProof _gbhConsensus _gbhExtra =
    res <$ verifyBBlockHeader res
  where
    res = UnsafeGenericBlockHeader {..}

-- | Smart constructor for 'GenericBlock'.
mkGenericBlock
    :: forall b m.
       ( HasHeaderHash (BBlockHeader b)
       , BlockchainHelpers b
       , BHeaderHash b ~ HeaderHash
       , MonadError Text m
       )
    => Maybe (BBlockHeader b)
    -> Body b
    -> (BHeaderHash b -> BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> ExtraBodyData b
    -> m (GenericBlock b)
mkGenericBlock prevHeader body consensus extraH extra = do
    header <- mkGenericHeader prevHeader body consensus extraH
    recreateGenericBlock header body extra

-- | Smart constructor for 'GenericBlock' which allows to recreate it.
recreateGenericBlock
    :: forall b m.
       ( BlockchainHelpers b
       , MonadError Text m
       )
    => (GenericBlockHeader b)
    -> Body b
    -> ExtraBodyData b
    -> m (GenericBlock b)
recreateGenericBlock _gbHeader _gbBody _gbExtra = do
    unless (checkBodyProof _gbBody (_gbhBodyProof _gbHeader)) $
        throwError "mkGenericBlock: incorrect proof of body"
    let res = UnsafeGenericBlock {..}
    res <$ verifyBBlock res

----------------------------------------------------------------------------
-- Lenses
----------------------------------------------------------------------------

makeLenses ''GenericBlockHeader
makeLenses ''GenericBlock

instance (BHeaderHash b ~ HeaderHash) =>
         HasPrevBlock (GenericBlockHeader b) where
    prevBlockL = gbhPrevBlock

instance (BHeaderHash b ~ HeaderHash) =>
         HasPrevBlock (GenericBlock b) where
    prevBlockL = gbHeader . gbhPrevBlock

-- | Lens from 'GenericBlock' to 'BHeaderHash' of its parent.
gbPrevBlock :: Lens' (GenericBlock b) (BHeaderHash b)
gbPrevBlock = gbHeader . gbhPrevBlock

-- | Lens from 'GenericBlock' to 'BodyProof'.
gbBodyProof :: Lens' (GenericBlock b) (BodyProof b)
gbBodyProof = gbHeader . gbhBodyProof

-- | Lens from 'GenericBlock' to 'ConsensusData'.
gbConsensus :: Lens' (GenericBlock b) (ConsensusData b)
gbConsensus = gbHeader . gbhConsensus
