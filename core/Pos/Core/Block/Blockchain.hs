{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module contains some general definitions related to blocks
-- and headers. The heart of this module is 'Blockchain' type class.

module Pos.Core.Block.Blockchain
       ( Blockchain (..)
       , GenericBlockHeader (..)
       , GenericBlock (..)

       -- * Smart constructors
       , mkGenericHeader
       , mkGenericBlock

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

import           Control.Lens (makeLenses)

import           Pos.Core.Class (HasHeaderHash (..), HasPrevBlock (..))
import           Pos.Core.Common (HeaderHash)
import           Pos.Core.Configuration.GenesisHash (HasGenesisHash, genesisHash)

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


----------------------------------------------------------------------------
-- Generic types
----------------------------------------------------------------------------

-- | Header of block contains some kind of summary. There are various
-- benefits which people get by separating header from other data.
--
-- The constructor has `Unsafe' prefix in its name, because there in
-- general there may be some invariants which must hold for the
-- contents of header.
data GenericBlockHeader b = UncheckedGenericBlockHeader
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
data GenericBlock b = UncheckedGenericBlock
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

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

-- | Smart constructor for 'GenericBlockHeader'.
mkGenericHeader
    :: forall b .
       ( HasHeaderHash (BBlockHeader b)
       , BHeaderHash b ~ HeaderHash
       , HasGenesisHash
       , Blockchain b
       )
    => Maybe (BBlockHeader b)
    -> Body b
    -> (BHeaderHash b -> BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> GenericBlockHeader b
mkGenericHeader prevHeader body consensus extra =
    UncheckedGenericBlockHeader h proof (consensus h proof) extra
  where
    h :: HeaderHash
    h = maybe genesisHash headerHash prevHeader
    proof = mkBodyProof body

-- | Smart constructor for 'GenericBlock'.
mkGenericBlock
    :: forall b .
       ( HasHeaderHash (BBlockHeader b)
       , BHeaderHash b ~ HeaderHash
       , HasGenesisHash
       , Blockchain b
       )
    => Maybe (BBlockHeader b)
    -> Body b
    -> (BHeaderHash b -> BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> ExtraBodyData b
    -> GenericBlock b
mkGenericBlock prevHeader body consensus extraH extra =
    UncheckedGenericBlock header body extra
  where
    header = mkGenericHeader prevHeader body consensus extraH

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
