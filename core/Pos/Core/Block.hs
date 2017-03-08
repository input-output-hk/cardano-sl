{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Core.Block
       ( Blockchain (..)
       , GenericBlockHeader (..)
       , GenericBlock (..)

       -- * Classes for overloaded accessors
       , HasPrevBlock (..)

       -- * Classes for headers
       , IsHeader
       , IsGenesisHeader
       , IsMainHeader (..)

       -- * Lenses
       , gbBody
       , gbHeader
       , gbExtra
       , gbBodyProof
       , gbhConsensus
       , gbhExtra
       , gbhPrevBlock
       , gbhBodyProof
       ) where

import           Control.Lens       (makeLenses)
import           Universum

import           Pos.Core.Types     (HasBlockVersion (..), HasDifficulty (..),
                                     HasEpochIndex (..), HasHeaderHash (..),
                                     HasSoftwareVersion (..), HeaderHash, SlotId)
import           Pos.Crypto.Signing (PublicKey)
import           Pos.Util.Util      (Some, applySome, liftLensSome)

----------------------------------------------------------------------------
-- GenericBlock
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

    verifyBBlock :: GenericBlock p -> Either Text ()


-- | Header of block contains some kind of summary. There are various
-- benefits which people get by separating header from other data.
data GenericBlockHeader b = GenericBlockHeader
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
data GenericBlock b = GenericBlock
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

makeLenses ''GenericBlockHeader
makeLenses ''GenericBlock

class HasPrevBlock s where
    prevBlockL :: Lens' s HeaderHash

instance HasPrevBlock (Some HasPrevBlock) where
    prevBlockL = liftLensSome prevBlockL

-- | Lens from 'GenericBlock' to 'BodyProof'.
gbBodyProof :: Lens' (GenericBlock b) (BodyProof b)
gbBodyProof = gbHeader . gbhBodyProof

----------------------------------------------------------------------------
-- Classes for headers
----------------------------------------------------------------------------

#define SOME_LENS_CLASS(HAS, LENS, CL)                       \
    instance HAS (Some CL) where LENS = liftLensSome LENS
#define SOME_FUNC_CLASS(HAS, FUNC, CL)                       \
    instance HAS (Some CL) where FUNC = applySome FUNC

-- Add (..) to export list when IsHeader or IsGenesisHeader get any methods

{- | A class that lets subpackages use some fields from headers without
depending on cardano-sl:

  * 'difficultyL'
  * 'epochIndexL'
  * 'prevBlockL'
  * 'headerHashG'
-}
class (HasDifficulty header
      ,HasEpochIndex header
      ,HasPrevBlock header
      ,HasHeaderHash header) =>
      IsHeader header

SOME_LENS_CLASS(HasDifficulty, difficultyL, IsHeader)
SOME_LENS_CLASS(HasEpochIndex, epochIndexL, IsHeader)
SOME_LENS_CLASS(HasPrevBlock,  prevBlockL,  IsHeader)
SOME_FUNC_CLASS(HasHeaderHash, headerHash,  IsHeader)

instance IsHeader (Some IsHeader)

-- | A class for genesis headers. Currently doesn't provide any data beyond
-- what 'IsHeader' provides.
class IsHeader header => IsGenesisHeader header

SOME_LENS_CLASS(HasDifficulty, difficultyL, IsGenesisHeader)
SOME_LENS_CLASS(HasEpochIndex, epochIndexL, IsGenesisHeader)
SOME_LENS_CLASS(HasPrevBlock,  prevBlockL,  IsGenesisHeader)
SOME_FUNC_CLASS(HasHeaderHash, headerHash,  IsGenesisHeader)

instance IsHeader        (Some IsGenesisHeader)
instance IsGenesisHeader (Some IsGenesisHeader)

{- | A class for main headers. In addition to 'IsHeader', provides:

  * 'headerSlotL'
  * 'headerLeaderKeyL'
  * 'blockVersionL'
  * 'softwareVersionL'
-}
class (IsHeader header
      ,HasBlockVersion header
      ,HasSoftwareVersion header) =>
      IsMainHeader header
  where
    -- | Id of the slot for which this block was generated.
    headerSlotL :: Lens' header SlotId
    -- | Public key of slot leader.
    headerLeaderKeyL :: Lens' header PublicKey

SOME_LENS_CLASS(HasDifficulty,      difficultyL,      IsMainHeader)
SOME_LENS_CLASS(HasEpochIndex,      epochIndexL,      IsMainHeader)
SOME_LENS_CLASS(HasPrevBlock,       prevBlockL,       IsMainHeader)
SOME_FUNC_CLASS(HasHeaderHash,      headerHash,       IsMainHeader)
SOME_LENS_CLASS(HasBlockVersion,    blockVersionL,    IsMainHeader)
SOME_LENS_CLASS(HasSoftwareVersion, softwareVersionL, IsMainHeader)

instance IsHeader     (Some IsMainHeader)
instance IsMainHeader (Some IsMainHeader) where
    headerSlotL = liftLensSome headerSlotL
    headerLeaderKeyL = liftLensSome headerLeaderKeyL
