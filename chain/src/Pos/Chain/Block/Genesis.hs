{-# LANGUAGE RecordWildCards #-}

-- | Types used in epoch boundary blocks
--   TODO: Change Genesis to EpochBoundary as it is a confusing misnomer

module Pos.Chain.Block.Genesis
       ( GenesisProof (..)
       , mkGenesisProof
       , checkGenesisProof

       , GenesisConsensusData (..)
       , gcdEpoch
       , gcdDifficulty

       , GenesisHeaderAttributes

       , GenesisExtraHeaderData (..)
       , gehAttributes

       , GenesisBody (..)
       , gbLeaders

       , GenesisBodyAttributes

       , GenesisExtraBodyData (..)
       , gebAttributes
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError)
import           Data.SafeCopy (SafeCopy (..), base, contain,
                     deriveSafeCopySimple, safeGet, safePut)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..),
                     deriveSimpleBi, encodeListLen, enforceSize)
import           Pos.Chain.Block.Util (checkBodyProof)
import           Pos.Core.Attributes (Attributes, areAttributesKnown)
import           Pos.Core.Common (ChainDifficulty, HasDifficulty (..),
                     SlotLeaders)
import           Pos.Core.Slotting (EpochIndex (..))
import           Pos.Crypto (Hash, hash)


--------------------------------------------------------------------------------
-- GenesisProof
--------------------------------------------------------------------------------

-- [CSL-199]: maybe we should use ADS.
-- | Proof of GenesisBody is just a hash of slot leaders list.
data GenesisProof = GenesisProof
    !(Hash SlotLeaders)
    deriving (Eq, Generic, Show)

instance NFData GenesisProof

instance Buildable GenesisProof where
    build (GenesisProof h) = Buildable.build h

instance Bi GenesisProof where
    encode (GenesisProof h) = encode h
    decode = GenesisProof <$> decode

instance SafeCopy GenesisProof where
    getCopy =
        contain $
        do x <- safeGet
           return $! GenesisProof x
    putCopy (GenesisProof x) =
        contain $
        do safePut x

mkGenesisProof :: GenesisBody -> GenesisProof
mkGenesisProof = GenesisProof . hash . _gbLeaders

checkGenesisProof :: MonadError Text m => GenesisBody -> GenesisProof -> m ()
checkGenesisProof = checkBodyProof mkGenesisProof


--------------------------------------------------------------------------------
-- GenesisConsensusData
--------------------------------------------------------------------------------

data GenesisConsensusData = GenesisConsensusData
    { -- | Index of the slot for which this genesis block is relevant.
      _gcdEpoch      :: !EpochIndex
    , -- | Difficulty of the chain ending in this genesis block.
      _gcdDifficulty :: !ChainDifficulty
    } deriving (Generic, Show, Eq)

instance NFData GenesisConsensusData

instance Bi GenesisConsensusData where
    encode bc =  encodeListLen 2
              <> encode (_gcdEpoch bc)
              <> encode (_gcdDifficulty bc)
    decode = do
      enforceSize "ConsensusData GenesisBlockchain" 2
      GenesisConsensusData <$> decode <*> decode

instance SafeCopy GenesisConsensusData where
    getCopy =
        contain $
        do _gcdEpoch <- safeGet
           _gcdDifficulty <- safeGet
           return $! GenesisConsensusData {..}
    putCopy GenesisConsensusData {..} =
        contain $
        do safePut _gcdEpoch
           safePut _gcdDifficulty


--------------------------------------------------------------------------------
-- GenesisHeaderAttributes
--------------------------------------------------------------------------------

-- | Represents genesis block header attributes.
type GenesisHeaderAttributes = Attributes ()


--------------------------------------------------------------------------------
-- GenesisExtraHeaderData
--------------------------------------------------------------------------------

-- | Represents genesis block header extra data
data GenesisExtraHeaderData = GenesisExtraHeaderData
    { -- | Header attributes
      _gehAttributes      :: !GenesisHeaderAttributes
    } deriving (Eq, Show, Generic)

instance NFData GenesisExtraHeaderData

instance Buildable GenesisExtraHeaderData where
    build (GenesisExtraHeaderData attrs)
        | areAttributesKnown attrs = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs


--------------------------------------------------------------------------------
-- GenesisBody
--------------------------------------------------------------------------------

-- | Body of genesis block consists of slot leaders for epoch
-- associated with this block.
data GenesisBody = GenesisBody
    { _gbLeaders :: !SlotLeaders
    } deriving (Generic, Show, Eq)

instance Bi GenesisBody where
    encode = encode . _gbLeaders
    decode = GenesisBody <$> decode

instance NFData GenesisBody

instance SafeCopy GenesisBody where
    getCopy =
        contain $
        do _gbLeaders <- safeGet
           return $! GenesisBody {..}
    putCopy GenesisBody {..} =
        contain $
        do safePut _gbLeaders


--------------------------------------------------------------------------------
-- GenesisBodyAttributes
--------------------------------------------------------------------------------

-- | Represents genesis block header attributes.
type GenesisBodyAttributes = Attributes ()


--------------------------------------------------------------------------------
-- GenesisExtraBodyData
--------------------------------------------------------------------------------

-- | Represents genesis block header extra data
data GenesisExtraBodyData = GenesisExtraBodyData
    { -- | Header attributes
      _gebAttributes      :: !GenesisBodyAttributes
    } deriving (Eq, Show, Generic)

instance NFData GenesisExtraBodyData

instance Buildable GenesisExtraBodyData where
    build (GenesisExtraBodyData attrs)
        | areAttributesKnown attrs = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs


--------------------------------------------------------------------------------
-- TH Derived Instances
--------------------------------------------------------------------------------

deriveSimpleBi ''GenesisExtraHeaderData [
    Cons 'GenesisExtraHeaderData [
        Field [| _gehAttributes :: GenesisHeaderAttributes |]
    ]]

deriveSimpleBi ''GenesisExtraBodyData [
    Cons 'GenesisExtraBodyData [
        Field [| _gebAttributes :: GenesisBodyAttributes |]
    ]]

deriveSafeCopySimple 0 'base ''GenesisExtraHeaderData
deriveSafeCopySimple 0 'base ''GenesisExtraBodyData


--------------------------------------------------------------------------------
-- GenesisConsensusData lenses
--------------------------------------------------------------------------------

makeLenses 'GenesisConsensusData

instance HasDifficulty GenesisConsensusData where
    difficultyL = gcdDifficulty


----------------------------------------------------------------------------
-- GenesisBody
----------------------------------------------------------------------------

makeLenses 'GenesisBody


----------------------------------------------------------------------------
-- GenesisExtra lenses
----------------------------------------------------------------------------

makeLenses ''GenesisExtraHeaderData
makeLenses ''GenesisExtraBodyData
