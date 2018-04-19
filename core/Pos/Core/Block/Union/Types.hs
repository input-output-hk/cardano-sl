-- | Union of blockchain types.

module Pos.Core.Block.Union.Types
       ( BlockHeader (BlockHeaderGenesis, BlockHeaderMain)
       , _BlockHeaderGenesis
       , _BlockHeaderMain
       , choosingBlockHeader
       , Block
       , ComponentBlock (..)

       , blockHeaderHash
       , blockHeaderProtocolMagic

       , module Pos.Core.Block.Genesis.Types
       , module Pos.Core.Block.Main.Types
       ) where

import           Control.Lens (LensLike', makePrisms)
import           Universum

import           Pos.Binary.Class (Bi)
import           Pos.Core.Common (BlockHeader, HeaderHash)
import           Pos.Crypto (ProtocolMagic, unsafeHash)
import           Pos.Core.Block.Blockchain (GenericBlockHeader (..))
-- Re-exports
import           Pos.Core.Block.Genesis.Types
import           Pos.Core.Block.Main.Types
import           Pos.Core.Class (IsGenesisHeader, IsMainHeader (..))
import           Pos.Util.Some (Some)

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

-- | Either header of ordinary main block or genesis block.
data instance BlockHeader
    = BlockHeaderGenesis GenesisBlockHeader
    | BlockHeaderMain MainBlockHeader

deriving instance Generic BlockHeader
deriving instance (Eq GenesisBlockHeader, Eq MainBlockHeader) => Eq BlockHeader
deriving instance (Show GenesisBlockHeader, Show MainBlockHeader) => Show BlockHeader

makePrisms 'BlockHeaderGenesis

choosingBlockHeader :: Functor f =>
       LensLike' f GenesisBlockHeader r
    -> LensLike' f MainBlockHeader r
    -> LensLike' f BlockHeader r
choosingBlockHeader onGenesis onMain f = \case
    BlockHeaderGenesis bh -> BlockHeaderGenesis <$> onGenesis f bh
    BlockHeaderMain bh -> BlockHeaderMain <$> onMain f bh

-- | Block.
type Block = Either GenesisBlock MainBlock

-- | Representation of 'Block' passed to a component.
data ComponentBlock payload =
    ComponentBlockGenesis (Some IsGenesisHeader)
    | ComponentBlockMain
       { bcmHeader  :: !(Some IsMainHeader)
       , bcmPayload :: !payload }

-- | This function is required because type inference fails in attempts to
-- hash only @Right@ or @Left@.
--
-- Perhaps, it shouldn't be here, but I decided not to create a module
-- for only this function.
blockHeaderHash :: Bi BlockHeader => BlockHeader -> HeaderHash
blockHeaderHash = unsafeHash

-- | The 'ProtocolMagic' in a 'BlockHeader'.
blockHeaderProtocolMagic :: BlockHeader -> ProtocolMagic
blockHeaderProtocolMagic (BlockHeaderGenesis gbh) = _gbhProtocolMagic gbh
blockHeaderProtocolMagic (BlockHeaderMain mbh)    = _gbhProtocolMagic mbh
