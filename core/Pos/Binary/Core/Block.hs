-- | Binary serialization of core block types.

module Pos.Binary.Core.Block
       (
       ) where

import           Universum

import           Data.Binary.Get    (getInt32be)
import           Data.Binary.Put    (putInt32be)

import           Pos.Binary.Class   (Bi (..), label)
import qualified Pos.Core.Block     as T
import           Pos.Core.Constants (protocolMagic)
import qualified Pos.Core.Types     as T
import           Pos.Util.Util      (eitherToFail)

-- | This instance required only for Arbitrary instance of HeaderHash
-- due to @instance Bi a => Hash a@.
instance Bi T.BlockHeaderStub where
    put _ = error "somebody tried to binary put BlockHeaderStub"
    get   = error "somebody tried to binary get BlockHeaderStub"

instance ( Bi (T.BHeaderHash b)
         , Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , T.BlockchainHelpers b
         ) =>
         Bi (T.GenericBlockHeader b) where
    put T.UnsafeGenericBlockHeader{..} = do
        putInt32be protocolMagic
        put _gbhPrevBlock
        put _gbhBodyProof
        put _gbhConsensus
        put _gbhExtra
    get =
        label "GenericBlockHeader" $ do
        blockMagic <- getInt32be
        when (blockMagic /= protocolMagic) $
            fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
        prevBlock <- get
        bodyProof <- get
        consensus <- get
        extra <- get
        eitherToFail $ T.recreateGenericHeader prevBlock bodyProof consensus extra

instance ( Bi (T.BHeaderHash b)
         , Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , Bi (T.Body b)
         , Bi (T.ExtraBodyData b)
         , T.BlockchainHelpers b
         ) =>
         Bi (T.GenericBlock b) where
    put T.UnsafeGenericBlock {..} = do
        put _gbHeader
        put _gbBody
        put _gbExtra
    get =
        label "GenericBlock" $ do
            header <- get
            body <- get
            extra <- get
            eitherToFail $ T.recreateGenericBlock header body extra
