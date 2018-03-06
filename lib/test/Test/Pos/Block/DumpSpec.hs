-- | This module tests blockchain dumps.

module Test.Pos.Block.DumpSpec
       ( spec
       ) where

import           Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

import           Pos.Arbitrary.Block ()
import           Pos.Block.Dump (decodeBlockDump, encodeBlockDump)
import qualified Pos.Communication ()
import qualified Pos.Core.Block as BT
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Util.Chrono (OldestFirst (..))

import           Test.Pos.Configuration (withDefConfiguration)

spec :: Spec
spec = withDefConfiguration $ describe "Blockchain dump" $ do
    prop "decode . encode === identity" $
        QC.forAll (QC.choose (0, 30))    $ \blockCount ->
        QC.forAll (QC.vector blockCount) $ \blocks ->
        QC.forAll (QC.choose (1, 10000)) $ \chunkSize ->
            decodeEncodeIdentity blocks chunkSize
    -- TODO: write tests for failures

decodeEncodeIdentity
    :: HasConfiguration
    => [BT.Block]            -- ^ Blocks to be encoded
    -> Int                   -- ^ Chunk size
    -> QC.Property
decodeEncodeIdentity blocks chunkSize = QC.monadicIO $ do
    enc <- QC.run $ encodeBlockDump $ OldestFirst blocks
    dec <- QC.run $ decodeBlockDump $
        BSL.fromChunks $ chunkBS chunkSize $ BSL.toStrict enc
    pure (OldestFirst blocks === dec)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

chunkBS :: Int -> ByteString -> [ByteString]
chunkBS n = takeWhile (not . null) . map (BS.take n) . iterate (BS.drop n)
