{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

import           Universum

import           Control.DeepSeq (NFData (..), deepseq)
import           Criterion
import           Criterion.Main
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Formatting (build, sformat, shown)
import           System.Environment (lookupEnv)

import           Pos.Arbitrary.Block.Generate (generateMainBlock)
import           Pos.Binary.Class (Bi, serialize, unsafeDeserialize)
import qualified Pos.Block.BHelpers as Verify
import           Pos.Core (Block, BlockHeader, BlockVersionData (..), Body, BodyProof,
                           CoinPortion (..), ConsensusData, DlgPayload, EpochIndex (..),
                           ExtraBodyData, ExtraHeaderData, MainBlock, MainBlockHeader,
                           MainBlockchain, SoftforkRule (..), SscPayload, Timestamp (..),
                           TxFeePolicy (..), TxPayload (..), UpdatePayload,
                           unsafeCoinPortionFromDouble, _gbBody, _gbExtra, _gbHeader, _gbhBodyProof,
                           _gbhConsensus, _gbhExtra, _mbDlgPayload, _mbSscPayload, _mbTxPayload,
                           _mbUpdatePayload)
import           Pos.Core.Block.Main ()
import           Pos.Core.Common (CoinPortion, SharedSeed (..))
import           Pos.Core.ProtocolConstants (ProtocolConstants (..))
import           Pos.Core.Genesis
import           Pos.Crypto (ProtocolMagic (..))

-- We need 'ProtocolMagic' and 'ProtocolConstants' in order to generate a
-- 'MainBlock'.

pm :: ProtocolMagic
pm = ProtocolMagic 0

pc :: ProtocolConstants
pc = ProtocolConstants
    { pcK = 7
    , pcVssMaxTTL = maxBound
    , pcVssMinTTL = minBound
    }

-- | A test subject: a MainBlock, and its various components, each paired with
-- its serialization.
data TestSubject = TestSubject
    { tsBlock       :: !(MainBlock, LBS.ByteString)
    , tsHeader      :: !(MainBlockHeader, LBS.ByteString)
    , tsBodyProof   :: !(BodyProof MainBlockchain, LBS.ByteString)
    , tsConsensus   :: !(ConsensusData MainBlockchain, LBS.ByteString)
    , tsExtraHeader :: !(ExtraHeaderData MainBlockchain, LBS.ByteString)
    , tsBody        :: !(Body MainBlockchain, LBS.ByteString)
    , tsTxPayload   :: !(TxPayload, LBS.ByteString)
    , tsSscPayload  :: !(SscPayload, LBS.ByteString)
    , tsDlgPayload  :: !(DlgPayload, LBS.ByteString)
    , tsUpdPayload  :: !(UpdatePayload, LBS.ByteString)
    , tsExtraBody   :: !(ExtraBodyData MainBlockchain, LBS.ByteString)
    }

instance NFData TestSubject where
    rnf (TestSubject {..}) =
        deepseq tsBlock $
          deepseq tsHeader $
          deepseq tsBodyProof $
          deepseq tsConsensus $
          deepseq tsExtraHeader $
          deepseq tsBody $
          deepseq tsTxPayload $
          deepseq tsSscPayload $
          deepseq tsDlgPayload $
          deepseq tsUpdPayload $
          deepseq tsExtraBody $
          ()

testSubjectSize :: (a, LBS.ByteString) -> Int64
testSubjectSize (_, bytes) = LBS.length bytes

printSize :: TestSubject -> String -> (TestSubject -> (a, LBS.ByteString)) -> IO ()
printSize ts name f = Universum.putStrLn $ name <> " of size " <> Universum.show (testSubjectSize (f ts)) <> " bytes"

printSizes :: TestSubject -> IO TestSubject
printSizes ts = do
    printSize ts "block" tsBlock
    printSize ts "header" tsHeader
    printSize ts "body proof" tsBodyProof
    printSize ts "consensus" tsConsensus
    printSize ts "extra header data" tsExtraHeader
    printSize ts "body" tsBody
    printSize ts "tx payload" tsTxPayload
    printSize ts "ssc payload" tsSscPayload
    printSize ts "dlg payload" tsDlgPayload
    printSize ts "upd payload" tsUpdPayload
    printSize ts "extra body data" tsExtraBody
    return ts

withSerialized :: Bi a => a -> (a, LBS.ByteString)
withSerialized a = (a, serialize a)

-- | Make a TestSubject using a seed for a PRNG and size.
testSubject
    :: ( )
    => Int -- ^ Seed
    -> Int -- ^ Size
    -> TestSubject
testSubject seed size =
  let block :: MainBlock
      block = generateMainBlock pm pc seed size

      tsBlock = withSerialized block
      tsHeader = withSerialized (_gbHeader $ block)
      tsBodyProof = withSerialized (_gbhBodyProof . _gbHeader $ block)
      tsConsensus = withSerialized (_gbhConsensus . _gbHeader $ block)
      tsExtraHeader = withSerialized (_gbhExtra . _gbHeader $ block)
      tsBody = withSerialized (_gbBody $ block)
      tsTxPayload = withSerialized (_mbTxPayload . _gbBody $ block)
      tsSscPayload = withSerialized (_mbSscPayload . _gbBody $ block)
      tsDlgPayload = withSerialized (_mbDlgPayload . _gbBody $ block)
      tsUpdPayload = withSerialized (_mbUpdatePayload . _gbBody $ block)
      tsExtraBody  = withSerialized (_gbExtra $ block)

  in  TestSubject {..}

benchMain :: ( ) => Int -> Int -> IO ()
benchMain seed size = defaultMain
    [ env (return (testSubject seed size) >>= printSizes) $ \ts -> bgroup "block" $
          [ bgroup "serialize" $
                [ bench "all" (nf serialize (fst . tsBlock $ ts))
                , bgroup "header" $
                      [ bench "all" (nf serialize (fst . tsHeader $ ts))
                      , bench "body_proof" (nf serialize (fst . tsBodyProof $ ts))
                      , bench "consensus" (nf serialize (fst . tsConsensus $ ts))
                      , bench "extra" (nf serialize (fst . tsExtraHeader $ ts))
                      ]
                , bgroup "body" $
                      [ bench "all" (nf serialize (fst . tsBody $ ts))
                      , bench "tx" (nf serialize (fst . tsTxPayload $ ts))
                      , bench "ssc" (nf serialize (fst . tsSscPayload $ ts))
                      , bench "dlg" (nf serialize (fst . tsDlgPayload $ ts))
                      , bench "upd" (nf serialize (fst . tsUpdPayload $ ts))
                      , bench "extra" (nf serialize (fst . tsExtraBody $ ts))
                      ]
                ]
          , bgroup "deserialize" $
                [ bench "all" (nf (unsafeDeserialize :: LBS.ByteString -> MainBlock) (snd . tsBlock $ ts))
                , bgroup "header" $
                      [ bench "all" (nf (unsafeDeserialize :: LBS.ByteString -> MainBlockHeader) (snd . tsHeader $ ts))
                      , bench "body_proof" (nf (unsafeDeserialize :: LBS.ByteString -> BodyProof MainBlockchain) (snd . tsBodyProof $ ts))
                      , bench "consensus" (nf (unsafeDeserialize :: LBS.ByteString -> ConsensusData MainBlockchain) (snd . tsConsensus $ ts))
                      , bench "extra" (nf (unsafeDeserialize :: LBS.ByteString -> ExtraHeaderData MainBlockchain) (snd . tsExtraHeader $ ts))
                      ]
                , bgroup "body" $
                      [ bench "all" (nf (unsafeDeserialize :: LBS.ByteString -> Body MainBlockchain) (snd . tsBody $ ts))
                      , bench "tx" (nf (unsafeDeserialize :: LBS.ByteString -> TxPayload) (snd . tsTxPayload $ ts))
                      , bench "ssc" (nf (unsafeDeserialize :: LBS.ByteString -> SscPayload) (snd . tsSscPayload $ ts))
                      , bench "dlg" (nf (unsafeDeserialize :: LBS.ByteString -> DlgPayload) (snd . tsDlgPayload $ ts))
                      , bench "upd" (nf (unsafeDeserialize :: LBS.ByteString -> UpdatePayload) (snd . tsUpdPayload $ ts))
                      , bench "extra" (nf (unsafeDeserialize :: LBS.ByteString -> ExtraBodyData MainBlockchain) (snd . tsExtraBody $ ts))
                      ]
                ]
          ]
    ]

main :: IO ()
main = do
  sizeStr <- lookupEnv "SIZE"
  seedStr <- lookupEnv "SEED"
  let size = case fmap reads sizeStr of
          Just [(size',"")] -> size'
          _                 -> 4
      seed = case fmap reads seedStr of
          Just [(seed',"")] -> seed'
          _                 -> 42
  benchMain seed size
