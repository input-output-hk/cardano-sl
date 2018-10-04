{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

import           Universum

import           Control.DeepSeq (NFData (..), deepseq)
import           Criterion
import           Criterion.Main
import qualified Data.ByteString.Lazy as LBS
import           System.Environment (lookupEnv)

import           Pos.Binary.Class (Bi, serialize, unsafeDeserialize)
import           Pos.Chain.Block (MainBlock, MainBlockHeader, MainBody,
                     MainConsensusData, MainExtraBodyData, MainExtraHeaderData,
                     MainProof, gbBody, gbBodyProof, gbConsensus, gbExtra,
                     gbHeader, gbhExtra, mainBlockDlgPayload,
                     mainBlockSscPayload, mainBlockTxPayload,
                     mainBlockUpdatePayload)
import           Pos.Chain.Delegation (DlgPayload)
import           Pos.Chain.Ssc (SscPayload)
import           Pos.Chain.Txp (TxPayload (..))
import           Pos.Chain.Update (UpdatePayload)
import           Pos.Crypto (ProtocolMagic (..))

import           Test.Pos.Chain.Block.Arbitrary.Generate (generateMainBlock)

-- We need 'ProtocolMagic' and 'ProtocolConstants' in order to generate a
-- 'MainBlock'.

pm :: ProtocolMagic
pm = ProtocolMagic 0

-- | A test subject: a MainBlock, and its various components, each paired with
-- its serialization.
data TestSubject = TestSubject
    { tsBlock       :: !(MainBlock, LBS.ByteString)
    , tsHeader      :: !(MainBlockHeader, LBS.ByteString)
    , tsBodyProof   :: !(MainProof, LBS.ByteString)
    , tsConsensus   :: !(MainConsensusData, LBS.ByteString)
    , tsExtraHeader :: !(MainExtraHeaderData, LBS.ByteString)
    , tsBody        :: !(MainBody, LBS.ByteString)
    , tsTxPayload   :: !(TxPayload, LBS.ByteString)
    , tsSscPayload  :: !(SscPayload, LBS.ByteString)
    , tsDlgPayload  :: !(DlgPayload, LBS.ByteString)
    , tsUpdPayload  :: !(UpdatePayload, LBS.ByteString)
    , tsExtraBody   :: !(MainExtraBodyData, LBS.ByteString)
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
    :: Int -- ^ Seed
    -> Int -- ^ Size
    -> TestSubject
testSubject seed size =
  let block :: MainBlock
      block = generateMainBlock pm seed size

      tsBlock = withSerialized block
      tsHeader = withSerialized (block ^. gbHeader)
      tsBodyProof = withSerialized (block ^. gbBodyProof)
      tsConsensus = withSerialized (block ^. gbConsensus)
      tsExtraHeader = withSerialized (block ^. gbHeader . gbhExtra)
      tsBody = withSerialized (block ^. gbBody)
      tsTxPayload = withSerialized (block ^. mainBlockTxPayload)
      tsSscPayload = withSerialized (block ^. mainBlockSscPayload)
      tsDlgPayload = withSerialized (block ^. mainBlockDlgPayload)
      tsUpdPayload = withSerialized (block ^. mainBlockUpdatePayload)
      tsExtraBody  = withSerialized (block ^. gbExtra)

  in  TestSubject {..}

benchMain :: ( ) => Int -> Int -> IO ()
benchMain seed size = defaultMain
    [ env (printSizes (testSubject seed size)) $ \ts -> bgroup "block" $
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
                      , bench "body_proof" (nf (unsafeDeserialize :: LBS.ByteString -> MainProof) (snd . tsBodyProof $ ts))
                      , bench "consensus" (nf (unsafeDeserialize :: LBS.ByteString -> MainConsensusData) (snd . tsConsensus $ ts))
                      , bench "extra" (nf (unsafeDeserialize :: LBS.ByteString -> MainExtraHeaderData) (snd . tsExtraHeader $ ts))
                      ]
                , bgroup "body" $
                      [ bench "all" (nf (unsafeDeserialize :: LBS.ByteString -> MainBody) (snd . tsBody $ ts))
                      , bench "tx" (nf (unsafeDeserialize :: LBS.ByteString -> TxPayload) (snd . tsTxPayload $ ts))
                      , bench "ssc" (nf (unsafeDeserialize :: LBS.ByteString -> SscPayload) (snd . tsSscPayload $ ts))
                      , bench "dlg" (nf (unsafeDeserialize :: LBS.ByteString -> DlgPayload) (snd . tsDlgPayload $ ts))
                      , bench "upd" (nf (unsafeDeserialize :: LBS.ByteString -> UpdatePayload) (snd . tsUpdPayload $ ts))
                      , bench "extra" (nf (unsafeDeserialize :: LBS.ByteString -> MainExtraBodyData) (snd . tsExtraBody $ ts))
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
