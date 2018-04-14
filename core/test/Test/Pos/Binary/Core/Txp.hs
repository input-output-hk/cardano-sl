module Test.Pos.Binary.Core.Txp
    ( main
    ) where

import           Universum
import           Test.Hspec (describe, it, hspec)
import           Test.QuickCheck (arbitrary)

import           Pos.Arbitrary.Txp () -- Arbitrary instances
import           Pos.Core
import           Pos.Crypto (ProtocolMagic (..))
import           Test.Pos.Binary.Class.Core


cc :: CoreConfiguration
cc = CoreConfiguration
    { ccGenesis = GCSpec genesisSpec
    , ccDbSerializeVersion = 0
    }

pc :: ProtocolConstants
pc = ProtocolConstants
    { pcK = 7
    , pcProtocolMagic = ProtocolMagic 0
    , pcVssMaxTTL = maxBound
    , pcVssMinTTL = minBound
    }

bvd :: BlockVersionData
bvd = BlockVersionData
    { bvdScriptVersion = 0
    , bvdSlotDuration  = 20000
    , bvdMaxBlockSize    = limit
    , bvdMaxHeaderSize   = limit
    , bvdMaxTxSize       = limit
    , bvdMaxProposalSize = limit
    , bvdMpcThd            = unsafeCoinPortionFromDouble 0
    , bvdHeavyDelThd       = unsafeCoinPortionFromDouble 0
    , bvdUpdateVoteThd     = unsafeCoinPortionFromDouble 0
    , bvdUpdateProposalThd = unsafeCoinPortionFromDouble 0
    , bvdUpdateImplicit = 0
    , bvdSoftforkRule     = SoftforkRule
          { srInitThd      = unsafeCoinPortionFromDouble 0
          , srMinThd       = unsafeCoinPortionFromDouble 0
          , srThdDecrement = unsafeCoinPortionFromDouble 0
          }
    , bvdTxFeePolicy      = TxFeePolicyUnknown 0 mempty
    , bvdUnlockStakeEpoch = EpochIndex { getEpochIndex = 0 }
    }
  where
    limit = fromIntegral ((2 :: Int) ^ (32 :: Int))

genesisInitializer :: GenesisInitializer
genesisInitializer = GenesisInitializer
    { giTestBalance = balance
    , giFakeAvvmBalance = FakeAvvmOptions
          { faoCount = 1
          , faoOneBalance = maxBound
          }
    , giAvvmBalanceFactor = unsafeCoinPortionFromDouble 0
    , giUseHeavyDlg = False
    , giSeed = 0
    }

balance :: TestnetBalanceOptions
balance = TestnetBalanceOptions
    { tboPoors = 1
    , tboRichmen = 1
    , tboTotalBalance = maxBound
    , tboRichmenShare = 1
    , tboUseHDAddresses = False
    }

genesisSpec :: GenesisSpec
genesisSpec = UnsafeGenesisSpec
    { gsAvvmDistr = GenesisAvvmBalances mempty
    , gsFtsSeed = SharedSeed mempty
    , gsHeavyDelegation = UnsafeGenesisDelegation mempty
    , gsBlockVersionData = bvd
    , gsProtocolConstants = pc
    , gsInitializer = genesisInitializer
    }

confDir :: FilePath
confDir = "./lib"

main :: IO ()
main =
    withCoreConfigurations cc confDir (Just (Timestamp 0)) Nothing $ hspec $ do
        describe "Bi" $ do
            it "encodedSize TxIn" $ encodedSizeProp @TxIn arbitrary
            it "encodedListSize TxIn" $ encodedListSizeProp @TxIn arbitrary

            it "encodedSize Tx" $ encodedSizePropGE @Tx arbitrary
            it "encodedListSize Tx" $ encodedListSizePropGE @Tx arbitrary

            -- it "encodedSize TxWitness" $ encodedSizePropGE @TxWitness arbitrary
            -- it "encodedListSize TxWitness" $ encodedListSizePropGE @TxWitness arbitrary
