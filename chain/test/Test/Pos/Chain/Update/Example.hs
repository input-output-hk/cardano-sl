module Test.Pos.Chain.Update.Example
       ( exampleBlockVersion
       , exampleBlockVersionData0
       , exampleBlockVersionData1
       , exampleBlockVersionData2
       , exampleBlockVersionModifier
       , exampleSoftwareVersion
       , exampleSystemTag
       , exampleUndo
       , exampleUpdateData
       , exampleUpdatePayload
       , exampleUpdateProof
       , exampleUpdateProposal
       , exampleUpdateProposalToSign
       , exampleUpdateVote
       , exampleUpAttributes
       , exampleUpId
       , exampleVoteId
       ) where

import           Universum

import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import           Data.List ((!!))
import           Data.Time.Units (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class (Raw (..))
import           Pos.Chain.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), BlockVersionModifier (..),
                     PrevValue (..), SoftforkRule (..), SoftwareVersion (..),
                     SystemTag (..), USUndo (..), UpAttributes, UpId,
                     UpdateData (..), UpdatePayload (..), UpdateProof,
                     UpdateProposal, UpdateProposalToSign (..),
                     UpdateVote (..), VoteId, mkUpdateProof,
                     mkUpdateProposalWSign, mkUpdateVoteSafe)
import           Pos.Core (Coeff (..), CoinPortion (..), EpochIndex (..),
                     FlatSlotId, ScriptVersion, TxFeePolicy (..),
                     TxSizeLinear (..))
import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..), hash)

import           Test.Pos.Core.ExampleHelpers (exampleAttributes,
                     examplePublicKey, exampleSafeSigner, exampleSlottingData,
                     getText)
import           Test.Pos.Crypto.Bi (getBytes)


exampleApplicationName :: ApplicationName
exampleApplicationName = ApplicationName "Golden"

exampleBlockVersion :: BlockVersion
exampleBlockVersion = BlockVersion 1 1 1

exampleBlockVersionData0 :: BlockVersionData
exampleBlockVersionData0 = BlockVersionData
                              (999 :: ScriptVersion)
                              (999 :: Millisecond)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (99 :: FlatSlotId)
                              sfrule
                              (TxFeePolicyTxSizeLinear tslin)
                              (EpochIndex 99)
    where
        tslin = TxSizeLinear c1' c2'
        c1' = Coeff (MkFixed 999)
        c2' = Coeff (MkFixed 77)
        sfrule = (SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99))

exampleBlockVersionData1 :: BlockVersionData
exampleBlockVersionData1 = BlockVersionData
    { bvdScriptVersion = 56903
    , bvdSlotDuration = 379 :: Millisecond
    , bvdMaxBlockSize = 0 :: Byte
    , bvdMaxHeaderSize = 2 :: Byte
    , bvdMaxTxSize = 8 :: Byte
    , bvdMaxProposalSize = 0 :: Byte
    , bvdMpcThd = CoinPortion
        { getCoinPortion = 340531572846619
        }
    , bvdHeavyDelThd = CoinPortion
        { getCoinPortion = 136666896062087
        }
    , bvdUpdateVoteThd = CoinPortion
        { getCoinPortion = 6813701006338
        }
    , bvdUpdateProposalThd = CoinPortion
        { getCoinPortion = 497008830503149
        }
    , bvdUpdateImplicit = 6473853538638325423
    , bvdSoftforkRule = SoftforkRule
        { srInitThd = CoinPortion
            { getCoinPortion = 960317883358477
            }
        , srMinThd = CoinPortion
            { getCoinPortion = 532535816427207
            }
        , srThdDecrement = CoinPortion
            { getCoinPortion = 329013992078399
            }
        }
    , bvdTxFeePolicy =
        TxFeePolicyTxSizeLinear $
        TxSizeLinear (Coeff $ MkFixed 172) (Coeff $ MkFixed 8)
    , bvdUnlockStakeEpoch = EpochIndex
        { getEpochIndex = 13346688070232230243
        }
    }

exampleBlockVersionData2 :: BlockVersionData
exampleBlockVersionData2 = BlockVersionData
    { bvdScriptVersion = 9061
    , bvdSlotDuration = 734 :: Millisecond
    , bvdMaxBlockSize = 4 :: Byte
    , bvdMaxHeaderSize = 5 :: Byte
    , bvdMaxTxSize = 6 :: Byte
    , bvdMaxProposalSize = 0 :: Byte
    , bvdMpcThd = CoinPortion
        { getCoinPortion = 236107662480767
        }
    , bvdHeavyDelThd = CoinPortion
        { getCoinPortion = 433334544134126
        }
    , bvdUpdateVoteThd = CoinPortion
        { getCoinPortion = 934785458282543
        }
    , bvdUpdateProposalThd = CoinPortion
        { getCoinPortion = 642506074573997
        }
    , bvdUpdateImplicit = 13112058965239099249
    , bvdSoftforkRule = SoftforkRule
        { srInitThd = CoinPortion
            { getCoinPortion = 46944805160625
            }
        , srMinThd = CoinPortion
            { getCoinPortion = 195823507728266
            }
        , srThdDecrement = CoinPortion
            { getCoinPortion = 747866672432320
            }
        }
    , bvdTxFeePolicy =
        TxFeePolicyTxSizeLinear $
        TxSizeLinear (Coeff $ MkFixed 31) (Coeff $ MkFixed 84)
    , bvdUnlockStakeEpoch = EpochIndex
        { getEpochIndex = 3647707432224754741
        }
    }

exampleBlockVersionModifier :: BlockVersionModifier
exampleBlockVersionModifier = BlockVersionModifier
                              (Just (999 :: ScriptVersion))
                              (Just (999 :: Millisecond))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just (99 :: FlatSlotId))
                              (Just sfrule')
                              (Just $ TxFeePolicyTxSizeLinear tslin')
                              (Just $ EpochIndex 99)
    where
        tslin' = TxSizeLinear co1 co2
        co1 = Coeff (MkFixed 999)
        co2 = Coeff (MkFixed 77)
        sfrule' = (SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99))

exampleSystemTag :: SystemTag
exampleSystemTag = (exampleSystemTags 0 1) !! 0

exampleSystemTags :: Int -> Int -> [SystemTag]
exampleSystemTags offset count = map (toSystemTag . (*offset)) [0..count-1]
  where
    toSystemTag start = SystemTag (getText start 16)

exampleUndo :: USUndo
exampleUndo = USUndo
    { unChangedBV        = HM.singleton exampleBlockVersion NoExist
    , unLastAdoptedBV    = Just exampleBlockVersion
    , unChangedProps     = HM.singleton exampleUpId NoExist
    , unChangedSV        = HM.singleton exampleApplicationName NoExist
    , unChangedConfProps = HM.singleton exampleSoftwareVersion NoExist
    , unPrevProposers    = Nothing
    , unSlottingData     = Just exampleSlottingData
    }

exampleUpAttributes :: UpAttributes
exampleUpAttributes = exampleAttributes

exampleUpdateData :: UpdateData
exampleUpdateData = (exampleUpdateDatas 10 2) !! 1

exampleUpdateDatas :: Int -> Int -> [UpdateData]
exampleUpdateDatas offset count = map (toUpdateData . (*offset)) [0..count-1]
  where
    toUpdateData start =
      let h = hash $ Raw (getBytes start 128)
      in  UpdateData h h h h

exampleUpId :: UpId
exampleUpId = hash exampleUpdateProposal

exampleUpdatePayload :: UpdatePayload
exampleUpdatePayload = UpdatePayload up uv
  where
    up = Just exampleUpdateProposal
    uv = [exampleUpdateVote]

exampleUpdateProof :: UpdateProof
exampleUpdateProof = mkUpdateProof exampleUpdatePayload

exampleUpdateProposal :: UpdateProposal
exampleUpdateProposalToSign :: UpdateProposalToSign
(exampleUpdateProposal, exampleUpdateProposalToSign) =
    ( mkUpdateProposalWSign pm bv bvm sv hm ua ss
    , UpdateProposalToSign bv bvm sv hm ua )
  where
    pm  = ProtocolMagic { getProtocolMagicId = ProtocolMagicId 0
                        , getRequiresNetworkMagic = RequiresNoMagic
                        }
    bv  = exampleBlockVersion
    bvm = exampleBlockVersionModifier
    sv  = exampleSoftwareVersion
    hm  = HM.fromList $ zip (exampleSystemTags 10 5) (exampleUpdateDatas 10 5)
    ua  = exampleUpAttributes
    ss  = exampleSafeSigner 0

exampleUpdateVote :: UpdateVote
exampleUpdateVote = mkUpdateVoteSafe pm ss ui ar
  where
    pm  = ProtocolMagic { getProtocolMagicId = ProtocolMagicId 0
                        , getRequiresNetworkMagic = RequiresNoMagic
                        }
    ss = exampleSafeSigner 0
    ui = exampleUpId
    ar = True

-- | ```type VoteId = (UpId, PublicKey, Bool)```
exampleVoteId :: VoteId
exampleVoteId = (exampleUpId, examplePublicKey, False)

exampleSoftwareVersion :: SoftwareVersion
exampleSoftwareVersion = SoftwareVersion exampleApplicationName 99
