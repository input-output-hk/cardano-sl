module Test.Pos.Chain.Update.Example
       ( exampleBlockVersion
       , exampleBlockVersionData
       , exampleBlockVersionModifier
       , exampleSoftwareVersion
       , exampleSystemTag
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
                     SoftforkRule (..), SoftwareVersion (..), SystemTag (..),
                     UpAttributes, UpId, UpdateData (..), UpdatePayload (..),
                     UpdateProof, UpdateProposal, UpdateProposalToSign (..),
                     UpdateVote (..), VoteId, mkUpdateProof,
                     mkUpdateProposalWSign, mkUpdateVoteSafe)
import           Pos.Core (Coeff (..), CoinPortion (..), EpochIndex (..),
                     FlatSlotId, ScriptVersion, TxFeePolicy (..),
                     TxSizeLinear (..))
import           Pos.Crypto (ProtocolMagic (..), hash)

import           Test.Pos.Core.ExampleHelpers (exampleAttributes,
                     examplePublicKey, exampleSafeSigner, getText)
import           Test.Pos.Crypto.Bi (getBytes)

exampleBlockVersion :: BlockVersion
exampleBlockVersion = BlockVersion 1 1 1

exampleBlockVersionData :: BlockVersionData
exampleBlockVersionData = BlockVersionData
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
    pm  = ProtocolMagic 0
    bv  = exampleBlockVersion
    bvm = exampleBlockVersionModifier
    sv  = exampleSoftwareVersion
    hm  = HM.fromList $ zip (exampleSystemTags 10 5) (exampleUpdateDatas 10 5)
    ua  = exampleUpAttributes
    ss  = exampleSafeSigner 0

exampleUpdateVote :: UpdateVote
exampleUpdateVote = mkUpdateVoteSafe pm ss ui ar
  where
    pm = ProtocolMagic 0
    ss = exampleSafeSigner 0
    ui = exampleUpId
    ar = True

-- | ```type VoteId = (UpId, PublicKey, Bool)```
exampleVoteId :: VoteId
exampleVoteId = (exampleUpId, examplePublicKey, False)

exampleSoftwareVersion :: SoftwareVersion
exampleSoftwareVersion = SoftwareVersion (ApplicationName "Golden") 99
