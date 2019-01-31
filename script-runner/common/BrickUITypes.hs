{-# LANGUAGE NoImplicitPrelude #-}

module BrickUITypes (AppState(..), Name(..), CustomEvent(..), Reply(..), SlotStart(..), NodeInfo(..), MenuChoice(..), CurrentScreen(..)) where

import qualified Brick.BChan as B
import qualified Brick.Widgets.List as L
import           Pos.Core (EpochOrSlot)
import           Universum

import           Pos.Chain.Update (ConfirmedProposalState, ProposalState, UpId)

data AppState = AppState
  { asLocalHeight      :: Word64
  , asGlobalHeight     :: Maybe Word64
  , asLastMsg          :: String
  , asLocalEpochOrSlot :: Maybe EpochOrSlot
  , asReplyChan        :: B.BChan Reply
  , asChoiceList       :: L.List Name MenuChoice
  , asCurrentScreen    :: CurrentScreen
  , asProposalList     :: L.List Name (Either ConfirmedProposalState (UpId,ProposalState))
  }

data MenuChoice = ListProposals | Dummy1 | Dummy2 deriving (Show, Eq)
data CurrentScreen = MainScreen | ProposalListing deriving (Show, Eq)

data Reply = TriggerShutdown | QueryProposals

data SlotStart = SlotStart
  { ssEpoch :: Word64
  , ssSlot  :: Word16
  } deriving Show

data NodeInfo = NodeInfo
  { niLocalHeight      :: Word64
  , niLocalEpochOrSlot :: EpochOrSlot
  , niGlobalHeight     :: Maybe Word64
  } deriving Show

data CustomEvent
    = CESlotStart SlotStart
    | CENodeInfo NodeInfo
    | QuitEvent
    | ProposalReply [ConfirmedProposalState] [(UpId,ProposalState)]
    deriving Show

data Name = ProposalName | MainMenu | None deriving (Show, Ord, Eq)
