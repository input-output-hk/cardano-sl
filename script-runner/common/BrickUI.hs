{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BrickUI (runUI) where

import           BrickUITypes (AppState (AppState, asChoiceList, asCurrentScreen, asGlobalHeight, asLastMsg, asLocalEpochOrSlot, asLocalHeight, asProposalList, asReplyChan),
                     CurrentScreen (MainScreen, ProposalListing),
                     CustomEvent (CENodeInfo, CESlotStart, ProposalReply, QuitEvent),
                     MenuChoice (Dummy1, Dummy2, ListProposals),
                     Name (MainMenu, None, ProposalName),
                     NodeInfo (NodeInfo, niGlobalHeight, niLocalEpochOrSlot, niLocalHeight),
                     Reply (QueryProposals, TriggerShutdown),
                     SlotStart (SlotStart))

import           Control.Concurrent.Async.Lifted.Safe (Async, async)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Vector as V
import           Formatting
import           Universum hiding (HashMap, list, on, state)

import           Brick (App (App, appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent),
                     BrickEvent (AppEvent, VtyEvent), EventM, Next, Widget,
                     continue, customMain, emptyWidget, hBox, halt, on, padAll,
                     padLeftRight, showFirstCursor, str, strWrap, txt, vBox,
                     withBorderStyle)
import qualified Brick.AttrMap as A
import           Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V

import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Update (ConfirmedProposalState (cpsAdopted, cpsConfirmed, cpsDecided, cpsImplicit, cpsNegativeStake, cpsPositiveStake, cpsProposed, cpsUpdateProposal, cpsVotes),
                     DecidedProposalState (dpsDecision, dpsDifficulty, dpsExtra, dpsUndecided),
                     ProposalState (PSDecided, PSUndecided), StakeholderVotes,
                     SystemTag (getSystemTag),
                     UndecidedProposalState (upsExtra, upsNegativeStake, upsPositiveStake, upsProposal, upsSlot, upsVotes),
                     UpId, UpdateData,
                     UpdateProposal (upData, upSoftwareVersion))
import           Pos.Core (Coin, EpochOrSlot (EpochOrSlot), getCoin)
import           Pos.Core.Slotting (EpochIndex (EpochIndex),
                     LocalSlotIndex (UnsafeLocalSlotIndex), SlotId (SlotId))

runUI :: IO (BChan CustomEvent, BChan Reply, Async ())
runUI = do
  eventChan <- newBChan 10
  replyChan <- newBChan 10
  let
    app = App
      { appDraw = drawUi
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent
      , appStartEvent = \x -> pure x
      , appAttrMap = const $ theMap
      }
    go :: IO ()
    go = do
      _ <- customMain (V.mkVty V.defaultConfig) (Just eventChan) app (defaultState replyChan)
      writeBChan replyChan TriggerShutdown
      pure ()
  brick <- async go
  pure (eventChan, replyChan, brick)

defaultState :: BChan Reply -> AppState
defaultState replyChan = AppState 0 Nothing "" Nothing replyChan defaultList MainScreen emptyList

defaultList :: L.List Name MenuChoice
defaultList = L.list MainMenu (V.fromList [ ListProposals, Dummy1, Dummy2 ]) 1

emptyList :: L.List Name a
emptyList = L.list None (V.fromList []) 1

localHeight :: AppState -> Widget Name
localHeight AppState{asLocalHeight} = str $ "Local Block Count: " <> show asLocalHeight

globalHeight :: AppState -> Widget Name
globalHeight AppState{asGlobalHeight} = str $ "global: " <> maybe "unknown" show asGlobalHeight

progressBar :: AppState -> Widget Name
progressBar AppState{asLocalHeight,asGlobalHeight} = do
  let
    fmt :: Format Text (Float -> Text)
    fmt = "Percent: " % float % "%"
    go :: Maybe Word64 -> Widget Name
    go (Just global) = do
      let
        percent :: Float
        percent = (fromIntegral asLocalHeight) / (fromIntegral global)
      P.progressBar (Just $ T.unpack $ sformat fmt (percent * 100)) percent
    go Nothing = emptyWidget
  go asGlobalHeight

lastMessage :: AppState -> Widget Name
lastMessage AppState{asLastMsg} = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "last debug msg") $ padAll 1 $ strWrap asLastMsg

currentTip :: AppState -> Widget Name
currentTip AppState{asLocalEpochOrSlot} = go asLocalEpochOrSlot
  where
    go :: Maybe EpochOrSlot -> Widget Name
    go (Just eors) = txt $ sformat fmt eors
    go Nothing     = txt "Local Slot unknown"
    fmt :: Format r (EpochOrSlot -> r)
    fmt = "Local Slot: " % fmtEpochOrSlot

fmtEpochOrSlot :: Format r (EpochOrSlot -> r)
fmtEpochOrSlot = later f
  where
    f :: EpochOrSlot -> T.Builder
    f (EpochOrSlot (Left (EpochIndex epoch))) = (T.fromText "EBB#") <> (T.fromString $ show epoch)
    f (EpochOrSlot (Right (SlotId (EpochIndex epoch) (UnsafeLocalSlotIndex slot)))) = (T.fromText "Epoch ") <> (T.fromString $ show epoch) <> (T.fromText " Slot ") <> (T.fromString $ show slot)

drawUi :: AppState -> [ Widget Name ]
drawUi state = do
  let
    debugMsg = lastMessage state
    currentWindow :: Widget Name
    currentWindow = case (asCurrentScreen state) of
        MainScreen      -> ui state
        ProposalListing -> proposalUi state
  [ vBox [ debugMsg, currentWindow ] ]

ui :: AppState -> Widget Name
ui state = vBox [ localHeight state, globalHeight state, progressBar state, currentTip state, actionsList state ]

renderUpData :: (SystemTag,UpdateData) -> Widget Name
renderUpData (tag,_) = padLeftRight 1 $ txt $ getSystemTag tag

renderUpdateProposalLabel :: Widget Name -> UpdateProposal -> Widget Name
renderUpdateProposalLabel lbl prop = hBox ([ lbl, version ] <> (map renderUpData updata))
  where
    updata :: [ (SystemTag, UpdateData) ]
    updata = HM.toList $ upData prop
    version :: Widget Name
    version = padLeftRight 1 $ str $ show $ upSoftwareVersion prop

proposalUi :: AppState -> Widget Name
proposalUi state = do
  let
    renderProposal :: Either ConfirmedProposalState (UpId,ProposalState) -> Widget Name
    renderProposal (Left proposal) = renderUpdateProposalLabel (str "Confirmed: ") (cpsUpdateProposal proposal)
    renderProposal (Right (_upid, PSUndecided prop)) = renderUpdateProposalLabel (str "Undecided: ") (upsProposal prop)
    renderProposal (Right (_upid, PSDecided prop)) = renderUpdateProposalLabel (str "Decided: ") (upsProposal $ dpsUndecided prop)
  let
    renderProposalState :: (UpId, ProposalState) -> Widget Name
    renderProposalState (upid, PSDecided prop) = do
      let
        decision, undecided, difficulty, extra :: Widget Name
        decision = str $ "Decision: " <> show (dpsDecision prop)
        undecided = renderProposalState (upid, PSUndecided $ dpsUndecided prop)
        difficulty = str $ "Difficulty: " <> show (dpsDifficulty prop)
        extra = str $ show $ dpsExtra prop
      vBox [ decision, undecided, difficulty, extra ]
    renderProposalState (upid', PSUndecided prop) = do
      let
        upid, votes, proposal, slot, extra :: Widget Name
        upid = str $ "UpId: " <> show upid'
        votes = renderVotes $ upsVotes prop
        proposal = renderUpdateProposal $ upsProposal prop
        slot = str $ "Slot: " <> show (upsSlot prop)
        extra = str $ "Extra: " <> show (upsExtra prop)
      vBox ([ upid, proposal, votes, slot, extra ] <> renderPosNeg upsPositiveStake upsNegativeStake prop)
    proposalList = L.renderList (const $ renderProposal) True (asProposalList state)
    details = case L.listSelectedElement (asProposalList state) of
      Just (_, Left proposal) -> renderFullProposal proposal
      Just (_, Right prop)    -> renderProposalState prop
      Nothing                 -> emptyWidget
    proposalListing = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Proposals") $ padLeftRight 1 proposalList
  vBox [ proposalListing, details ]

-- for future use:
-- getHeaderEpochOrSlot :: MonadDBRead m => HeaderHash -> m (Maybe EpochOrSlot)
-- exists

renderVotes :: StakeholderVotes -> Widget Name
renderVotes votes = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "StakeholderVotes") $ strWrap $ "Votes: " <> show (length $ toList votes) <> " " <> show votes

renderUpdateProposal :: UpdateProposal -> Widget Name
renderUpdateProposal prop = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "UpdateProposal") $ strWrap $ show prop

renderPosNeg :: (a -> Coin) -> (a -> Coin) -> a -> [ Widget Name ]
renderPosNeg pos neg thing = [ posWidget, negWidget ]
  where
    posWidget, negWidget :: Widget Name
    posWidget = str $ "Positive: " <> show (getCoin $ pos thing)
    negWidget = str $ "Negative: " <> show (getCoin $ neg thing)

renderFullProposal :: ConfirmedProposalState -> Widget Name
renderFullProposal prop = do
  let
    proposal, implicit, proposed, decided, confirmed, adopted :: Widget Name
    proposal = renderUpdateProposal $ cpsUpdateProposal prop
    implicit = str $ "Implicit: " <> if (cpsImplicit prop) then "Yes" else "no"
    hhf :: String -> (ConfirmedProposalState -> HeaderHash) -> Widget Name
    hhf lbl hh = str $ lbl <> show (hh prop)
    proposed = hhf "Proposed: " cpsProposed
    decided = hhf "Decided: " cpsDecided
    confirmed = hhf "Confirmed: " cpsConfirmed
    adopted = str $ "Adopted: " <> maybe "not yet (or no BV changes)" show (cpsAdopted prop)
    votes = renderVotes $ cpsVotes prop
  vBox ([ proposal, implicit, proposed, decided, confirmed, adopted, votes ] <> renderPosNeg cpsPositiveStake cpsNegativeStake prop)

actionsList :: AppState -> Widget Name
actionsList state = do
  let
    renderRow :: Bool -> MenuChoice -> Widget Name
    renderRow _ name = str $ show name
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Main Menu") $ padLeftRight 1 $ L.renderList renderRow True (asChoiceList state)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,            V.white `on` V.blue)
  , (L.listSelectedAttr,    V.blue `on` V.white)
  , (P.progressCompleteAttr, V.blue `on` V.green)
  , (P.progressIncompleteAttr, V.blue `on` V.red)
  ]

handleEvent :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent state (VtyEvent evt) = do
  case (asCurrentScreen state) of
    MainScreen -> do
      let
        openThing :: EventM Name (Next AppState)
        openThing = do
          case L.listSelectedElement (asChoiceList state) of
            Just (_, ListProposals) -> do
              liftIO $ writeBChan (asReplyChan state) QueryProposals
              continue $ state
            Just (_, item) -> do
              continue $ state { asLastMsg = show item }
            Nothing -> do
              continue state
      case evt of
        V.EvKey (V.KChar 'q') [] -> do
          halt state
        V.EvKey V.KEnter [] -> openThing
        V.EvKey V.KRight [] -> openThing
        _ -> do
          newlist <- L.handleListEventVi L.handleListEvent evt (asChoiceList state)
          continue $ state { asLastMsg = show evt, asChoiceList = newlist }
    ProposalListing -> do
      case evt of
        V.EvKey (V.KChar 'q') [] -> continue $ state { asCurrentScreen = MainScreen }
        V.EvKey V.KLeft []       -> continue $ state { asCurrentScreen = MainScreen }
        _ -> do
          newlist <- L.handleListEventVi L.handleListEvent evt (asProposalList state)
          continue $ state { asLastMsg = show evt, asProposalList = newlist }

handleEvent state (AppEvent ae) = do
  case ae of
    CENodeInfo (NodeInfo{niLocalHeight,niGlobalHeight,niLocalEpochOrSlot}) -> do
      continue $ state
        { asLocalHeight = niLocalHeight
        , asGlobalHeight = niGlobalHeight
        , asLocalEpochOrSlot = Just niLocalEpochOrSlot
        }
    QuitEvent -> halt state
    CESlotStart (SlotStart e s) -> continue $ state { asLastMsg = (show e) <> " " <> (show s) }
    ProposalReply proposals other -> do
      let
        list = (map Left proposals) <> (map Right other)
      continue $ state
        { asCurrentScreen = ProposalListing
        , asProposalList = L.list ProposalName (V.fromList list) 1
        }

handleEvent state evt = do
  continue $ state { asLastMsg = show evt }
