{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-compat #-}
-- We are missing (MonadFail Gen), therfore [a,b,c,d] <- vectorOf 4 will trigger a warning with -compat
module Test.Spec.Submission (
    spec
  , dependentTransactions
  ) where

import           Universum hiding (elems)

import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId (..),
                     HdAccountIx (..), HdRootId (..), eskToHdRootId)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (Pending (..), emptyPending,
                     pendingTransactions, removePending)
import           Cardano.Wallet.Kernel.Submission
import           Control.Lens (at, non, to)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import           Formatting (bprint, (%))
import qualified Formatting as F
import           Formatting.Buildable (build)
import qualified Pos.Core as Core
import           Pos.Crypto.Hashing (hash)
import           Pos.Crypto.Signing.Safe (safeDeterministicKeyGen)
import           Pos.Data.Attributes (Attributes (..), UnparsedFields (..))
import           Serokell.Util.Text (listJsonIndent)
import qualified Test.Pos.Core.Arbitrary.Txp as Core

import           Cardano.Wallet.Kernel.Util (disjoint)
import           Test.QuickCheck (Gen, Property, arbitrary, choose, conjoin,
                     forAll, listOf, shuffle, vectorOf, (===))
import           Test.QuickCheck.Property (counterexample)
import           Util.Buildable (ShowThroughBuild (..))
import           Util.Buildable.Hspec

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

{-------------------------------------------------------------------------------
  QuickCheck core-based generators, which cannot be placed in the normal
  modules without having `wallet-new` depends from `cardano-sl-txp-test`.
-------------------------------------------------------------------------------}

genPending :: Core.ProtocolMagic -> Gen Pending
genPending pMagic = do
    elems <- listOf (do tx  <- Core.genTx
                        wit <- (V.fromList <$> listOf (Core.genTxInWitness pMagic))
                        aux <- Core.TxAux <$> pure tx <*> pure wit
                        pure (hash tx, aux)
                    )
    return $ emptyPending & over pendingTransactions (fmap (M.union (M.fromList elems)))


-- | An hardcoded 'HdAccountId'.
myAccountId :: HdAccountId
myAccountId = HdAccountId {
      _hdAccountIdParent = myHdRootId
    , _hdAccountIdIx     = HdAccountIx 0x42
    }
    where
        myHdRootId :: HdRootId
        myHdRootId = eskToHdRootId .
                               snd .
                               safeDeterministicKeyGen (BS.pack (replicate 32 0)) $ mempty

-- Generates a random schedule by picking a slot >= of the input one but
-- within a 'slot + 10' range, as really generating schedulers which generates
-- things too far away in the future is not very useful for testing, if not
-- testing that a scheduler will never reschedule something which cannot be
-- reached.
genSchedule :: MaxRetries -> Map HdAccountId Pending -> Slot -> Gen Schedule
genSchedule maxRetries pending (Slot lowerBound) = do
    let pendingTxs  = pending ^. at myAccountId
                               . non emptyPending
                               . pendingTransactions
                               . fromDb . to M.toList
    slots    <- vectorOf (length pendingTxs) (fmap Slot (choose (lowerBound, lowerBound + 10)))
    retries  <- vectorOf (length pendingTxs) (choose (0, maxRetries))
    let events = List.foldl' updateFn mempty (zip3 slots pendingTxs retries)
    return $ Schedule events mempty
    where
        updateFn acc (slot, (txId, txAux), retries) =
            let s = ScheduleSend myAccountId txId txAux (SubmissionCount retries)
                e = ScheduleEvents [s] mempty
            in prependEvents slot e acc

genWalletSubmissionState :: HdAccountId -> MaxRetries -> Gen WalletSubmissionState
genWalletSubmissionState accId maxRetries = do
    pending   <- M.singleton accId <$> genPending (Core.ProtocolMagic 0)
    let slot  = Slot 0 -- Make the layer always start from 0, to make running the specs predictable.
    scheduler <- genSchedule maxRetries pending slot
    return $ WalletSubmissionState pending scheduler slot

genWalletSubmission :: HdAccountId
                    -> MaxRetries
                    -> ResubmissionFunction
                    -> Gen WalletSubmission
genWalletSubmission accId maxRetries rho =
    WalletSubmission <$> pure rho <*> genWalletSubmissionState accId maxRetries

{-------------------------------------------------------------------------------
  Submission layer tests
-------------------------------------------------------------------------------}

instance (Buildable a, Buildable b) => Buildable (a,b) where
    build (a,b) = bprint ("(" % F.build % "," % F.build % ")") a b

instance Buildable [LabelledTxAux] where
    build xs = bprint (listJsonIndent 4) xs

instance (Buildable a) => Buildable (S.Set a) where
    build xs = bprint (listJsonIndent 4) (S.toList xs)

instance (Buildable a) => Buildable (Map HdAccountId a) where
    build xs = bprint (listJsonIndent 4) (M.toList xs)

constantResubmit :: ResubmissionFunction
constantResubmit = giveUpAfter 255

giveUpAfter :: Int -> ResubmissionFunction
giveUpAfter retries currentSlot scheduled oldScheduler =
    let rPolicy = constantRetry 1 retries
    in defaultResubmitFunction rPolicy currentSlot scheduled oldScheduler

-- | Checks whether or not the second input is fully contained within the first.
shouldContainPending :: Pending
                     -> M.Map HdAccountId Pending
                     -> Bool
shouldContainPending p1 p2 =
    let pending1 = p1 ^. pendingTransactions . fromDb
        pending2 = p2 ^. at myAccountId . non emptyPending . pendingTransactions . fromDb
    in pending2 `M.isSubmapOf` pending1

-- | Checks that @any@ of the input transactions (in the pending set) appears
-- in the local pending set of the given 'WalletSubmission'.
doesNotContainPending :: M.Map HdAccountId Pending
                      -> WalletSubmission
                      -> Bool
doesNotContainPending p ws =
    let pending      = p ^. at myAccountId . non emptyPending . pendingTransactions . fromDb
        localPending = ws ^. localPendingSet myAccountId . pendingTransactions . fromDb
    in M.intersection localPending pending == mempty

toTxIdSet :: Pending -> Set Core.TxId
toTxIdSet p = S.fromList $ map fst (p ^. pendingTransactions . fromDb . to M.toList)

toTxIdSet' :: M.Map HdAccountId Pending -> Set Core.TxId
toTxIdSet' p =
    S.fromList $ map fst (p ^. at myAccountId
                             . non emptyPending
                             . pendingTransactions
                             . fromDb . to M.toList
                         )

toTxIdSet'' :: M.Map HdAccountId (Set Core.TxId) -> Set Core.TxId
toTxIdSet'' p = p ^. at myAccountId . non mempty

pendingFromTxs :: [Core.TxAux] -> Pending
pendingFromTxs txs =
    let entries = map (\t -> (hash (Core.taTx t), t)) txs
    in emptyPending & (pendingTransactions . fromDb) .~ (M.fromList entries)

data LabelledTxAux = LabelledTxAux {
      labelledTxLabel :: String
    , labelledTxAux   :: Core.TxAux
    }

instance Buildable LabelledTxAux where
    build labelled =
         let tx = Core.taTx (labelledTxAux labelled)
         in bprint (F.shown % " [" % F.build % "] -> " % listJsonIndent 4) (labelledTxLabel labelled) (hash tx) (inputsOf tx)
      where
          inputsOf :: Core.Tx -> [Core.TxIn]
          inputsOf tx = NonEmpty.toList (Core._txInputs tx)

-- Generates 4 transactions A, B, C, D such that
-- D -> C -> B -> A (C depends on B which depends on A)
dependentTransactions :: Gen (LabelledTxAux, LabelledTxAux, LabelledTxAux, LabelledTxAux)
dependentTransactions = do
    let emptyAttributes = Attributes () (UnparsedFields mempty)
    inputForA  <- (Core.TxInUtxo <$> arbitrary <*> arbitrary)
    outputForA <- (Core.TxOut <$> arbitrary <*> arbitrary)
    outputForB <- (Core.TxOut <$> arbitrary <*> arbitrary)
    outputForC <- (Core.TxOut <$> arbitrary <*> arbitrary)
    outputForD <- (Core.TxOut <$> arbitrary <*> arbitrary)
    [a,b,c,d] <- vectorOf 4 (Core.genTxAux (Core.ProtocolMagic 0))
    let a' = a { Core.taTx = (Core.taTx a) {
                     Core._txInputs  = inputForA :| mempty
                   , Core._txOutputs = outputForA :| mempty
                   , Core._txAttributes = emptyAttributes
                   }
               }
    let b' = b { Core.taTx = (Core.taTx b) {
                     Core._txInputs = Core.TxInUtxo (hash (Core.taTx a')) 0 :| mempty
                   , Core._txOutputs = outputForB :| mempty
                   , Core._txAttributes = emptyAttributes
                   }
               }
    let c' = c { Core.taTx = (Core.taTx c) {
                     Core._txInputs = Core.TxInUtxo (hash (Core.taTx b')) 0 :| mempty
                   , Core._txOutputs = outputForC :| mempty
                   , Core._txAttributes = emptyAttributes
                   }
               }
    let d' = d { Core.taTx = (Core.taTx d) {
                     Core._txInputs = Core.TxInUtxo (hash (Core.taTx c')) 0 :| mempty
                   , Core._txOutputs = outputForD :| mempty
                   , Core._txAttributes = emptyAttributes
                   }
               }
    return ( LabelledTxAux "B" b'
           , LabelledTxAux "C" c'
           , LabelledTxAux "A" a'
           , LabelledTxAux "D" d'
           )

---
--- Pure generators, running in Identity
---
genPureWalletSubmission :: HdAccountId -> Gen (ShowThroughBuild WalletSubmission)
genPureWalletSubmission accId =
    STB <$> genWalletSubmission accId 255 constantResubmit

genPurePair :: Gen (ShowThroughBuild (WalletSubmission, M.Map HdAccountId Pending))
genPurePair = do
    STB layer <- genPureWalletSubmission myAccountId
    pending <- genPending (Core.ProtocolMagic 0)
    let pending' = removePending (toTxIdSet $ layer ^. localPendingSet myAccountId) pending
    pure $ STB (layer, M.singleton myAccountId pending')

class ToTxIds a where
    toTxIds :: a -> [Core.TxId]

instance ToTxIds Core.TxAux where
    toTxIds tx = [hash (Core.taTx tx)]

instance ToTxIds LabelledTxAux where
    toTxIds (LabelledTxAux _ txAux) = toTxIds txAux

instance ToTxIds a => ToTxIds [a] where
    toTxIds = mconcat . map toTxIds

instance ToTxIds Pending where
    toTxIds p = map fst . M.toList $ p ^. pendingTransactions . fromDb

instance ToTxIds ScheduleSend where
    toTxIds (ScheduleSend _ txId _ _) = [txId]

failIf :: (Buildable a, Buildable b) => String -> (a -> b -> Bool) -> a -> b -> Property
failIf label f x y =
  counterexample (show (STB x) ++ interpret res ++ show (STB y)) res
  where
    res = f x y
    interpret True  = " failIf succeeded "
    interpret False = " " <> label <> " "

isSubsetOf :: (Buildable a, Ord a) => S.Set a -> S.Set a -> Property
isSubsetOf = failIf "not infix of" S.isSubsetOf

includeEvent :: String -> ScheduleEvents -> LabelledTxAux -> Property
includeEvent label se tx =
    failIf (label <> ": doesn't include event")
           (\t s -> hash (Core.taTx (labelledTxAux t)) `List.elem` toTxIds (s ^. seToSend)) tx se

includeEvents :: String -> ScheduleEvents -> [LabelledTxAux] -> Property
includeEvents label se txs = failIf (label <> ": not includes all of") checkEvent se txs
    where
        checkEvent :: ScheduleEvents -> [LabelledTxAux] -> Bool
        checkEvent (ScheduleEvents toSend _) =
          all (\t -> hash (Core.taTx (labelledTxAux t)) `List.elem` toTxIds toSend)

mustNotIncludeEvents :: String -> ScheduleEvents -> [LabelledTxAux] -> Property
mustNotIncludeEvents label se txs = failIf (label <> ": does include one of") checkEvent se txs
    where
        checkEvent :: ScheduleEvents -> [LabelledTxAux] -> Bool
        checkEvent (ScheduleEvents toSend _) =
          all (\t -> not $ hash (Core.taTx (labelledTxAux t)) `List.elem` toTxIds toSend)

addPending' :: M.Map HdAccountId Pending
            -> WalletSubmission
            -> WalletSubmission
addPending' m ws = M.foldlWithKey' (\acc k v -> addPending k v acc) ws m

spec :: Spec
spec = do
    describe "Test wallet submission layer" $ do

      it "supports addition of pending transactions" $
          forAll genPurePair $ \(unSTB -> (submission, toAdd)) ->
              let currentSlot = submission ^. getCurrentSlot
                  submission' = addPending' toAdd submission
                  schedule = submission' ^. getSchedule
                  ((ScheduleEvents toSend _),_) = scheduledFor (mapSlot succ currentSlot) schedule
              in conjoin [
                   failIf "localPending set not updated" shouldContainPending (submission' ^. localPendingSet myAccountId) toAdd
                   -- Check that all the added transactions are scheduled for the next slot
                 , failIf "not infix of" S.isSubsetOf (toTxIdSet' toAdd) (S.fromList $ toTxIds toSend)
                 ]

      it "supports deletion of pending transactions" $
          forAll genPurePair $ \(unSTB -> (submission, toRemove)) ->
              doesNotContainPending toRemove $ remPendingById myAccountId (toTxIdSet' toRemove) submission

      it "remPending . addPending = id" $
          forAll genPurePair $ \(unSTB -> (submission, pending)) ->
              let originallyPending = submission ^. localPendingSet myAccountId
                  currentlyPending  = view (localPendingSet myAccountId)
                                           (remPendingById myAccountId
                                                           (toTxIdSet' pending)
                                                           (addPending' pending submission)
                                           )
              in failIf "the two pending set are not equal" (==) originallyPending currentlyPending

      it "increases its internal slot after ticking" $ do
          forAll (genPureWalletSubmission myAccountId) $ \(unSTB -> submission) ->
              let slotNow  = submission ^. getCurrentSlot
                  (_, _, ws') = tick submission
                  in failIf "internal slot didn't increase" (==) (ws' ^. getCurrentSlot) (mapSlot succ slotNow)

      it "constantRetry works predictably" $ do
           let policy = constantRetry 1 5
           conjoin [
                policy (SubmissionCount 0) (Slot 0) === SendIn (Slot 1)
              , policy (SubmissionCount 1) (Slot 1) === SendIn (Slot 2)
              , policy (SubmissionCount 2) (Slot 2) === SendIn (Slot 3)
              , policy (SubmissionCount 3) (Slot 3) === SendIn (Slot 4)
              , policy (SubmissionCount 4) (Slot 4) === SendIn (Slot 5)
              , policy (SubmissionCount 5) (Slot 5) === CheckConfirmedIn (Slot 6)
              ]

      it "limit retries correctly" $ do
          forAll genPurePair $ \(unSTB -> (ws, pending)) ->
              let ws' = (addPending' pending ws) & wsResubmissionFunction .~ giveUpAfter 3
                  (evicted1, _, ws1) = tick ws'
                  (evicted2, _, ws2) = tick ws1
                  (evicted3, _, ws3) = tick ws2
                  (evicted4, _, ws4) = tick ws3
                  (evicted5, _, ws5) = tick ws4
                  (evicted6, _, _) = tick ws5
              in conjoin [
                   failIf "evicted1 includes any of pending" (\e p -> disjoint (toTxIdSet' p) (toTxIdSet'' e)) evicted1 pending
                 , failIf "evicted2 includes any of pending" (\e p -> disjoint (toTxIdSet' p) (toTxIdSet'' e)) evicted2 pending
                 , failIf "evicted3 includes any of pending" (\e p -> disjoint (toTxIdSet' p) (toTxIdSet'' e)) evicted3 pending
                 , failIf "evicted4 includes any of pending" (\e p -> disjoint (toTxIdSet' p) (toTxIdSet'' e)) evicted4 pending
                 , failIf "evicted5 doesn't contain all pending" (\e p -> (toTxIdSet' p) `S.isSubsetOf` (toTxIdSet'' e)) evicted5 pending
                 , failIf "evicted6 contains something from evicted5" (\e6 e5 -> disjoint (toTxIdSet'' e5) (toTxIdSet'' e6)) evicted6 evicted5
                 ]

      describe "tickSlot" $ do
          -- Given A,B,C,D where D `dependsOn` C `dependsOn` B `dependsOn` A,
          -- check that if these 4 are all scheduled within the same slot, they
          -- are all scheduled for submission.
          it "Given D->C->B->A all in the same slot, they are all sent" $ do
              let generator = do (b,c,a,d) <- dependentTransactions
                                 ws  <- addPending myAccountId (pendingFromTxs (map labelledTxAux [a,b,c,d])) . unSTB <$> genPureWalletSubmission myAccountId
                                 txs <- shuffle [b,c,a,d]
                                 return $ STB (ws, txs)
              forAll generator $ \(unSTB -> (submission, txs)) ->
                  let currentSlot = submission ^. getCurrentSlot
                      schedule = submission ^. getSchedule
                      nxtSlot = mapSlot succ currentSlot
                      scheduledEvents = fst (scheduledFor nxtSlot schedule)
                      -- Tick directly the next slot, as 'addPending' schedules
                      -- everything for @currentSlot + 1@.
                      result = tickSlot nxtSlot submission
                  in case result of
                         (toSend, _, _) -> conjoin [
                               includeEvents "[a,b,c,d] not scheduled" scheduledEvents txs
                             , S.fromList (toTxIds txs) `isSubsetOf` S.fromList (toTxIds toSend)
                             ]

          -- Given A,B,C,D where D `dependsOn` C `dependsOn` B `dependsOn` A,
          -- if [A,B,C] are scheduled on slot 2 and [D] on slot 1, we shouldn't
          -- send anything.
          it "Given D->C->B->A, if C,B,A are in the future, D is not sent this slot" $ do
              let generator = do (b,c,a,d) <- dependentTransactions
                                 ws  <- addPending myAccountId (pendingFromTxs (map labelledTxAux [a,b,c])) . unSTB <$> genPureWalletSubmission myAccountId
                                 return $ STB (addPending myAccountId (pendingFromTxs (map labelledTxAux [d])) ((\(_,_,s) -> s) . tick $ ws), d)
              forAll generator $ \(unSTB -> (submission, d)) ->
                  let currentSlot = submission ^. getCurrentSlot
                      schedule = submission ^. getSchedule
                      nxtSlot = mapSlot succ currentSlot
                      scheduledEvents = fst (scheduledFor nxtSlot schedule)
                      -- Tick directly the next slot, as 'addPending' schedules
                      -- everything for @currentSlot + 1@.
                      result = tickSlot nxtSlot submission
                  in case result of
                         (toSend, _, _) -> conjoin [
                               includeEvent "d scheduled" scheduledEvents d
                             , failIf "is subset of"
                                         (\x y -> not $ S.isSubsetOf x y)
                                         (S.fromList (toTxIds [d]))
                                         (S.fromList (toTxIds toSend))
                             ]

          -- Given A,B,C,D where D `dependsOn` C `dependsOn` B `dependsOn` A, if:
          -- * [A,B] are scheduled on slot 1
          -- * [D] is scheduled on slot 2
          -- * [C] is scheduled on slot 3
          -- Then during slot 1 we would send both [A,B], on slot 2 we won't send
          -- anything and finally on slot 3 we would send [C,D].
          it "Given D->C->B->A, can send [A,B] now, [D,C] in the future" $ do
              let generator :: Gen (ShowThroughBuild (WalletSubmission, [LabelledTxAux]))
                  generator = do (b,c,a,d) <- dependentTransactions
                                 ws  <- addPending myAccountId (pendingFromTxs (map labelledTxAux [a,b])) . unSTB <$> genPureWalletSubmission myAccountId
                                 let (_, _, ws')  = tick ws
                                 let ws'' = addPending myAccountId (pendingFromTxs (map labelledTxAux [d])) ws'
                                 return $ STB (ws'', [a,b,c,d])

              forAll generator $ \(unSTB -> (submission1, [a,b,c,d])) ->
                  let slot1     = submission1 ^. getCurrentSlot
                      (scheduledInSlot1, confirmed1, _) = tickSlot slot1 submission1

                      -- Let's assume that @A@ and @B@ finally are adopted,
                      -- and the wallet calls 'remPending' on them.
                      modifyPending = addPending myAccountId (pendingFromTxs (map labelledTxAux [c]))
                                    . remPendingById myAccountId (toTxIdSet (pendingFromTxs (map labelledTxAux [a,b])))
                      (_, _, submission2) = (\(e,s,st) -> (e, s, modifyPending st)) . tick $ submission1

                      -- We are in slot 2 now. During slot 2, @D@ is scheduled and
                      -- we add @C@ to be sent during slot 3. However, due to
                      -- the fact @D@ is depedent on @C@, the scheduler shouldn't
                      -- schedule @D@, this slot, which will end up in the
                      -- nursery.
                      slot2 = submission2 ^. getCurrentSlot
                      (scheduledInSlot2, confirmed2, _) = tickSlot slot2 submission2
                      (_, _, submission3) = tick submission2

                      -- Finally, during slot 3, both @C@ and @D@ are sent.

                      slot3 = submission3 ^. getCurrentSlot
                      (scheduledInSlot3, confirmed3, _) = tickSlot slot3 submission3

                  in conjoin [
                         slot1 === Slot 1
                       , slot2 === Slot 2
                       , slot3 === Slot 3
                       , includeEvents "[a,b] scheduled slot 1" (ScheduleEvents scheduledInSlot1 confirmed1) [a,b]
                       , mustNotIncludeEvents "none of [a,b,c,d] was scheduled" (ScheduleEvents scheduledInSlot2 confirmed2) [a,b,c,d]
                       , includeEvents "[c,d] scheduled slot 3" (ScheduleEvents scheduledInSlot3 confirmed3) [c,d]
                       ]
