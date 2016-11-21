{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- | This module defines methods which operate on GtLocalData.

module Pos.Ssc.GodTossing.LocalData.LocalData
       ( -- sscProcessMessage
         -- * Instances
         -- ** instance SscLocalDataClass SscGodTossing
       ) where

import           Data.Default                       (Default (def))
import           Universum

import           Pos.Ssc.Class.LocalData            (HasSscLocalData, MonadSscLD,
                                                     SscLocalDataClass (..))
import           Pos.Ssc.Class.Types                (Ssc (..))
import           Pos.Ssc.GodTossing.LocalData.Types (GtLocalData)
import           Pos.Ssc.GodTossing.Types.Instance  ()
import           Pos.Ssc.GodTossing.Types.Type      (SscGodTossing)
import           Pos.Types                          (SlotId (..), blockMpc)

type Query a = forall m. ( HasSscLocalData SscGodTossing GtLocalData
                         , MonadSscLD SscGodTossing m
                         ) => m a
type Update a = forall m. ( HasSscLocalData SscGodTossing GtLocalData
                          , MonadSscLD SscGodTossing m
                          ) => m a

instance SscLocalDataClass SscGodTossing where
    sscEmptyLocalData = def
    sscGetLocalPayloadQ = notImplemented

-- -- Should be executed before doing any updates within given slot.
-- mpcProcessNewSlot :: SlotId -> Update ()
-- mpcProcessNewSlot si@SlotId {siSlot = slotIdx} = do
--     zoom' lastVer $ do
--         unless (isCommitmentIdx slotIdx) $ dsLocalCommitments .= mempty
--         unless (isOpeningIdx slotIdx) $ dsLocalOpenings .= mempty
--         unless (isSharesIdx slotIdx) $ dsLocalShares .= mempty
--     dsLastProcessedSlot .= si

-- sscProcessMessage :: SscMessage SscGodTossing -> Update ()
-- sscProcessMessage (DSCommitments ne)     =
--     helper DSCommitments mpcProcessCommitment ne
-- sscProcessMessage (DSOpenings ne)        =
--     helper DSOpenings mpcProcessOpening ne
-- sscProcessMessage (DSSharesMulti ne)     =
--     helper DSSharesMulti mpcProcessShares ne
-- sscProcessMessage (DSVssCertificates ne) =
--     helper DSVssCertificates mpcProcessVssCertificate ne
-- sscGetLocalPayload = getLocalPayload

-- -- | Helper for sscProcessMessage
-- helper :: (NonEmpty (a, b) -> GtMessage)
--        -> (a -> b -> Update Bool)
--        -> NonEmpty (a, b)
--        -> Update (Maybe GtMessage)
-- helper c f ne = do
--     res <- toList <$> mapM (uncurry f) ne
--     let updated = map snd . filter fst . zip res . toList $ ne
--     if null updated
--       then return Nothing
--       else return $ Just . c . fromList $ updated

-- mpcProcessCommitment
--     :: PublicKey -> (Commitment, CommitmentSignature) -> Update Bool
-- mpcProcessCommitment pk c = do
--     epochIdx <- siEpoch <$> use dsLastProcessedSlot
--     ok <- readerToState $ and <$> magnify' lastVer (sequence $ checks epochIdx)
--     ok <$ when ok (zoom' lastVer $ dsLocalCommitments %= HM.insert pk c)
--   where
--     checks epochIndex =
--         [ pure . isVerSuccess $ verifySignedCommitment pk epochIndex c
--         , not . HM.member pk <$> view dsGlobalCommitments
--         , not . HM.member pk <$> view dsLocalCommitments
--         ]

-- mpcProcessOpening :: PublicKey -> Opening -> Update Bool
-- mpcProcessOpening pk o = do
--     ok <- readerToState $ and <$> sequence checks
--     ok <$ when ok (zoom' lastVer $ dsLocalOpenings %= HM.insert pk o)
--   where
--     checks = [checkOpeningAbsence pk, checkOpeningLastVer pk o]

-- -- Check that there is no opening from given public key in blocks. It is useful
-- -- in opening processing.
-- checkOpeningAbsence :: PublicKey -> Query Bool
-- checkOpeningAbsence pk =
--     magnify' lastVer $
--     (&&) <$> (notMember <$> view dsGlobalOpenings) <*>
--     (notMember <$> view dsLocalOpenings)
--   where
--     notMember = not . HM.member pk

-- mpcProcessShares :: PublicKey -> HashMap PublicKey Share -> Update Bool
-- mpcProcessShares pk s
--     | null s = pure False
--     | otherwise = do
--         -- TODO: we accept shares that we already have (but don't add them to
--         -- local shares) because someone who sent us those shares might not be
--         -- aware of the fact that they are already in the blockchain. On the
--         -- other hand, now nodes can send us huge spammy messages and we can't
--         -- ban them for that. On the third hand, is this a concern?
--         preOk <- readerToState $ checkSharesLastVer pk s
--         let mpcProcessSharesDo = do
--                 globalSharesForPK <-
--                     HM.lookupDefault mempty pk <$> use dsGlobalShares
--                 localSharesForPk <- HM.lookupDefault mempty pk <$> use dsLocalShares
--                 let s' = s `HM.difference` globalSharesForPK
--                 let newLocalShares = localSharesForPk `HM.union` s'
--                 -- Note: size is O(n), but union is also O(n + m), so
--                 -- it doesn't matter.
--                 let ok = preOk && (HM.size newLocalShares /= HM.size localSharesForPk)
--                 ok <$ (when ok $ dsLocalShares . at pk .= Just newLocalShares)
--         zoom' lastVer $ mpcProcessSharesDo

-- mpcProcessVssCertificate :: PublicKey -> VssCertificate -> Update Bool
-- mpcProcessVssCertificate pk c = zoom' lastVer $ do
--     ok <- not . HM.member pk <$> use dsGlobalCertificates
--     ok <$ when ok (dsLocalCertificates %= HM.insert pk c)


-- mpcProcessBlock :: Block ssc -> Update ()
-- mpcProcessBlock blk = do
--     lv <- use lastVer
--     case blk of
--         -- Genesis blocks don't contain anything interesting, but when they
--         -- “arrive”, we clear global commitments and other globals. Not
--         -- certificates, though, because we don't want to make nodes resend
--         -- them in each epoch.
--         Left _  -> do
--             undefined
--             --TODO clear local
--         Right b -> do
--             let blockCommitments  = b ^. blockMpc . mdCommitments
--                 blockOpenings     = b ^. blockMpc . mdOpenings
--                 blockShares       = b ^. blockMpc . mdShares
--                 blockCertificates = b ^. blockMpc . mdVssCertificates
--             dsLocalCommitments  %= (`HM.difference` blockCommitments)
--             -- openings
--             dsLocalOpenings  %= (`HM.difference` blockOpenings)
--             -- shares
--             dsLocalShares  %= (`diffDoubleMap` blockShares)
--             -- VSS certificates
--             dsLocalCertificates  %= (`HM.difference` blockCertificates)

-- -- | Get local payload part
-- getLocalPayload :: SlotId -> Query GtPayload
-- getLocalPayload slotId = filterGtPayload slotId <$> getStoredLocalPayload

-- getStoredLocalPayload :: Query GtPayload
-- getStoredLocalPayload =
--     magnify' lastVer $
--     GtPayload <$> view dsLocalCommitments <*> view dsLocalOpenings <*>
--     view dsLocalShares <*> view dsLocalCertificates

-- -- | Remove messages irrelevant to given slot id from payload.
-- filterGtPayload :: SlotId -> GtPayload -> GtPayload
-- filterGtPayload slotId GtPayload {..} =
--     GtPayload
--     { _mdCommitments = filteredCommitments
--     , _mdOpenings = filteredOpenings
--     , _mdShares = filteredShares
--     , ..
--     }
--   where
--     filteredCommitments = filterDo isCommitmentId _mdCommitments
--     filteredOpenings = filterDo isOpeningId _mdOpenings
--     filteredShares = filterDo isSharesId _mdShares
--     filterDo
--         :: Monoid container
--         => (SlotId -> Bool) -> container -> container
--     filterDo checker container
--         | checker slotId = container
--         | otherwise = mempty
