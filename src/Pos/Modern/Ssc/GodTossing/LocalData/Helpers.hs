{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Pos.Modern.Ssc.GodTossing.LocalData.Helpers
       (
         HasGtState (..)
       , GtState
       , gtRunModify
       , gtRunRead
       ) where

import           Control.Lens                              (makeClassy)
import           Universum

import qualified Pos.Modern.Ssc.GodTossing.LocalData.Types as LD
import qualified Pos.Modern.Ssc.GodTossing.Storage.Types   as GS
import           Pos.Modern.Ssc.GodTossing.Types.Instance  ()
import           Pos.Ssc.Class.LocalData                   (MonadSscLD (modifyLocalData))
import           Pos.Ssc.GodTossing.Types.Base             (CommitmentsMap, OpeningsMap,
                                                            SharesMap, VssCertificatesMap)
import           Pos.Ssc.GodTossing.Types.Type             (SscGodTossing)
import           Pos.Types                                 (SlotId)

-- | This wrapper using for pass local and global state to
-- | functions which works with state using lens.
data GtState = GtState
     {
      -- | Set of 'Commitment's stored in blocks for current epoch. This can
      -- be calculated by 'mconcat'ing stored commitments, but it would be
      -- inefficient to do it every time we need to know if commitments is
      -- stored in blocks.
      _gtGlobalCommitments  :: !CommitmentsMap
    , -- | Openings stored in blocks
      _gtGlobalOpenings     :: !OpeningsMap
    , -- | Decrypted shares stored in blocks. These shares are guaranteed to
      -- match encrypted shares stored in 'dsGlobalCommitments'.
      _gtGlobalShares       :: !SharesMap
    , -- | VSS certificates stored in blocks (for all time, not just for
      -- current epoch)
      _gtGlobalCertificates :: !VssCertificatesMap
      -- | Local set of 'Commitment's. These are valid commitments which are
      -- known to the node and not stored in blockchain. It is useful only
      -- for the first 'k' slots, after that it should be discarded.
    , _gtLocalCommitments   :: !CommitmentsMap
    , -- | Local set of openings
      _gtLocalOpenings      :: !OpeningsMap
    , -- | Local set of decrypted shares (encrypted shares are stored in
      -- commitments).
      _gtLocalShares        :: !SharesMap
    , -- | Local set of VSS certificates
      _gtLocalCertificates  :: !VssCertificatesMap
    , -- | Last slot we are aware of.
      _gtLastProcessedSlot  :: !SlotId
    } deriving Show

makeClassy ''GtState

gtRunModify
    :: MonadSscLD SscGodTossing m
    => State GtState a -> m a
gtRunModify upd =
    modifyLocalData (
        \(g, l) ->
          let (res, newState) = runState upd (toGtState g l) in
          (res, fromGtState newState))

 -- TODO maybe should we add readLocalData :: ((SscGlobalState, SscLolalData) ->
 --                                           (a, SscLolalData)) -> m a
gtRunRead
    :: MonadSscLD SscGodTossing m
    => Reader GtState a -> m a
gtRunRead rd =
    modifyLocalData (
        \(g, l) ->
          let res = runReader rd (toGtState g l) in
          (res, l))

toGtState :: GS.GtGlobalStateM -> LD.GtLocalDataM -> GtState
toGtState g l =
    GtState
    { -- Can I simplify it?
      _gtGlobalCommitments  = GS._gsCommitments g
    , _gtGlobalOpenings     = GS._gsOpenings g
    , _gtGlobalShares       = GS._gsShares g
    , _gtGlobalCertificates = GS._gsVssCertificates g
    , _gtLocalCommitments   = LD._ldCommitments l
    , _gtLocalOpenings      = LD._ldOpenings l
    , _gtLocalShares        = LD._ldShares l
    , _gtLocalCertificates  = LD._ldCertificates l
    , _gtLastProcessedSlot  = LD._ldLastProcessedSlot l
    }

fromGtState :: GtState -> LD.GtLocalDataM
fromGtState st =
    LD.GtLocalDataM
    {
      _ldCommitments       = _gtLocalCommitments st
    , _ldOpenings          = _gtLocalOpenings st
    , _ldShares            = _gtLocalShares st
    , _ldCertificates      = _gtLocalCertificates st
    , _ldLastProcessedSlot = _gtLastProcessedSlot st
    }
