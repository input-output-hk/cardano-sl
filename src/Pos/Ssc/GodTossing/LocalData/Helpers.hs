{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Pos.Ssc.GodTossing.LocalData.Helpers
       (
         HasGtState (..)
       , GtState
       , gtRunModify
       , gtRunRead
       ) where

import           Control.Lens                       (makeClassy)
import           Universum

import           Pos.Ssc.Extra                      (MonadSscLD (modifyLocalData))
import qualified Pos.Ssc.GodTossing.LocalData.Types as LD
import           Pos.Ssc.GodTossing.Types           (CommitmentsMap, GtGlobalState,
                                                     OpeningsMap, SharesMap,
                                                     SscGodTossing, VssCertificatesMap,
                                                     _gsCommitments, _gsOpenings,
                                                     _gsShares, _gsVssCertificates)
import           Pos.Types                          (SlotId)

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

toGtState :: GtGlobalState -> LD.GtLocalData -> GtState
toGtState g l =
    GtState
    { -- Can I simplify it?
      _gtGlobalCommitments  = _gsCommitments g
    , _gtGlobalOpenings     = _gsOpenings g
    , _gtGlobalShares       = _gsShares g
    , _gtGlobalCertificates = _gsVssCertificates g
    , _gtLocalCommitments   = LD._ldCommitments l
    , _gtLocalOpenings      = LD._ldOpenings l
    , _gtLocalShares        = LD._ldShares l
    , _gtLocalCertificates  = LD._ldCertificates l
    , _gtLastProcessedSlot  = LD._ldLastProcessedSlot l
    }

fromGtState :: GtState -> LD.GtLocalData
fromGtState st =
    LD.GtLocalData
    {
      _ldCommitments       = _gtLocalCommitments st
    , _ldOpenings          = _gtLocalOpenings st
    , _ldShares            = _gtLocalShares st
    , _ldCertificates      = _gtLocalCertificates st
    , _ldLastProcessedSlot = _gtLastProcessedSlot st
    }
