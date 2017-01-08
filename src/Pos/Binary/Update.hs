{-# LANGUAGE UndecidableInstances #-}

-- | Binary serialization of Pos.Update.Types module

module Pos.Binary.Update () where

import           Data.Binary        (Binary)
import           Data.Binary.Get    (label)
import qualified Data.Text          as T
import           Universum

import           Pos.Binary.Class   (Bi (..))
import           Pos.Binary.Util    (getAsciiString1b, putAsciiString1b)
import           Pos.Binary.Version ()
import qualified Pos.Update.Types   as U

instance Bi U.SystemTag where
    get =
        label "SystemTag" $
        U.mkSystemTag =<< T.pack <$> getAsciiString1b "SystemTag" U.systemTagMaxLength
    put (T.unpack . U.getSystemTag -> tag) = putAsciiString1b tag

instance Bi U.UpdateVote where
    get = label "UpdateVote" $ U.UpdateVote <$> get <*> get <*> get <*> get
    put U.UpdateVote {..} =  put uvKey
                          *> put uvProposalId
                          *> put uvDecision
                          *> put uvSignature

instance Bi U.UpdateData where
    get = label "UpdateData" $ U.UpdateData <$> get <*> get <*> get
    put U.UpdateData {..} =  put udAppDiffHash
                          *> put udPkgHash
                          *> put udUpdaterHash

instance Bi U.UpdateProposal where
    get = label "UpdateProposal" $ U.UpdateProposal <$> get <*> get <*> get <*> get
    put U.UpdateProposal {..} =  put upProtocolVersion
                              *> put upScriptVersion
                              *> put upSoftwareVersion
                              *> put upData

-- These types are used only for DB. But it still makes sense to
-- define serialization manually I suppose.
-- [CSL-124]
instance Binary U.VoteState
instance Bi U.VoteState

