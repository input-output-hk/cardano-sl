{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Binary serialization of Pos.Types.Update module

module Pos.Binary.Update () where

import           Data.Binary.Get   (label)
import qualified Data.Text         as T
import           Universum

import           Pos.Binary.Class  (Bi (..))
import           Pos.Binary.Util   (getAsciiString1b, putAsciiString1b)
import qualified Pos.Types.Update  as U
import qualified Pos.Types.Version as V

instance Bi U.SystemTag where
    get = label "SystemTag" $ U.mkSystemTag =<< T.pack <$> getAsciiString1b "SystemTag" U.systemTagMaxLength
    put (T.unpack . U.getSystemTag -> tag) = putAsciiString1b tag


instance Bi U.UpdateVote where
    get = label "UpdateVote" $ U.UpdateVote <$> get <*> get <*> get
    put U.UpdateVote {..} =  put uvKey
                          *> put uvDecision
                          *> put uvSignature

instance Bi U.UpdateData where
    get = label "UpdateData" $ U.UpdateData <$> get <*> get <*> get
    put U.UpdateData {..} =  put udAppDiffHash
                          *> put udPkgHash
                          *> put udUpdaterHash

instance (Bi V.ProtocolVersion, Bi V.SoftwareVersion) => Bi U.UpdateProposal where
    get = label "UpdateProposal" $ U.UpdateProposal <$> get <*> get <*> get <*> get
    put U.UpdateProposal {..} =  put upProtocolVersion
                              *> put upScriptVersion
                              *> put upSoftwareVersion
                              *> put upData
