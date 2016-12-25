{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Binary serialization of Pos.Types.Update module

module Pos.Binary.Update () where

import qualified Data.Text         as T
import           Universum

import           Pos.Binary.Class  (Bi (..))
import           Pos.Binary.Util   (getAsciiString1b, putAsciiString1b)
import qualified Pos.Types.Update  as U
import qualified Pos.Types.Version as V

instance Bi U.SystemTag where
    get = U.mkSystemTag =<< T.pack <$> getAsciiString1b "SystemTag" U.systemTagMaxLength
    put (T.unpack . U.getSystemTag -> tag) = putAsciiString1b tag


instance Bi U.UpdateVote where
    get = U.UpdateVote <$> get <*> get <*> get
    put U.UpdateVote {..} =  put uvKey
                          *> put uvDecision
                          *> put uvSignature

instance Bi U.UpdateData where
    get = U.UpdateData <$> get <*> get <*> get
    put U.UpdateData {..} =  put udAppDiffHash
                          *> put udPkgHash
                          *> put udUpdaterHash

instance (Bi V.ProtocolVersion, Bi V.SoftwareVersion) => Bi U.UpdateProposal where
    get = U.UpdateProposal <$> get <*> get <*> get <*> get
    put U.UpdateProposal {..} =  put upProtocolVersion
                              *> put upScriptVersion
                              *> put upSoftwareVersion
                              *> put upData
