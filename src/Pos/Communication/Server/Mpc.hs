{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Server which handles MPC-related things.

module Pos.Communication.Server.Mpc
       ( SendOpening
       , SendCommitment
       , mpcListeners
       ) where

import           Control.TimeWarp.Rpc        (Listener (..))
import           Universum

import           Pos.Communication.Types.Mpc (SendCommitment (..), SendOpening (..))
import qualified Pos.State                   as St
import           Pos.WorkMode                (WorkMode)

mpcListeners :: WorkMode m => [Listener m]
mpcListeners = [Listener handleCommitment, Listener handleOpening]

handleCommitment :: WorkMode m => SendCommitment -> m ()
handleCommitment (SendCommitment pk c) = do
    -- TODO: actually check the commitment?
    _ <- St.processCommitment pk c
    return ()

handleOpening :: WorkMode m => SendOpening -> m ()
handleOpening (SendOpening pk o) = do
    -- TODO: actually check the commitment?
    _ <- St.processOpening pk o
    return ()
