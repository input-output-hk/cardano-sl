{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Server which handles MPC-related things.

module Pos.Communication.Server.Mpc
       ( SendOpening
       , SendCommitment
       , mpcListeners
       ) where

import           Control.TimeWarp.Rpc
import           Data.Binary          (Binary)
import           Data.MessagePack     (MessagePack)
import           Universum

-- import           Pos.Crypto   (hash)
import           Pos.Crypto
import qualified Pos.State            as St
import           Pos.Types
import           Pos.WorkMode

----------------------------------------------------------------------------
-- Request types we'll be handling
----------------------------------------------------------------------------

data SendOpening =
    SendOpening PublicKey Opening
    deriving (Show, Generic)
data SendCommitment =
    SendCommitment PublicKey (Commitment, CommitmentSignature)
    deriving (Show, Generic)

instance Binary SendOpening
instance Binary SendCommitment

instance MessagePack SendOpening
instance MessagePack SendCommitment

mkMessage ''Void

mkRequest' ''SendOpening ''() ''Void
mkRequest' ''SendCommitment ''() ''Void

----------------------------------------------------------------------------
-- Actual listeners are here
----------------------------------------------------------------------------

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
