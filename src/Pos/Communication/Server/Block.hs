-- | Server which handles blocks.

module Pos.Communication.Server.Block
       ( blockListeners

       , handleBlockHeader -- tmp
       , handleBlockRequest -- tmp
       ) where

import           Control.TimeWarp.Logging (logDebug)
import           Control.TimeWarp.Rpc     (Listener (..), reply)
import           Formatting               (build, sformat, (%))
import           Serokell.Util            (VerificationRes (..), listBuilderJSON)
import           Universum

import           Pos.Communication.Types  (RequestBlock (..), ResponseMode,
                                           SendBlock (..), SendBlockHeader (..))
import           Pos.Crypto               (hash)
import           Pos.Slotting             (getCurrentSlot)
import qualified Pos.State                as St
import           Pos.WorkMode             (WorkMode)

-- | Listeners for requests related to blocks processing.
blockListeners :: WorkMode m => [Listener m]
blockListeners =
    [ Listener handleBlock
    -- , Listener handleBlockHeader
    -- , Listener handleBlockRequest
    ]

handleBlock :: WorkMode m => SendBlock -> m ()
handleBlock (SendBlock block) = do
    _ <- St.processBlock block
    notImplemented

handleBlockHeader
    :: ResponseMode m
    => SendBlockHeader -> m ()
handleBlockHeader (SendBlockHeader header) =
    whenM checkUsefulness $ reply (RequestBlock h)
  where
    h = hash $ Right header
    checkUsefulness = do
        slotId <- getCurrentSlot
        verRes <- St.mayBlockBeUseful slotId header
        case verRes of
            VerFailure errors -> do
                let fmt =
                        "Ignoring header with hash "%build%
                        " for the following reasons: "%build
                let msg = sformat fmt h (listBuilderJSON errors)
                False <$ logDebug msg
            VerSuccess -> pure True

handleBlockRequest
    :: ResponseMode m
    => RequestBlock -> m ()
handleBlockRequest (RequestBlock h) =
    maybe (pure ()) (reply . SendBlock) =<< St.getBlock h
