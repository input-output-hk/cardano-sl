{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators   #-}

-- API server logic

module Pos.Explorer.Web.Server
       ( explorerServeImpl
       , explorerApp
       , explorerHandlers
       ) where

import           Control.Monad.Catch            (try)
import           Network.Wai                    (Application)
import           Servant.API                    ((:<|>) ((:<|>)))
import           Servant.Server                 (Handler, Server, ServerT, serve)
import           Universum

import           Pos.Communication              (SendActions)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Web                        (serveImpl)
import           Pos.WorkMode                   (WorkMode)

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CBlockEntry (..), CTxEntry (..))
import           Pos.Explorer.Web.Error         (ExplorerError (..))

----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------

type ExplorerMode m = WorkMode SscGodTossing m

explorerServeImpl :: ExplorerMode m => m Application -> Word16 -> m ()
explorerServeImpl = serveImpl

explorerApp :: ExplorerMode m => m (Server ExplorerApi) -> m Application
explorerApp serv = serve explorerApi <$> serv

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

explorerHandlers :: ExplorerMode m => SendActions m -> ServerT ExplorerApi m
explorerHandlers sendActions =
    catchExplorerError ... getLastBlocks
    :<|>
    catchExplorerError ... getLastTxs
  where
    catchExplorerError = try
    f ... g = (f .) . g

getLastBlocks :: ExplorerMode m => Maybe Word -> Maybe Word -> m [CBlockEntry]
getLastBlocks _ _ = return []

getLastTxs :: ExplorerMode m => Maybe Word -> Maybe Word -> m [CTxEntry]
getLastTxs _ _ = return []
