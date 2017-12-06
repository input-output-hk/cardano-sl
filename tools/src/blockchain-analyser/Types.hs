{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Types ( initBlockchainAnalyser
             , DBFolderStat
             , BlockchainInspector
             , prevBlock
             ) where

import           Universum

import           Control.Lens (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           Ether.Internal (HasLens (..))
import           Mockable (Production)

import           Pos.Core (HasConfiguration, HeaderHash, prevBlockL)
import           Pos.Core.Block (Block)
import           Pos.DB (MonadDBRead (..))
import qualified Pos.DB as DB
import qualified Pos.DB.Block as BDB
import           Pos.Util (postfixLFields)

type DBFolderStat = (Text, Integer)

-- | Enough context for analysing blocks.
data BlockchainInspectorContext = BlockchainInspectorContext { bicNodeDBs :: DB.NodeDBs }

makeLensesWith postfixLFields ''BlockchainInspectorContext

type BlockchainInspector = ReaderT BlockchainInspectorContext Production

runBlockchainInspector :: BlockchainInspectorContext -> BlockchainInspector a -> Production a
runBlockchainInspector = flip Mtl.runReaderT

initBlockchainAnalyser ::
    HasConfiguration
    => DB.NodeDBs
    -> BlockchainInspector a
    -> Production a
initBlockchainAnalyser nodeDBs action = do
    let bicNodeDBs = nodeDBs
    let inspectorCtx = BlockchainInspectorContext {..}
    runBlockchainInspector inspectorCtx action

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance HasLens DB.NodeDBs BlockchainInspectorContext DB.NodeDBs where
    lensOf = bicNodeDBs_L

instance HasConfiguration => MonadDBRead BlockchainInspector where
    dbGet = DB.dbGetDefault
    dbIterSource = DB.dbIterSourceDefault
    dbGetSerBlock = BDB.dbGetSerBlockRealDefault
    dbGetSerUndo = BDB.dbGetSerUndoRealDefault

prevBlock :: HasConfiguration => Block -> HeaderHash
prevBlock = view prevBlockL
