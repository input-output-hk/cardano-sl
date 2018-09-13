{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Types ( initBlockchainAnalyser
             , DBFolderStat
             , BlockchainInspector
             , prevBlock
             ) where

import           Universum

import           Control.Lens (makeLensesWith)
import qualified Control.Monad.Reader as Mtl

import           Pos.Chain.Block (Block, HeaderHash, prevBlockL)
import           Pos.DB (MonadDBRead (..))
import qualified Pos.DB as DB
import qualified Pos.DB.Block as BDB
import           Pos.Util (postfixLFields)
import           Pos.Util.Util (HasLens (..))

type DBFolderStat = (Text, Integer)

-- | Enough context for analysing blocks.
data BlockchainInspectorContext = BlockchainInspectorContext { bicNodeDBs :: DB.NodeDBs }

makeLensesWith postfixLFields ''BlockchainInspectorContext

type BlockchainInspector = ReaderT BlockchainInspectorContext IO

runBlockchainInspector :: BlockchainInspectorContext -> BlockchainInspector a -> IO a
runBlockchainInspector = flip Mtl.runReaderT

initBlockchainAnalyser ::
       DB.NodeDBs
    -> BlockchainInspector a
    -> IO a
initBlockchainAnalyser nodeDBs action = do
    let bicNodeDBs = nodeDBs
    let inspectorCtx = BlockchainInspectorContext {..}
    runBlockchainInspector inspectorCtx action

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance HasLens DB.NodeDBs BlockchainInspectorContext DB.NodeDBs where
    lensOf = bicNodeDBs_L

instance MonadDBRead BlockchainInspector where
    dbGet = DB.dbGetDefault
    dbIterSource = DB.dbIterSourceDefault
    dbGetSerBlock = BDB.dbGetSerBlockRealDefault
    dbGetSerUndo = BDB.dbGetSerUndoRealDefault
    dbGetSerBlund = BDB.dbGetSerBlundRealDefault

prevBlock :: Block -> HeaderHash
prevBlock = view prevBlockL
