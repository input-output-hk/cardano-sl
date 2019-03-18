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
import           System.IO.Unsafe (unsafePerformIO)

import           Pos.Chain.Block (Block, HeaderHash, prevBlockL)
import           Pos.DB (MonadDBRead (..))
import qualified Pos.DB as DB
import qualified Pos.DB.Block as BDB
import           Pos.Util (postfixLFields)
import           Pos.Util.Util (HasLens (..))
import           Pos.DB.Epoch.Index (IndexCache, mkIndexCache)

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

-- how would i embed this value inside the BlockchainInspector structure? (which i would have to turn into a data record?)
{-# NOINLINE unsafeCache #-}
unsafeCache :: IndexCache
unsafeCache = unsafePerformIO $ mkIndexCache 10

instance DB.MonadDBRead BlockchainInspector where
    dbGet         = DB.dbGetDefault
    dbIterSource  = DB.dbIterSourceDefault
    dbGetSerBlock = BDB.dbGetSerBlockRealDefault unsafeCache
    dbGetSerUndo  = BDB.dbGetSerUndoRealDefault unsafeCache
    dbGetSerBlund = BDB.dbGetSerBlundRealDefault unsafeCache

prevBlock :: Block -> HeaderHash
prevBlock = view prevBlockL
