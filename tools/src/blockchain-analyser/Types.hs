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

import           Control.Lens           (makeLensesWith)
import qualified Control.Monad.Reader   as Mtl
import           Ether.Internal         (HasLens (..))
import           Mockable               (Production)

import           Pos.Block.Core         (Block, BlockHeader)
import           Pos.Block.Types        (Undo)
import           Pos.Core               (HeaderHash, prevBlockL)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB                 (MonadBlockDBGeneric (..), MonadDBRead (..))
import qualified Pos.DB                 as DB
import qualified Pos.DB.Block           as BDB
import           Pos.Ssc.GodTossing     (SscGodTossing)
import           Pos.Util.Util          (postfixLFields)

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

instance
    HasConfiguration =>
    MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo BlockchainInspector
  where
    dbGetBlock = BDB.dbGetBlockDefault @SscGodTossing
    dbGetUndo = BDB.dbGetUndoDefault @SscGodTossing
    dbGetHeader = BDB.dbGetHeaderDefault @SscGodTossing

prevBlock :: HasConfiguration => Block SscGodTossing -> HeaderHash
prevBlock = view prevBlockL
