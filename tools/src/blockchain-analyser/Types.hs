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

import           Control.Lens         (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           Ether.Internal       (HasLens (..))
import           Mockable             (Production)

import           Pos.Block.Core       (Block, BlockHeader)
import           Pos.Block.Types      (Undo)
import           Pos.Core             (HasCoreConstants, HeaderHash)
import           Pos.Core.Block       (gbHeader, gbPrevBlock, gbhPrevBlock)
import           Pos.DB               (MonadBlockDBGeneric (..), MonadDBRead (..))
import qualified Pos.DB               as DB
import qualified Pos.DB.Block         as BDB
import           Pos.DB.Sum           (DBSum (..))
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Util.Util        (postfixLFields)

type DBFolderStat = (Text, Integer)

-- | Enough context for analysing blocks.
data BlockchainInspectorContext = BlockchainInspectorContext
    { tbgcDBSum   :: DB.DBSum
    , tbgcNodeDBs :: DB.NodeDBs
    }

makeLensesWith postfixLFields ''BlockchainInspectorContext

type BlockchainInspector = ReaderT BlockchainInspectorContext Production

runBlockchainInspector :: BlockchainInspectorContext -> BlockchainInspector a -> Production a
runBlockchainInspector = flip Mtl.runReaderT

initBlockchainAnalyser ::
    HasCoreConstants
    => DB.NodeDBs
    -> BlockchainInspector a
    -> Production a
initBlockchainAnalyser nodeDBs action = do
    let tbgcDBSum   = RealDB nodeDBs
    let tbgcNodeDBs = nodeDBs
    let inspectorCtx = BlockchainInspectorContext {..}
    runBlockchainInspector inspectorCtx action

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance HasLens DBSum BlockchainInspectorContext DBSum where
    lensOf = tbgcDBSum_L

instance HasLens DB.NodeDBs BlockchainInspectorContext DB.NodeDBs where
    lensOf = tbgcNodeDBs_L

instance MonadDBRead BlockchainInspector where
    dbGet = DB.dbGetSumDefault
    dbIterSource = DB.dbIterSourceSumDefault

instance
    HasCoreConstants =>
    MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo BlockchainInspector
  where
    dbGetBlock = BDB.dbGetBlockSumDefault @SscGodTossing
    dbGetUndo = BDB.dbGetUndoSumDefault @SscGodTossing
    dbGetHeader = BDB.dbGetHeaderSumDefault @SscGodTossing

prevBlock :: HasCoreConstants => Block SscGodTossing -> HeaderHash
prevBlock (Left gB)  = gB ^. gbHeader . gbhPrevBlock
prevBlock (Right mB) = mB ^. gbPrevBlock
