{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Types ( initBlockchainAnalyser
             , DBFolderStat
             , ) where

import           Universum

import           Control.Lens         (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           Ether.Internal       (HasLens (..))
import           Mockable             (Production, currentTime)

import           Pos.Block.Core       (Block, BlockHeader)
import           Pos.Block.Slog       (SlogContext, mkSlogContext)
import           Pos.Block.Types      (Undo)
import           Pos.Context          (GenesisUtxo (..))
import           Pos.Core             (GenesisWStakeholders, HasCoreConstants,
                                       Timestamp (..))
import           Pos.DB               (MonadBlockDBGeneric (..),
                                       MonadBlockDBGenericWrite (..), MonadDB (..),
                                       MonadDBRead (..))
import qualified Pos.DB               as DB
import qualified Pos.DB.Block         as BDB
import           Pos.DB.DB            (initNodeDBs)
import           Pos.DB.Sum           (DBSum (..))
import           Pos.Genesis          (GenesisContext (..), gtcUtxo, gtcWStakeholders)
import           Pos.GState           (GStateContext (..))
import qualified Pos.GState           as GS
-- import           Pos.KnownPeers       (MonadFormatPeers (..))
import           Pos.Launcher         (newInitFuture)
import           Pos.Lrc.Context      (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting         (HasSlottingVar (..))
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Util.Util        (postfixLFields)

type DBFolderStat = (Text, Integer)

-- | Enough context for analysing blocks.
-- "T" means tool
data BlockchainInspectorContext = BlockchainInspectorContext
    { tbgcDBSum         :: DB.DBSum
    -- , tbgcGenesisContext :: GenesisContext
    -- , tbgcSystemStart    :: Timestamp
    }

makeLensesWith postfixLFields ''BlockchainInspectorContext

type BlockchainInspector = ReaderT BlockchainInspectorContext Production

runBlockchainInspector :: BlockchainInspectorContext -> BlockchainInspector a -> Production a
runBlockchainInspector = flip Mtl.runReaderT

initBlockchainAnalyser ::
    DB.NodeDBs
    -> BlockchainInspector a
    -> Production a
initBlockchainAnalyser nodeDBs action = do
    let tbgcDBSum = RealDB nodeDBs

    -- tbgcSystemStart <- Timestamp <$> currentTime
    -- let tbgcGenesisContext = genesisCtx
    let inspectorCtx = BlockchainInspectorContext {..}
    runBlockchainInspector inspectorCtx $ do
        {-
        initNodeDBs @SscGodTossing
        slotVar <- newTVarIO =<< GS.getSlottingData
        putSlottingVar slotVar

        lcLrcSync <- mkLrcSyncData >>= newTVarIO
        putLrcCtx $ LrcContext {..}

        slogContext <- mkSlogContext
        putSlogContext slogContext
        -}
        action

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

{-
instance GS.HasGStateContext BlockchainInspectorContext where
    gStateContext = tbgcDBSum_L
-}

instance HasLens DBSum BlockchainInspectorContext DBSum where
    lensOf = tbgcDBSum_L

{-
instance HasLens SlogContext BlockchainInspectorContext SlogContext where
    lensOf = tbgcGState_L . GS.gscSlogContext

instance HasLens LrcContext BlockchainInspectorContext LrcContext where
    lensOf = tbgcGState_L . GS.gscLrcContext
-}

{-
instance HasLens GenesisUtxo BlockchainInspectorContext GenesisUtxo where
    lensOf = tbgcGenesisContext_L . gtcUtxo

instance HasLens GenesisWStakeholders BlockchainInspectorContext GenesisWStakeholders where
    lensOf = tbgcGenesisContext_L . gtcWStakeholders
-}

{-
instance HasSlottingVar BlockchainInspectorContext where
    slottingTimestamp = tbgcSystemStart_L
    slottingVar = tbgcGState_L . GS.gscSlottingVar
-}

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

{-
-- | In BlockchainInspectorMode we don't have a queue available (we do no comms)
instance MonadFormatPeers BlockchainInspector where
    formatKnownPeers _ = return Nothing
-}
