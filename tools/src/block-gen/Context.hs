{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Initial context satisfying MonadBlockGen.

module Context
       ( initTBlockGenMode
       ) where

import           Universum

import           Control.Lens         (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           Ether.Internal       (HasLens (..))
import           Mockable             (Production, currentTime)

import           Pos.Block.Core       (Block, BlockHeader)
import           Pos.Block.Slog       (SlogContext, mkSlogContext)
import           Pos.Block.Types      (Undo)
import           Pos.Context          (GenesisUtxo (..))
import           Pos.Core             (Timestamp (..))
import           Pos.DB               (MonadBlockDBGeneric (..),
                                       MonadBlockDBGenericWrite (..), MonadDB (..),
                                       MonadDBRead (..))
import qualified Pos.DB               as DB
import qualified Pos.DB.Block         as BDB
import           Pos.DB.DB            (initNodeDBs)
import           Pos.DB.Sum           (DBSum (..))
import           Pos.GState           (GStateContext (..))
import qualified Pos.GState           as GS
import           Pos.Launcher         (newInitFuture)
import           Pos.Lrc.Context      (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting         (HasSlottingVar (..))
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Util.Util        (postfixLFields)

-- | Enough context for generation of blocks.
-- "T" means tool
data TBlockGenContext = TBlockGenContext
    { tbgcGState      :: GStateContext
    , tbgcGenesisUtxo :: GenesisUtxo
    , tbgcSystemStart :: Timestamp
    }

makeLensesWith postfixLFields ''TBlockGenContext
type TBlockGenMode = ReaderT TBlockGenContext Production

runTBlockGenMode :: TBlockGenContext -> TBlockGenMode a -> Production a
runTBlockGenMode = flip Mtl.runReaderT

initTBlockGenMode :: DB.NodeDBs -> GenesisUtxo -> TBlockGenMode a -> Production a
initTBlockGenMode nodeDBs genUtxo action = do
    let _gscDB = RealDB nodeDBs
    (_gscSlogContext, putSlogContext) <- newInitFuture
    (_gscLrcContext, putLrcCtx) <- newInitFuture
    (_gscSlottingVar, putSlottingVar) <- newInitFuture
    let tbgcGState = GStateContext {..}

    tbgcSystemStart <- Timestamp <$> currentTime
    let tbgcGenesisUtxo = genUtxo
    let tblockCtx = TBlockGenContext {..}
    runTBlockGenMode tblockCtx $ do
        initNodeDBs @SscGodTossing
        slotVar <- newTVarIO =<< GS.getSlottingData
        putSlottingVar slotVar

        lcLrcSync <- mkLrcSyncData >>= newTVarIO
        putLrcCtx $ LrcContext {..}

        slogContext <- mkSlogContext
        putSlogContext slogContext
        action

----------------------------------------------------------------------------
-- Boilerplate TBlockGenMode instances
----------------------------------------------------------------------------

-- Sno^W The God of Contexts requires more bOILeRpLaTE.

instance GS.HasGStateContext TBlockGenContext where
    gStateContext = tbgcGState_L

instance HasLens DBSum TBlockGenContext DBSum where
    lensOf = tbgcGState_L . GS.gscDB

instance HasLens SlogContext TBlockGenContext SlogContext where
    lensOf = tbgcGState_L . GS.gscSlogContext

instance HasLens LrcContext TBlockGenContext LrcContext where
    lensOf = tbgcGState_L . GS.gscLrcContext

instance HasLens GenesisUtxo TBlockGenContext GenesisUtxo where
    lensOf = tbgcGenesisUtxo_L

instance HasSlottingVar TBlockGenContext where
    slottingTimestamp = tbgcSystemStart_L
    slottingVar = tbgcGState_L . GS.gscSlottingVar


instance MonadDBRead TBlockGenMode where
    dbGet = DB.dbGetSumDefault
    dbIterSource = DB.dbIterSourceSumDefault

instance MonadDB TBlockGenMode where
    dbPut = DB.dbPutSumDefault
    dbWriteBatch = DB.dbWriteBatchSumDefault
    dbDelete = DB.dbDeleteSumDefault

instance
    MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo TBlockGenMode
  where
    dbGetBlock = BDB.dbGetBlockSumDefault @SscGodTossing
    dbGetUndo = BDB.dbGetUndoSumDefault @SscGodTossing
    dbGetHeader = BDB.dbGetHeaderSumDefault @SscGodTossing

instance
    MonadBlockDBGenericWrite (BlockHeader SscGodTossing) (Block SscGodTossing) Undo TBlockGenMode
  where
    dbPutBlund = BDB.dbPutBlundSumDefault

