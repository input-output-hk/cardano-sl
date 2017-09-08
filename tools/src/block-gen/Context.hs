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
import           Pos.Block.Slog       (HasSlogGState (..), mkSlogGState)
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
import           Pos.KnownPeers       (MonadFormatPeers (..))
import           Pos.Lrc.Context      (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting         (HasSlottingVar (..))
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Util             (newInitFuture, postfixLFields)

-- | Enough context for generation of blocks.
-- "T" means tool
data TBlockGenContext = TBlockGenContext
    { tbgcGState         :: GStateContext
    , tbgcGenesisContext :: GenesisContext
    , tbgcSystemStart    :: Timestamp
    }

makeLensesWith postfixLFields ''TBlockGenContext

type TBlockGenMode = ReaderT TBlockGenContext Production

runTBlockGenMode :: TBlockGenContext -> TBlockGenMode a -> Production a
runTBlockGenMode = flip Mtl.runReaderT

initTBlockGenMode ::
       HasCoreConstants
    => DB.NodeDBs
    -> GenesisContext
    -> TBlockGenMode a
    -> Production a
initTBlockGenMode nodeDBs genesisCtx action = do
    let _gscDB = RealDB nodeDBs
    (_gscSlogGState, putSlogGState) <- newInitFuture "slogGState"
    (_gscLrcContext, putLrcCtx) <- newInitFuture "lrcCtx"
    (_gscSlottingVar, putSlottingVar) <- newInitFuture "slottingVar"
    let tbgcGState = GStateContext {..}

    tbgcSystemStart <- Timestamp <$> currentTime
    let tbgcGenesisContext = genesisCtx
    let tblockCtx = TBlockGenContext {..}
    runTBlockGenMode tblockCtx $ do
        initNodeDBs @SscGodTossing
        slotVar <- newTVarIO =<< GS.getSlottingData
        putSlottingVar slotVar

        lcLrcSync <- mkLrcSyncData >>= newTVarIO
        putLrcCtx $ LrcContext {..}

        slogGS <- mkSlogGState
        putSlogGState slogGS
        action

----------------------------------------------------------------------------
-- Boilerplate TBlockGenMode instances
----------------------------------------------------------------------------

-- Sno^W The God of Contexts requires more bOILeRpLaTE.

instance GS.HasGStateContext TBlockGenContext where
    gStateContext = tbgcGState_L

instance HasLens DBSum TBlockGenContext DBSum where
    lensOf = tbgcGState_L . GS.gscDB

instance HasSlogGState TBlockGenContext where
    slogGState = tbgcGState_L . slogGState

instance HasLens LrcContext TBlockGenContext LrcContext where
    lensOf = tbgcGState_L . GS.gscLrcContext

instance HasLens GenesisUtxo TBlockGenContext GenesisUtxo where
    lensOf = tbgcGenesisContext_L . gtcUtxo

instance HasLens GenesisWStakeholders TBlockGenContext GenesisWStakeholders where
    lensOf = tbgcGenesisContext_L . gtcWStakeholders

instance HasLens GenesisContext TBlockGenContext GenesisContext where
    lensOf = tbgcGenesisContext_L

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
    HasCoreConstants =>
    MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo TBlockGenMode
  where
    dbGetBlock = BDB.dbGetBlockSumDefault @SscGodTossing
    dbGetUndo = BDB.dbGetUndoSumDefault @SscGodTossing
    dbGetHeader = BDB.dbGetHeaderSumDefault @SscGodTossing

instance
    HasCoreConstants =>
    MonadBlockDBGenericWrite (BlockHeader SscGodTossing) (Block SscGodTossing) Undo TBlockGenMode
  where
    dbPutBlund = BDB.dbPutBlundSumDefault

-- | In TBlockGenMode we don't have a queue available (we do no comms)
instance MonadFormatPeers TBlockGenMode where
    formatKnownPeers _ = return Nothing
