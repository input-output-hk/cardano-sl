{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Initial context satisfying MonadBlockGen.

module Context
       ( initTBlockGenMode
       ) where

import           Universum

import           Control.Lens               (makeLensesWith)
import qualified Control.Monad.Reader       as Mtl
import           Ether.Internal             (HasLens (..))
import           Mockable                   (Production, currentTime)

import           Pos.Block.Core             (Block, BlockHeader)
import           Pos.Block.Slog             (HasSlogGState (..), mkSlogGState)
import           Pos.Block.Types            (Undo)
import           Pos.Core                   (Timestamp (..))
import           Pos.DB                     (MonadBlockDBGeneric (..),
                                             MonadBlockDBGenericWrite (..), MonadDB (..),
                                             MonadDBRead (..))
import qualified Pos.DB                     as DB
import qualified Pos.DB.Block               as BDB
import           Pos.DB.DB                  (initNodeDBs)
import           Pos.DB.Sum                 (DBSum (..))
import           Pos.GState                 (GStateContext (..))
import qualified Pos.GState                 as GS
import           Pos.KnownPeers             (MonadFormatPeers (..))
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Lrc.Context            (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting               (HasSlottingVar (..))
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Util                   (newInitFuture, postfixLFields)

-- | Enough context for generation of blocks.
-- "T" means tool
data TBlockGenContext = TBlockGenContext
    { tbgcGState      :: GStateContext
    , tbgcSystemStart :: Timestamp
    }

makeLensesWith postfixLFields ''TBlockGenContext

type TBlockGenMode = ReaderT TBlockGenContext Production

runTBlockGenMode :: TBlockGenContext -> TBlockGenMode a -> Production a
runTBlockGenMode = flip Mtl.runReaderT

initTBlockGenMode ::
       HasConfigurations
    => DB.NodeDBs
    -> TBlockGenMode a
    -> Production a
initTBlockGenMode nodeDBs action = do
    let _gscDB = RealDB nodeDBs
    (_gscSlogGState, putSlogGState) <- newInitFuture "slogGState"
    (_gscLrcContext, putLrcCtx) <- newInitFuture "lrcCtx"
    (_gscSlottingVar, putSlottingVar) <- newInitFuture "slottingVar"
    let tbgcGState = GStateContext {..}

    tbgcSystemStart <- Timestamp <$> currentTime
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

instance HasSlottingVar TBlockGenContext where
    slottingTimestamp = tbgcSystemStart_L
    slottingVar = tbgcGState_L . GS.gscSlottingVar

instance HasConfigurations => MonadDBRead TBlockGenMode where
    dbGet = DB.dbGetSumDefault
    dbIterSource = DB.dbIterSourceSumDefault

instance HasConfigurations => MonadDB TBlockGenMode where
    dbPut = DB.dbPutSumDefault
    dbWriteBatch = DB.dbWriteBatchSumDefault
    dbDelete = DB.dbDeleteSumDefault

instance
    HasConfigurations =>
    MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo TBlockGenMode
  where
    dbGetBlock = BDB.dbGetBlockSumDefault @SscGodTossing
    dbGetUndo = BDB.dbGetUndoSumDefault @SscGodTossing
    dbGetHeader = BDB.dbGetHeaderSumDefault @SscGodTossing

instance
    HasConfigurations =>
    MonadBlockDBGenericWrite (BlockHeader SscGodTossing) (Block SscGodTossing) Undo TBlockGenMode
  where
    dbPutBlund = BDB.dbPutBlundSumDefault

-- | In TBlockGenMode we don't have a queue available (we do no comms)
instance MonadFormatPeers TBlockGenMode where
    formatKnownPeers _ = return Nothing
