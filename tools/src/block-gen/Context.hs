{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Initial context satisfing MonadBlockGen.

module Context
       ( bracketTBlockGenMode
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
import qualified Pos.DB.Block         as DB
import           Pos.DB.DB            (initNodeDBs)
import           Pos.DB.Pure          (DBPureVar, newDBPureVar)
import           Pos.GState           (GStateContext (..), GStateContextPure)
import qualified Pos.GState           as GS
import           Pos.Launcher         (newInitFuture)
import           Pos.Lrc.Context      (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting         (HasSlottingVar (..))
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Util.Util        (postfixLFields)

data TBlockGenContext = TBlockGenContext
    { tbgcGState      :: GStateContextPure
    , tbgcGenesisUtxo :: GenesisUtxo
    , tbgcSystemStart :: Timestamp
    }

makeLensesWith postfixLFields ''TBlockGenContext

type TBlockGenMode = ReaderT TBlockGenContext Production

runTBlockGenMode :: TBlockGenContext -> TBlockGenMode a -> Production a
runTBlockGenMode = flip Mtl.runReaderT

bracketTBlockGenMode :: TBlockGenMode a -> Production a
bracketTBlockGenMode action = do
    _gscDB <- newDBPureVar
    (_gscLrcContext, putLrcCtx) <- newInitFuture
    (_gscSlottingVar, putSlottingVar) <- newInitFuture
    (_gscSlogContext, putSlogContext) <- newInitFuture
    let tbgcGState = GStateContext {..}

    tbgcSystemStart <- Timestamp <$> currentTime
    let tbgcGenesisUtxo = undefined
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

instance GS.HasGStateContext TBlockGenContext DBPureVar where
    gStateContext = tbgcGState_L

instance HasLens DBPureVar TBlockGenContext DBPureVar where
    lensOf = tbgcGState_L . GS.gscDB

instance HasLens SlogContext TBlockGenContext SlogContext where
    lensOf = tbgcGState_L . GS.gscSlogContext

instance HasLens GenesisUtxo TBlockGenContext GenesisUtxo where
    lensOf = tbgcGenesisUtxo_L

instance HasLens LrcContext TBlockGenContext LrcContext where
    lensOf = tbgcGState_L . GS.gscLrcContext

instance HasSlottingVar TBlockGenContext where
    slottingTimestamp = tbgcSystemStart_L
    slottingVar = tbgcGState_L . GS.gscSlottingVar

instance MonadDBRead TBlockGenMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance MonadDB TBlockGenMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance
    MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo TBlockGenMode
  where
    dbGetBlock  = DB.dbGetBlockPureDefault @SscGodTossing
    dbGetUndo   = DB.dbGetUndoPureDefault @SscGodTossing
    dbGetHeader = DB.dbGetHeaderPureDefault @SscGodTossing

instance
    MonadBlockDBGenericWrite (BlockHeader SscGodTossing) (Block SscGodTossing) Undo TBlockGenMode
  where
    dbPutBlund = DB.dbPutBlundPureDefault
