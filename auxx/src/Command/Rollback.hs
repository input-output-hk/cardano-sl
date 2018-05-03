-- | Rollback functionality in Auxx.

module Command.Rollback
       ( rollbackAndDump
       ) where

import           Universum

import           Control.Lens (_Wrapped)
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import           Data.List (genericTake)
import           Formatting (build, int, sformat, string, (%))
import           System.Wlog (logInfo)

import           Pos.Block.Logic (BypassSecurityCheck (..), rollbackBlocksUnsafe)
import           Pos.Block.Slog (ShouldCallBListener (..))
import           Pos.Block.Types (Blund)
import           Pos.Core (difficultyL, epochIndexL)
import           Pos.Core.Block (mainBlockTxPayload)
import           Pos.Core.Txp (TxAux)
import qualified Pos.DB.Block.Load as DB
import qualified Pos.DB.BlockIndex as DB
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.StateLock (Priority (..), withStateLock)
import           Pos.Txp (flattenTxPayload)
import           Pos.Util.Chrono (NewestFirst, _NewestFirst)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.JsonLog.Events (MemPoolModifyReason (..))

import           Mode (MonadAuxxMode)

-- | Rollback given number of blocks from the DB and dump transactions
-- from it to the given file.
rollbackAndDump
    :: ( MonadAuxxMode m
       , HasCompileInfo
       )
    => Word
    -> FilePath
    -> m ()
rollbackAndDump numToRollback outFile = withStateLock HighPriority ApplyBlockWithRollback $ \_ -> do
    printTipDifficulty
    blundsMaybeEmpty <- modifyBlunds <$>
        DB.loadBlundsFromTipByDepth (fromIntegral numToRollback)
    logInfo $ sformat ("Loaded "%int%" blunds") (length blundsMaybeEmpty)
    case _Wrapped nonEmpty blundsMaybeEmpty of
        Nothing -> pass
        Just blunds -> do
            let extractTxs :: Blund -> [TxAux]
                extractTxs (Left _, _) = []
                extractTxs (Right mainBlock, _) =
                    flattenTxPayload $ mainBlock ^. mainBlockTxPayload
            let allTxs :: [TxAux]
                allTxs = concatMap extractTxs blunds
            liftIO $ BSL.writeFile outFile (encode allTxs)
            logInfo $ sformat ("Dumped "%int%" transactions to "%string)
                      (length allTxs) (outFile)
            rollbackBlocksUnsafe (BypassSecurityCheck True) (ShouldCallBListener True) blunds
            logInfo $ sformat ("Rolled back "%int%" blocks") (length blunds)
            printTipDifficulty
  where
    -- It's illegal to rollback 0-th genesis block.  We also may load
    -- more blunds than necessary, because genesis blocks don't
    -- contribute to depth counter.
    modifyBlunds :: HasSscConfiguration => NewestFirst [] Blund -> NewestFirst [] Blund
    modifyBlunds =
        over _NewestFirst (genericTake numToRollback . skip0thGenesis)
    skip0thGenesis = filter (not . is0thGenesis)
    is0thGenesis :: Blund -> Bool
    is0thGenesis (Left genBlock, _)
        | genBlock ^. epochIndexL == 0 = True
    is0thGenesis _ = False
    printTipDifficulty = do
        tipDifficulty <- view difficultyL <$> DB.getTipHeader
        logInfo $ sformat ("Our tip's difficulty is "%build) tipDifficulty
