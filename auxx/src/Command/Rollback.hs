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

import           Pos.Chain.Block (Blund, mainBlockTxPayload)
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Txp (TxAux, flattenTxPayload)
import           Pos.Core (difficultyL, epochIndexL)
import           Pos.Core.Chrono (NewestFirst, _NewestFirst)
import           Pos.DB.Block (BypassSecurityCheck (..),
                     ShouldCallBListener (..), rollbackBlocksUnsafe)
import qualified Pos.DB.Block as DB
import qualified Pos.DB.BlockIndex as DB
import           Pos.Infra.StateLock (Priority (..), withStateLock)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason (..))
import           Pos.Util.Wlog (logInfo)

import           Mode (MonadAuxxMode)

-- | Rollback given number of blocks from the DB and dump transactions
-- from it to the given file.
rollbackAndDump
    :: MonadAuxxMode m
    => Genesis.Config
    -> Word
    -> FilePath
    -> m ()
rollbackAndDump genesisConfig numToRollback outFile = withStateLock HighPriority ApplyBlockWithRollback $ \_ -> do
    printTipDifficulty
    blundsMaybeEmpty <- modifyBlunds <$> DB.loadBlundsFromTipByDepth
        (configGenesisHash genesisConfig)
        (fromIntegral numToRollback)
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
            rollbackBlocksUnsafe genesisConfig (BypassSecurityCheck True) (ShouldCallBListener True) blunds
            logInfo $ sformat ("Rolled back "%int%" blocks") (length blunds)
            printTipDifficulty
  where
    -- It's illegal to rollback 0-th genesis block.  We also may load
    -- more blunds than necessary, because genesis blocks don't
    -- contribute to depth counter.
    modifyBlunds :: NewestFirst [] Blund -> NewestFirst [] Blund
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
