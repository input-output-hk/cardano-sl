-- | Rollback functionality in Auxx.

module Command.Rollback
       ( rollbackAndDump
       ) where

import           Universum

import           Control.Lens         (_Wrapped)
import qualified Data.ByteString.Lazy as BSL
import           Data.List            (genericTake)
import           Formatting           (build, int, sformat, string, (%))
import           System.Wlog          (logInfo)

import           Pos.Binary           (serialize)
import           Pos.Block.Core       (mainBlockTxPayload)
import           Pos.Block.Logic      (BypassSecurityCheck (..), rollbackBlocksUnsafe)
import           Pos.Block.Types      (Blund)
import           Pos.Core             (HasCoreConstants, difficultyL, epochIndexL)
import           Pos.DB.DB            (getTipHeader, loadBlundsFromTipByDepth)
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Txp              (TxAux, flattenTxPayload)
import           Pos.Util.Chrono      (NewestFirst, _NewestFirst)

import           Mode                 (AuxxMode, AuxxSscType)

-- | Rollback given number of blocks from the DB and dump transactions
-- from it to the given file.
rollbackAndDump :: HasCoreConstants => Word -> FilePath -> AuxxMode ()
rollbackAndDump numToRollback outFile = do
    printTipDifficulty
    blundsMaybeEmpty <- modifyBlunds <$>
        loadBlundsFromTipByDepth @SscGodTossing (fromIntegral numToRollback)
    logInfo $ sformat ("Loaded "%int%" blunds") (length blundsMaybeEmpty)
    case _Wrapped nonEmpty blundsMaybeEmpty of
        Nothing -> pass
        Just blunds -> do
            let extractTxs :: Blund SscGodTossing -> [TxAux]
                extractTxs (Left _, _) = []
                extractTxs (Right mainBlock, _) =
                    flattenTxPayload $ mainBlock ^. mainBlockTxPayload
            let allTxs :: [TxAux]
                allTxs = concatMap extractTxs blunds
            liftIO $ BSL.writeFile outFile (serialize allTxs)
            logInfo $ sformat ("Dumped "%int%" transactions to "%string)
                      (length allTxs) (outFile)
            rollbackBlocksUnsafe (BypassSecurityCheck True) blunds
            logInfo $ sformat ("Rolled back "%int%" blocks") (length blunds)
            printTipDifficulty
  where
    -- It's illegal to rollback 0-th genesis block.  We also may load
    -- more blunds than necessary, because genesis blocks don't
    -- contribute to depth counter.
    modifyBlunds :: NewestFirst [] (Blund ssc) -> NewestFirst [] (Blund ssc)
    modifyBlunds =
        over _NewestFirst (genericTake numToRollback . skip0thGenesis)
    skip0thGenesis = filter (not . is0thGenesis)
    is0thGenesis :: Blund ssc -> Bool
    is0thGenesis (Left genBlock, _)
        | genBlock ^. epochIndexL == 0 = True
    is0thGenesis _ = False
    printTipDifficulty = do
        tipDifficulty <- view difficultyL <$> getTipHeader @AuxxSscType
        logInfo $ sformat ("Our tip's difficulty is "%build) tipDifficulty
