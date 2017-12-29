module Command.Dump
    ( dump
    ) where

import           Universum

import           Control.Lens (_Wrapped)
import qualified Data.ByteString as BS
import           Formatting (build, sformat, string, (%))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (pathSeparator)
import           System.Wlog (logInfo)

import           Pos.Binary (serialize')
import           Pos.Block.Types (Blund)
import           Pos.Core (BlockHeader, HeaderHash, blockHeaderHash, difficultyL, epochIndexL,
                           genesisHash, getEpochIndex, prevBlockL)
import           Pos.Crypto (shortHashF)
import           Pos.DB.Block (getBlund)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.Error (DBError (..))
import           Pos.StateLock (Priority (..), withStateLock)
import           Pos.Util.Chrono (NewestFirst (..))
import           Pos.Util.Util (maybeThrow)

import           Mode (MonadAuxxMode)

-- | Dump whole blockchain in CBOR format to the specified folder.
-- Each epoch will be at <outFolder>/epoch<epochIndex>.cbor.
dump
    :: (MonadAuxxMode m, MonadIO m)
    => FilePath
    -> m ()
dump outFolder = withStateLock HighPriority "auxx" $ \_ -> do
    printTipDifficulty
    liftIO $ createDirectoryIfMissing True outFolder
    tipHeader <- DB.getTipHeader
    doDump tipHeader
  where
    printTipDifficulty :: MonadAuxxMode m => m ()
    printTipDifficulty = do
        tipDifficulty <- view difficultyL <$> DB.getTipHeader
        logInfo $ sformat ("Our tip's difficulty is "%build) tipDifficulty

    loadEpoch
        :: MonadAuxxMode m
        => HeaderHash
        -> m (Maybe HeaderHash, NewestFirst [] Blund)
    loadEpoch start = do
        -- returns Maybe (tip of previous epoch)
        (maybePrev, blundsMaybeEmpty) <- doIt [] start
        pure (maybePrev, NewestFirst blundsMaybeEmpty)
      where
        doIt :: MonadAuxxMode m => [Blund] -> HeaderHash -> m (Maybe HeaderHash, [Blund])
        doIt !acc h
            | h == genesisHash = pure (Nothing, reverse acc)
            | otherwise = do
                d <- getBlundThrow h
                let prev = d ^. prevBlockL
                    newAcc = d : acc
                case d ^. _1 of
                    Left _  -> pure (Just prev, reverse newAcc)
                    Right _ -> doIt newAcc prev

    doDump
        :: (MonadAuxxMode m, MonadIO m)
        => BlockHeader
        -> m ()
    doDump start = do
        let epochIndex = start ^. epochIndexL
        logInfo $ sformat ("Processing "%build) epochIndex
        (maybePrev, blundsMaybeEmpty) <- loadEpoch $ blockHeaderHash start
        case _Wrapped nonEmpty blundsMaybeEmpty of
            Nothing -> pass
            Just blunds -> do
                let sblunds = serialize' (0 :: Word8, blunds)
                    path = sformat (string%string%"epoch"%build%".cbor")
                            outFolder [pathSeparator] (getEpochIndex epochIndex)
                liftIO $ BS.writeFile (toString path) sblunds
                whenJust maybePrev $ \prev -> do
                    maybeHeader <- DB.getHeader prev
                    case maybeHeader of
                        Just header -> doDump header
                        Nothing     -> error "DB contains block but not its parent"

    getBlundThrow
        :: MonadDBRead m
        => HeaderHash -> m Blund
    getBlundThrow hash =
        maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlund hash
      where
        errFmt = "getBlundThrow: no blund with HeaderHash: "%shortHashF
