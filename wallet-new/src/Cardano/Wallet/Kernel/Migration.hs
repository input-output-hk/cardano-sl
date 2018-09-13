module Cardano.Wallet.Kernel.Migration (migrateAcid) where

import           Universum

import           Control.Lens (review)
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import           System.Directory (doesDirectoryExist, renamePath)

import qualified Pos.Core as Core
import           Pos.Util.Wlog (Severity (..))
import qualified Pos.Wallet.Web.ClientTypes as WebTypes
import qualified Pos.Wallet.Web.State.Storage as WS

import           Pos.Wallet.Web.State.Acidic (openState)
import           Pos.Wallet.Web.State.State (getWalletSnapshot)

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (InDb))
import           Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Restore (restoreWallet)
import           Cardano.Wallet.Kernel.Types
import           Cardano.Wallet.WalletLayer.Kernel.Wallets (prefilter)

-- Migration from the old wallet data layer to the new.
-- TODO: CBR-27, CBR-309

migrateAcid :: (Severity -> Text -> IO ()) -> Kernel.PassiveWallet -> FilePath -> Keystore -> IO ()
migrateAcid logMsg pw filepath keystore = do
    logMsg Info "starting acid state migration"
    storageM <- openLegacydb filepath
    case storageM of
        Nothing ->
            logMsg Warning $ "couldn`t find legacy db at " <> pack filepath
        Just st -> do
            let rootsFound = hdRootsFromWalletStorage st
            let roots = catMaybes rootsFound
            let totalN = length rootsFound
            let validN = length roots
            let failedN = totalN - validN
            when (failedN > 0) $ do
                logMsg Error $ show failedN
                    <> " out of "
                    <> show totalN
                    <> " rootAddress(es) failed to decode"

            mapM_ (restore pw keystore) roots

            backupPath <- moveLegacyDB filepath

            -- asynchronous restoration still runs at this point.
            logMsg Info $ "acid state migration succeeded. Old db backup can be found at " <> pack backupPath

restore ::  Kernel.PassiveWallet -> Keystore -> HD.HdRoot ->  IO ()
restore pw keystore HD.HdRoot{..} = do
    let wId = WalletIdHdRnd _hdRootId
    mEsk <- Keystore.lookup wId keystore
    case mEsk of
        Just esk -> do
            _ <- restoreWallet pw False _hdRootName _hdRootAssurance esk (prefilter esk pw wId)
            return ()
        Nothing ->
            return ()

openLegacydb :: FilePath -> IO (Maybe WS.WalletStorage)
openLegacydb filepath = do
    exists <- doesDirectoryExist filepath
    case exists of
        False -> return Nothing
        True  -> do
            -- here we assume that the old db file won`t be deleted in the meantime.
            db <- liftIO $ openState False filepath
            Just <$> liftIO (getWalletSnapshot db)

moveLegacyDB :: FilePath -> IO FilePath
moveLegacyDB filepath = go 0
  where
    go :: Int -> IO FilePath
    go n = do
        let backupPath = filepath <> "-backup-" <> show n
        exists <- doesDirectoryExist backupPath
        case exists of
            True  -> go $ n + 1
            False -> do
                renamePath filepath backupPath
                return backupPath

{-------------------------------------------------------------------------------
  Pure helper functions for migration. This include only data that are not
  derivable from the blockchain.
-------------------------------------------------------------------------------}

-- | Obtain all of the 'HD.HdRoot's in the 'WS.WalletStorage'.
hdRootsFromWalletStorage :: WS.WalletStorage -> [Maybe HD.HdRoot]
hdRootsFromWalletStorage ws =
  flip fmap (HM.toList (WS._wsWalletInfos ws)) $ \(cwalId, wi) -> do
     let wMeta = WS._wiMeta wi :: WebTypes.CWalletMeta
     let (WebTypes.CId (WebTypes.CHash t)) = cwalId
     rootAddr <- rightToMaybe $ Core.decodeTextAddress t
     pure (HD.HdRoot
        { HD._hdRootId = HD.HdRootId . InDb $ rootAddr
        , HD._hdRootName = HD.WalletName (WebTypes.cwName wMeta)
        , HD._hdRootHasPassword = HD.HasSpendingPassword
            (InDb (review Core.timestampSeconds (WS._wiPassphraseLU wi)))
        , HD._hdRootAssurance = case WebTypes.cwAssurance wMeta of
            WebTypes.CWAStrict -> HD.AssuranceLevelStrict
            WebTypes.CWANormal -> HD.AssuranceLevelNormal
        , HD._hdRootCreatedAt =
            InDb (review Core.timestampSeconds (WS._wiCreationTime wi))
        })
