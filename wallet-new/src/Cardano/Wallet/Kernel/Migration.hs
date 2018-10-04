module Cardano.Wallet.Kernel.Migration (migrateLegacyDataLayer) where

import           Universum

import           Control.Lens.TH
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import           Data.Time (defaultTimeLocale, formatTime, getCurrentTime,
                     iso8601DateFormat)
import           System.Directory (doesDirectoryExist, makeAbsolute, renamePath)

import           Formatting ((%))
import qualified Formatting as F

import qualified Pos.Core as Core
import           Pos.Util.Wlog (Severity (..))
import qualified Pos.Wallet.Web.ClientTypes as WebTypes
import qualified Pos.Wallet.Web.State.Storage as WS

import           Pos.Wallet.Web.State.Acidic (closeState, openState)
import           Pos.Wallet.Web.State.State (getWalletSnapshot)

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (InDb))
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import           Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Restore (restoreWallet)
import           Cardano.Wallet.Kernel.Types

{-------------------------------------------------------------------------------
  Pure helper functions for migration. This include only data that are not
  derivable from the blockchain.
-------------------------------------------------------------------------------}

-- | An heuristic confidence score of whether or not a spending password was
-- set for this wallet.
-- Note how is not possible to reconstruct this information
-- reliably, because the legacy wallet never stored if this wallet was
-- spending-password-protected, it only stored the creation time and the
-- last time the password was changed. However, when creating a wallet, they
-- did set the lastUpdate == dateCreated, so it's not possible to distinguish
-- between the case where the user set the password at creation time and
-- never changed it or the case where the user never set the password at all.
data Confidence =
    HasSpendingPasswordUnknown
  -- ^ This happens because there is no real way to judge if the user set
  -- a spending password or not, especially because if the user @did@ set a
  -- spending password right at the start, there will be no way of reconstructing
  -- this from the legacy storage. Unfortunately, this is the case most users
  -- fall into.
  | ProbablyHasSpendingPassword
  -- ^ Ihis is when the users later changed their password.
  -- In principle they could set it to be empty (so we still cannot be sure),
  -- but this is unlikely (and disallowed by the frontend).

data MigrationMetadata = MigrationMetadata {
    _mmHdRootId            :: HD.HdRootId
  , _mmAssuranceLevel      :: HD.AssuranceLevel
  , _mmWalletName          :: HD.WalletName
  , _mmDefaultAddress      :: Core.Address
  -- ^ A default Address which will be used during restoration. It's a trick
  -- to avoid having the user insert the @spending password@ during migration,
  -- which would complicate things sensibly.
  , _mmHasSpendingPassword :: Bool
  }

makeLenses ''MigrationMetadata

data MetadataUnavailable = NotEnoughData

-- | Obtain any interesting metadata necessary to migrate a wallet from
-- the legacy 'WS.WalletStorage'.
metadataFromWalletStorage :: WS.WalletStorage
                          -> [Either MetadataUnavailable MigrationMetadata]
metadataFromWalletStorage ws =
    let allWalletInfos = HM.toList (WS._wsWalletInfos ws)
    in map extract allWalletInfos
  where
      extract :: (WebTypes.CId (WebTypes.Wal), WS.WalletInfo)
              -> Either MetadataUnavailable MigrationMetadata
      extract (wId, wInfo) =
          MigrationMetadata <$> extractHdRootId wId
                            <*> extractAssuranceLevel wInfo
                            <*> extractWalletName wInfo
                            <*> extractAddress (wId, wInfo)
                            <*> extractHasSpendingPassword wInfo

      extractHdRootId :: WebTypes.CId (WebTypes.Wal)
                      -> Either MetadataUnavailable HD.HdRootId
      extractHdRootId cwalId = do
          let (WebTypes.CId (WebTypes.CHash t)) = cwalId
          rootAddr <- first (const NotEnoughData) (Core.decodeTextAddress t)
          pure . HD.HdRootId . InDb $ rootAddr

      extractAssuranceLevel :: WS.WalletInfo
                            -> Either MetadataUnavailable HD.AssuranceLevel
      extractAssuranceLevel wi = do
          let wMeta = WS._wiMeta wi :: WebTypes.CWalletMeta
          pure $ case WebTypes.cwAssurance wMeta of
                     WebTypes.CWAStrict -> HD.AssuranceLevelStrict
                     WebTypes.CWANormal -> HD.AssuranceLevelNormal

      extractWalletName :: WS.WalletInfo
                        -> Either MetadataUnavailable HD.WalletName
      extractWalletName wi = do
          let wMeta = WS._wiMeta wi :: WebTypes.CWalletMeta
          pure $ HD.WalletName (WebTypes.cwName wMeta)

      extractHasSpendingPassword :: WS.WalletInfo
                                 -> Either MetadataUnavailable Bool
      extractHasSpendingPassword wi = do
          let confidence = case WS._wiPassphraseLU wi == WS._wiCreationTime wi of
                                True  -> HasSpendingPasswordUnknown
                                False -> ProbablyHasSpendingPassword
              -- Our best guess whether or not this wallet has the spending password.
              -- currently we always yield True, but we can tweak these values if
              -- our assumptions reveal not to be correct (or if we can fine tune
              -- our heuristic).
              bestGuess = case confidence of
                               HasSpendingPasswordUnknown  -> True
                               ProbablyHasSpendingPassword -> True
          pure bestGuess

      extractAddress :: (WebTypes.CId (WebTypes.Wal), WS.WalletInfo)
                     -> Either MetadataUnavailable Core.Address
      extractAddress (cwalId, _) =
        findSuitableAddress cwalId (HM.toList (WS._wsAccountInfos ws))

      findSuitableAddress :: WebTypes.CId (WebTypes.Wal)
                          -> [(WebTypes.AccountId, WS.AccountInfo)]
                          -> Either MetadataUnavailable Core.Address
      findSuitableAddress _ [] = Left NotEnoughData
      findSuitableAddress cwalId ((WebTypes.AccountId aiWId _index, acc) : xs)
          | aiWId /= cwalId = findSuitableAddress cwalId xs
          | otherwise =
              -- NOTE: In principle, we might want to pick an unused address,
              -- to make the whole restoration process cleaner. However, not
              -- only would this come with a O(n * log(n)) lookup rather than
              -- just a linear scan, but it wouldn't be that useful: considering
              -- that Daedalus never creates unused addresses automatically, the
              -- actual chances of finding an unused one would be pretty slim.
              case HM.keys . WS._aiAddresses $ acc of
                   []    -> findSuitableAddress cwalId xs
                   (a:_) -> Right a


-- | Tries to check the existence of the DB at 'FilePath'. It first tries to
-- check for the input 'FilePath' as-it-is. If this fails, it tries to check
-- for absolute path.
-- This is sometimes necessary if the DB path is given with paths like
-- '../wallet-db'. Apparently `doesDirectoryExist` for such a relative path
-- was returning 'False', after making the path absolute.
resolveDbPath :: FilePath -> IO (Maybe FilePath)
resolveDbPath fp = do
    exists <- doesDirectoryExist fp
    case exists of
         True  -> return (Just fp)
         False -> do
             absPath  <- makeAbsolute fp
             absExist <- doesDirectoryExist absPath
             return $ if absExist then Just absPath else Nothing

-- | Migrates the wallet database created with the legacy data layer onto the
-- new format. It does that by extract the metadata not deriveable from the
-- blockchain first, and then kicking-off the async restoration process.
--
-- When @forced@ is False we are lenient in logging any error and continuing
-- rather than crashing the node. The rationale is that if
-- we live the node running, we would give the user a chance
-- to submit a bug report from the Daedalus interface.
--
-- However when @forced@ is True the migration is a all-or-nothing.
-- If anything fails (e.g. wallet decoding, resotartion etc) the node crashes.
migrateLegacyDataLayer :: Kernel.PassiveWallet
                       -> FilePath
                       -> Bool
                       -> IO ()
migrateLegacyDataLayer pw unresolvedDbPath forced = do
    let logMsg = pw ^. Kernel.walletLogMessage
    logMsg Info "Starting acid state migration"
    resolved <- resolveDbPath unresolvedDbPath
    case resolved of
       Nothing -> -- We assume no migration is needed and we move along
            case forced of
                True -> do
                    logMsg Error $ "Migration failed! Legacy DB was not found at " <> pack unresolvedDbPath <> ", but migration is forced"
                    exitFailure
                False -> do
                    logMsg Info $ "No legacy DB at " <> pack unresolvedDbPath <> ", migration is not needed."
       Just legacyDbPath -> do
           bracketLegacyDB legacyDbPath $ \st -> do
                let  (unavailable, available) = partitionEithers (metadataFromWalletStorage st)
                     unavailableLen = length unavailable
                     availableLen   = length available

                when (unavailableLen > 0) $ do
                    let msg = show unavailableLen
                              <> " out of "
                              <> show (unavailableLen + availableLen)
                              <> " wallets failed to be converted into the metadata "
                              <> "information needed for migration."
                    case forced of
                        True -> do
                            logMsg Error $ "Migration failed! " <> msg
                            exitFailure
                        False -> do
                            logMsg Error msg

                when (availableLen > 0) $ do
                    logMsg Info $ "Found "
                        <> show availableLen
                        <> " rootAddress(es) to migrate."

                mapM_ (restore pw forced) available

           -- Now that we have closed the DB, we can move the directory
           backupPath <- moveLegacyDB legacyDbPath

           -- asynchronous restoration still runs at this point.
           logMsg Info $ "acid state migration succeeded. Old db backup can be found at " <> pack backupPath


restore :: Kernel.PassiveWallet
        -> Bool
        -> MigrationMetadata
        -> IO ()
restore pw forced metadata = do
    let logMsg = pw ^. Kernel.walletLogMessage
        keystore = pw ^. Kernel.walletKeystore
        wId = WalletIdHdRnd (metadata ^. mmHdRootId)
    mEsk <- Keystore.lookup wId keystore
    case mEsk of
        Just esk -> do
            res <- restoreWallet pw
                                 (metadata ^. mmHasSpendingPassword)
                                 (metadata ^. mmDefaultAddress)
                                 (metadata ^. mmWalletName)
                                 (metadata ^. mmAssuranceLevel)
                                 esk
            case res of
                 Right (restoredRoot, balance) -> do
                     let msg = "Migrating " % F.build
                                % " with balance " % F.build
                     logMsg Info (F.sformat msg restoredRoot balance)
                 Left err -> do
                     let errMsg = "Couldn't migrate " % F.build
                                % " due to : " % F.build % "."
                         msg = F.sformat errMsg wId err
                     case forced of
                        False -> logMsg Error msg
                        True  -> do
                            logMsg Error ("Migration failed! " <> msg <> " You are advised to delete the newly created db and try again.")
                            exitFailure
        Nothing -> do
            let errMsg = "Couldn't migrate " % F.build
                       % " : the key was not found in the keystore."
                msg = F.sformat errMsg wId
            case forced of
                False -> logMsg Error msg
                True  -> do
                    logMsg Error ("Migration failed! " <> msg <> " You are advised to delete the newly created db and try again.")
                    exitFailure

-- PRECONDITION: The 'FilePath' should exist. This is checked at the call site.
bracketLegacyDB :: FilePath -> (WS.WalletStorage -> IO a) -> IO a
bracketLegacyDB legacyDbPath withWalletStorage =
    bracket (openState False legacyDbPath)
            closeState
            -- Apparently hlint thinks this is more readable, heh.
            (getWalletSnapshot >=> withWalletStorage)

-- | Move the legacy database into a backup directory.
moveLegacyDB :: FilePath -> IO FilePath
moveLegacyDB filepath = do
    now <- getCurrentTime
    let backupPath =  filepath
                   <> "-backup-"
                   <> formatTime defaultTimeLocale (iso8601DateFormat (Just "%H_%M_%S")) now
    renamePath filepath backupPath
    return backupPath

