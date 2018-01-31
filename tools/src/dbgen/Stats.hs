{-# LANGUAGE TypeFamilies #-}

module Stats where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Wallet.Web.State.Acidic (GetWalletStorage (..), closeState, openState, query)
import           Pos.Wallet.Web.State.Storage (WalletStorage (..))
import           Serokell.AcidState.ExtendedState (ExtendedState)
import           Text.Printf (printf)

import           Rendering (blankLine, bold, listOf, red, renderAccount, renderWallet, say)

{- For reference:

data WalletStorage = WalletStorage
    { _wsWalletInfos     :: !(HashMap (CId Wal) WalletInfo)
    , _wsAccountInfos    :: !(HashMap AccountId AccountInfo)
    , _wsProfile         :: !CProfile
    , _wsReadyUpdates    :: [CUpdateInfo]
    , _wsTxHistory       :: !(HashMap (CId Wal) (HashMap CTxId CTxMeta))
    , _wsHistoryCache    :: !(HashMap (CId Wal) (Map TxId TxHistoryEntry))
    , _wsUtxo            :: !Utxo
    -- @_wsBalances@ depends on @_wsUtxo@,
    -- it's forbidden to update @_wsBalances@ without @_wsUtxo@
    , _wsBalances        :: !WalletBalances
    , _wsUsedAddresses   :: !CustomAddresses
    , _wsChangeAddresses :: !CustomAddresses
    }
-}

-- type Utxo = Map TxIn TxOutAux


showStats :: HasConfigurations => WalletStorage -> IO ()
showStats WalletStorage{..} = do
    let wallets  = HM.elems _wsWalletInfos
    let accounts = HM.toList _wsAccountInfos
    -- let txsIn    = elems _wsUtxo
    say $ bold "Wallets:" <> printf " %d"  (length wallets)
    listOf (map renderWallet wallets)
    blankLine
    say $ bold "Accounts:" <> printf " %d" (length accounts)
    listOf (map renderAccount accounts)
    blankLine
    say $ printf "Number of used addresses: %d" (length _wsUsedAddresses)
    say $ printf "Number of change addresses: %d" (length _wsChangeAddresses)

showStatsAndExit :: HasConfigurations => FilePath -> IO ()
showStatsAndExit walletPath = do
    bracket (openState False walletPath) (\x -> closeState x >> exitSuccess) $ \db -> do
        walletStorage <- getStorage db
        showStats walletStorage

showStatsNoExit :: HasConfigurations => FilePath -> IO ()
showStatsNoExit walletPath = do
  bracket (openState False walletPath) (\x -> closeState x) $ \db -> do
        walletStorage <- getStorage db
        showStats walletStorage

showStatsData :: HasConfigurations => String -> FilePath -> IO ()
showStatsData mark walletPath = do
    blankLine
    say $ red $ "The stats " <> mark <> " modification:"
    showStatsNoExit walletPath
    blankLine

getStorage :: ExtendedState WalletStorage -> IO WalletStorage
getStorage db = liftIO (query db GetWalletStorage)

