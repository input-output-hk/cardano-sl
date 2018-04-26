module Rendering where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Pos.Wallet.Web.ClientTypes.Types (AccountId (..), CAccountMeta (..), CHash (..),
                                                   CId (..), CWalletMeta (..), Wal)
import           Pos.Wallet.Web.State.Storage (AccountInfo (..), WalletInfo (..),
                                               WalletSyncState (..))
import           System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleIntensity (..),
                                      ConsoleLayer (..), SGR (..), setSGRCode)
import           Text.Printf (printf)

renderWallet :: WalletInfo -> T.Text
renderWallet WalletInfo{..} = toText renderWalletString
  where
    renderWalletString :: String
    renderWalletString = printf "%s, %s, %s, PendingTxs: %s"
                              (bold $ toString $ cwName _wiMeta)
                              (renderReady _wiIsReady)
                              (renderSync _wiSyncState)
                              (renderPendingTxs _wsPendingTxs)

renderAccount :: (AccountId, AccountInfo) -> T.Text
renderAccount (cid, AccountInfo{..}) = toText accountRenderString
  where
    accountRenderString :: String
    accountRenderString = printf "%s, %s, addresses: %s, removed: %s"
                              (bold $ toString $ caName _aiMeta)
                              (renderAccountId cid)
                              (cyan $ show $ HM.size _aiAddresses)
                              (cyan $ show $ HM.size _aiRemovedAddresses)

renderSync :: WalletSyncState -> String
renderSync wt = case wt of
  NotSynced         -> red "NotSynced"
  SyncedWith h      -> green "Synced" <> printf "[%s]" (show h :: String)
  RestoringFrom _ h -> green "Restoring" <> printf "[%s]" (show h :: String)

renderPendingTxs :: HM.HashMap a b -> String
renderPendingTxs m = case HM.size m of
  0 -> green "0"
  x -> yellow (show x)

colored :: Color -> String -> String
colored col txt = setSGRCode [Reset, SetColor Foreground Dull col] <> txt <> setSGRCode [Reset]

red :: String -> String
red = colored Red

bold :: String -> String
bold txt = setSGRCode [Reset, SetConsoleIntensity BoldIntensity] <> txt <> setSGRCode [Reset]

yellow :: String -> String
yellow = colored Red

green :: String -> String
green = colored Green

cyan :: String -> String
cyan = colored Cyan

renderReady :: Bool -> String
renderReady True  = green "ready"
renderReady False = yellow "restoring"

listOf :: [T.Text] -> IO ()
listOf = putStrLn . toString . mappend "\n- " . T.intercalate "\n- "

blankLine :: IO ()
blankLine = say "\n"

-- | Log on stdout.
say :: MonadIO m => String -> m ()
say = liftIO . putStrLn

renderAccountId :: AccountId -> String
renderAccountId AccountId{..} = printf "%s@%s" (renderWId aiWId) (cyan (show aiIndex))

renderWId :: CId Wal -> String
renderWId (CId (CHash hash)) = cyan (toString hash)

