{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Universum

import           Cardano.Crypto.Wallet (unXPrv)
import           Cardano.Wallet.Kernel (DatabaseMode (..), DatabaseOptions (..),
                     bracketPassiveWallet)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdRoot (..), HdRootId (..),
                     HdRootId (..), WalletName (..), eskToHdRootId)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.Keystore (bracketLegacyKeystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (mockNodeStateDef)
import           Data.Aeson (ToJSON (..), (.=))
import           Data.ByteArray.Encoding (Base (..), convertToBase)
import           Data.ByteString (ByteString)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import           Options.Applicative
import           Pos.Core.Common (addrToBase58)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (EncryptedPass (..), EncryptedSecretKey (..))
import           Pos.Crypto.Configuration (ProtocolMagic (..),
                     ProtocolMagicId (..), RequiresNetworkMagic (..))
import           Pos.Infra.InjectFail (mkFInjects)
import           Pos.Util.Log.Severity (Severity (..))
import           Pos.Util.Trace (Trace (..))
import           Pos.Util.UserSecret (readUserSecret)

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.Read as Kernel
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

data Options = Options
    { pm     :: ProtocolMagic
    , dbPath :: FilePath
    , usPath :: FilePath
    }

main :: IO ()
main = do
    let preferences = prefs showHelpOnEmpty
    Options{pm,dbPath,usPath} <- customExecParser preferences parserInfo
    userSecret <- readUserSecret (contramap snd stderrTrace) usPath
    bracketLegacyKeystore userSecret $ \ks -> do
        let dbMode = UseFilePath $ DatabaseOptions
                { Kernel.dbPathAcidState = dbPath <> "-acid"
                , Kernel.dbPathMetadata  = dbPath <> "-sqlite.sqlite3"
                , Kernel.dbRebuild       = False
                }
        fInjects <- mkFInjects Nothing
        bracketPassiveWallet pm dbMode log ks mockNodeStateDef fInjects $ \pw -> do
            wallets <- extractWallet pw
            BL8.putStrLn $ Json.encodePretty (Export <$> wallets)
  where
    log = const (B8.hPutStrLn stderr . T.encodeUtf8)
    stderrTrace = Trace $ Op $ TIO.hPutStrLn stderr

extractWallet
    :: Kernel.PassiveWallet
        -- ^ A passive wallet
    -> IO [(WalletName, EncryptedSecretKey)]
extractWallet pw = do
    wKeys <- Keystore.getKeys (pw ^. Kernel.walletKeystore)
    let nm  = makeNetworkMagic (pw ^.  Kernel.walletProtocolMagic)
    snapshot <- Kernel.getWalletSnapshot pw
    fmap catMaybes $ forM wKeys $ \esk -> do
        let rootId  = eskToHdRootId nm esk
        case Kernel.lookupHdRootId snapshot rootId of
            Left _                    -> do
                let wid = T.decodeUtf8 $ addrToBase58 $ getHdRootId rootId ^. fromDb
                log Error $ "No wallet for id: " <> wid
                pure Nothing

            Right HdRoot{_hdRootName} ->
                pure $ Just (_hdRootName, esk)
  where
    log = pw ^. Kernel.walletLogMessage

newtype Export a = Export a deriving (Show)

instance ToJSON (Export (WalletName, EncryptedSecretKey)) where
    toJSON (Export (name, EncryptedSecretKey{eskPayload, eskHash})) = Json.object
        [ "name" .= getWalletName name
        , "encrypted_root_private_key" .= base16 (unXPrv eskPayload)
        , "passphrase_hash" .= base16 (getEncryptedPass eskHash)
        ]
      where
        base16 = T.decodeUtf8 . convertToBase @ByteString @ByteString Base16

--
-- Command-line
--
--

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parser) $
    progDesc "Export known legacy wallets with their encrypted secret key"
  where
    parser = Options <$> pmOption <*> dbOption <*> usOption

-- --mainnet | --testnet MAGIC
pmOption :: Parser ProtocolMagic
pmOption = mainnetFlag <|> (ProtocolMagic <$> magicOption <*> pure RequiresMagic)
  where
    mainnetFlag = flag'
        (ProtocolMagic (ProtocolMagicId 764824073) RequiresNoMagic)
        (long "mainnet")

    magicOption = fmap ProtocolMagicId $ option auto $ mempty
        <> long "testnet"
        <> metavar "MAGIC"

-- --db-path FILEPATH
dbOption :: Parser FilePath
dbOption = option str $ mempty
    <> long "wallet-db-path"
    <> metavar "FILEPATH"
    <> help "Path to the wallet's database."

-- --keyfile FILEPATH
usOption :: Parser FilePath
usOption = option str $ mempty
    <> long "keyfile"
    <> metavar "FILEPATH"
    <> help "Path to the secret key-store."
