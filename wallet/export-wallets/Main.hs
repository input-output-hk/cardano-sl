{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Universum

import           Cardano.Crypto.Wallet (toXPub, unXPrv, unXPub)
import           Cardano.Wallet.Kernel (DatabaseMode (..), DatabaseOptions (..),
                     bracketPassiveWallet)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdRoot (..),
                     WalletName (..), eskToHdRootId)
import           Cardano.Wallet.Kernel.Keystore (bracketLegacyKeystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (mockNodeStateDef)
import           Crypto.Hash (Blake2b_160, hash)
import           Data.Aeson (ToJSON (..), (.=))
import           Data.ByteArray.Encoding (Base (..), convertToBase)
import           Data.ByteString (ByteString)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import           Options.Applicative
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (EncryptedPass (..), EncryptedSecretKey (..))
import           Pos.Crypto.Configuration (ProtocolMagic (..),
                     ProtocolMagicId (..), RequiresNetworkMagic (..))
import           Pos.Crypto.Signing (checkPassMatches, emptyPassphrase)
import           Pos.Infra.InjectFail (mkFInjects)
import           Pos.Util.Trace (Trace (..))
import           Pos.Util.UserSecret (readUserSecret)
import           System.Directory (doesDirectoryExist)

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.Read as Kernel
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

data Options = Options
    { pm     :: ProtocolMagic
    , dbPath :: Maybe FilePath
    , usPath :: FilePath
    }

main :: IO ()
main = do
    let preferences = prefs showHelpOnEmpty
    Options{pm,dbPath,usPath} <- customExecParser preferences parserInfo
    dbMode <- getDbMode dbPath
    userSecret <- readUserSecret (contramap snd stderrTrace) usPath
    bracketLegacyKeystore userSecret $ \ks -> do
        fInjects <- mkFInjects Nothing
        bracketPassiveWallet pm dbMode log ks mockNodeStateDef fInjects $ \pw -> do
            wallets <- extractWallet pw
            BL8.putStrLn $ Json.encodePretty (Export <$> wallets)
  where
    log = const (B8.hPutStrLn stderr . T.encodeUtf8)

    stderrTrace = Trace $ Op $ TIO.hPutStrLn stderr

    guardDatabase = doesDirectoryExist . dbPathAcidState  >=> \case
        True  -> pure ()
        False -> fail "There's no acid-state database matching the given path."

    getDbMode = \case
        Nothing ->
            pure UseInMemory
        Just dbPath -> do
            let dbOpts = DatabaseOptions
                    { dbPathAcidState = dbPath <> "-acid"
                    , dbPathMetadata  = dbPath <> "-sqlite.sqlite3"
                    , dbRebuild       = False
                    }
            guardDatabase dbOpts
            pure (UseFilePath dbOpts)

extractWallet
    :: Kernel.PassiveWallet
    -> IO [(Maybe WalletName, EncryptedSecretKey)]
extractWallet pw = do
    wKeys <- Keystore.getKeys (pw ^. Kernel.walletKeystore)
    let nm  = makeNetworkMagic (pw ^.  Kernel.walletProtocolMagic)
    snapshot <- Kernel.getWalletSnapshot pw
    forM wKeys $ \esk -> do
        let rootId  = eskToHdRootId nm esk
        case Kernel.lookupHdRootId snapshot rootId of
            Left _ ->
                pure (Nothing, esk)
            Right HdRoot{_hdRootName} ->
                pure (Just _hdRootName, esk)

newtype Export a = Export a deriving (Show)

instance ToJSON (Export (Maybe WalletName, EncryptedSecretKey)) where
    toJSON (Export (name, esk@EncryptedSecretKey{eskPayload, eskHash})) = Json.object
        [ "id" .= base16 (mkWalletId eskPayload)
        , "name" .= (getWalletName <$> name)
        , "encrypted_root_private_key" .= base16 (unXPrv eskPayload)
        , "passphrase_hash" .= base16 (getEncryptedPass eskHash)
        , "is_passphrase_empty" .= case checkPassMatches emptyPassphrase esk of
            Nothing -> False
            Just{}  -> True
        ]
      where
        base16 = T.decodeUtf8 . convertToBase @ByteString @ByteString Base16
        mkWalletId = BA.convert . hash @_ @Blake2b_160 . unXPub . toXPub

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
dbOption :: Parser (Maybe FilePath)
dbOption = optional $ option str $ mempty
    <> long "wallet-db-path"
    <> metavar "FILEPATH"
    <> help "Path to the wallet's database."

-- --keyfile FILEPATH
usOption :: Parser FilePath
usOption = option str $ mempty
    <> long "keyfile"
    <> metavar "FILEPATH"
    <> help "Path to the secret key-store."
