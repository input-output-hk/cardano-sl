module Main
       ( main
       ) where

import           Universum

import           Control.Lens           ((?~))
import           Crypto.Random          (MonadRandom)
import           Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58)
import qualified Data.List              as L
import qualified Data.Text              as T
import           Formatting             (build, sformat, stext, (%))
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        ((</>))
import           System.FilePath.Glob   (glob)
import           System.Wlog            (Severity (Debug), WithLogger, consoleOutB,
                                         lcTermSeverity, logInfo, setupLogging,
                                         usingLoggerName)
import qualified Text.JSON.Canonical    as CanonicalJSON

import           Pos.Binary             (asBinary, serialize')
import           Pos.Core               (CoreConfiguration (..),
                                         GenesisConfiguration (..),
                                         GenesisInitializer (..), addressHash, ccGenesis,
                                         coreConfiguration, generateFakeAvvm,
                                         generateSecrets, generatedSecrets, gsInitializer,
                                         mkVssCertificate, vcSigningKey, vssMaxTTL)
import           Pos.Crypto             (EncryptedSecretKey (..), SecretKey (..),
                                         VssKeyPair, hashHexF, noPassEncrypt,
                                         redeemPkB64F, toPublic, toVssPublicKey)
import           Pos.Launcher           (HasConfigurations, withConfigurations)
import           Pos.Util.UserSecret    (readUserSecret, takeUserSecret, usKeys,
                                         usPrimKey, usVss, usWallet,
                                         writeUserSecretRelease)
import           Pos.Wallet.Web.Secret  (wusRootKey)

import           Dump                   (dumpFakeAvvmSeed, dumpGeneratedGenesisData,
                                         dumpKeyfile)
import           KeygenOptions          (DumpAvvmSeedsOptions (..), GenKeysOptions (..),
                                         KeygenCommand (..), KeygenOptions (..),
                                         getKeygenOptions)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

rearrangeKeyfile :: (MonadIO m, MonadThrow m, WithLogger m) => FilePath -> m ()
rearrangeKeyfile fp = do
    us <- takeUserSecret fp
    let sk = maybeToList $ us ^. usPrimKey
    writeUserSecretRelease $
        us & usKeys %~ (++ map noPassEncrypt sk)

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

rearrange :: (MonadIO m, MonadThrow m, WithLogger m) => FilePath -> m ()
rearrange msk = mapM_ rearrangeKeyfile =<< liftIO (glob msk)

genPrimaryKey :: (HasConfigurations, MonadIO m, MonadThrow m, WithLogger m, MonadRandom m) => FilePath -> m ()
genPrimaryKey path = do
    sk <- liftIO $ generateSecrets Nothing
    void $ dumpKeyfile True path sk
    logInfo $ "Successfully generated primary key " <> (toText path)

readKey :: (MonadIO m, MonadThrow m, WithLogger m) => FilePath -> m ()
readKey path = do
    us <- readUserSecret path
    logInfo $ maybe "No Primary key"
                    (("Primary: " <>) . showKeyWithAddressHash) $
                    view usPrimKey us
    logInfo $ maybe "No wallet set"
                    (("Wallet set: " <>) . showKeyWithAddressHash . decryptESK . view wusRootKey) $
                    view usWallet us
    logInfo $ "Keys: " <> (T.concat $ L.intersperse "\n" $
                           map (showKeyWithAddressHash . decryptESK) $
                           view usKeys us)
    logInfo $ maybe "No vss"
                    (("Vss PK: " <>) . showPvssKey) $
                    view usVss us

showKeyWithAddressHash :: SecretKey -> Text
showKeyWithAddressHash sk =
    sformat (stext%"; address hash: "%hashHexF) (toBase58Text pk) ah
  where
    pk = toPublic sk
    toBase58Text = decodeUtf8 . encodeBase58 bitcoinAlphabet . serialize'
    ah = addressHash pk

showPvssKey :: VssKeyPair -> Text
showPvssKey = sformat build . asBinary . toVssPublicKey

decryptESK :: EncryptedSecretKey -> SecretKey
decryptESK (EncryptedSecretKey sk _) = SecretKey sk

dumpAvvmSeeds
    :: (MonadIO m, WithLogger m)
    => DumpAvvmSeedsOptions -> m ()
dumpAvvmSeeds DumpAvvmSeedsOptions{..} = do
    logInfo $ "Generating fake avvm data into " <> fromString dasPath
    liftIO $ createDirectoryIfMissing True dasPath

    when (dasNumber <= 0) $ error $
        "number of seeds should be positive, but it's " <> show dasNumber

    (fakeAvvmPubkeys, seeds) <-
        liftIO $ unzip <$> replicateM (fromIntegral dasNumber) generateFakeAvvm

    forM_ (zip seeds [1 .. dasNumber]) $ \(seed, i) ->
        dumpFakeAvvmSeed (dasPath </> ("key"<>show i<>".seed")) seed
    forM_ (fakeAvvmPubkeys `zip` [1..dasNumber]) $
        \(rPk,i) -> writeFile (dasPath </> "key"<>show i<>".pk")
                              (sformat redeemPkB64F rPk)

    logInfo $ "Seeds were generated"

generateKeysByGenesis
    :: (HasConfigurations, MonadIO m, WithLogger m, MonadThrow m, MonadRandom m)
    => GenKeysOptions -> m ()
generateKeysByGenesis GenKeysOptions{..} = do
    case ccGenesis coreConfiguration of
        GCSrc {} ->
            error $ "Launched source file conf"
        GCSpec spec -> case gsInitializer spec of
            MainnetInitializer{}   -> error "Can't generate keys for MainnetInitializer"
            TestnetInitializer{..} -> do
                dumpGeneratedGenesisData (gkoOutDir, gkoKeyPattern)
                                         tiTestBalance
                                         (fromMaybe (error "No secrets for genesis") generatedSecrets)
                logInfo (toText gkoOutDir <> " generated successfully")

genVssCert
    :: (HasConfigurations, WithLogger m, MonadIO m)
    => FilePath -> m ()
genVssCert path = do
    us <- readUserSecret path
    let primKey = fromMaybe (error "No primary key") (us ^. usPrimKey)
        vssKey  = fromMaybe (error "No VSS key") (us ^. usVss)
    let cert = mkVssCertificate
                 primKey
                 (asBinary (toVssPublicKey vssKey))
                 (vssMaxTTL - 1)
    putText $ sformat ("JSON: key "%hashHexF%", value "%stext)
              (addressHash $ vcSigningKey cert)
              (decodeUtf8 $
                    CanonicalJSON.renderCanonicalJSON $
                    runIdentity $
                    CanonicalJSON.toJSON cert)

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

main :: IO ()
main = do
    KeygenOptions{..} <- getKeygenOptions
    setupLogging $ consoleOutB & lcTermSeverity ?~ Debug
    usingLoggerName "keygen" $ withConfigurations koConfigurationOptions $ do
        logInfo "Processing command"
        case koCommand of
            RearrangeMask msk       -> rearrange msk
            GenerateKey path        -> genPrimaryKey path
            GenerateVss path        -> genVssCert path
            ReadKey path            -> readKey path
            DumpAvvmSeeds opts      -> dumpAvvmSeeds opts
            GenerateKeysBySpec gkbg -> generateKeysByGenesis gkbg
