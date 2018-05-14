module Main
       ( main
       ) where

import           Universum

import           Crypto.Random (MonadRandom)
import           Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58)
import qualified Data.List as L
import qualified Data.Text as T
import           Formatting (build, sformat, stext, string, (%))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)
import           System.Wlog (WithLogger, debugPlus, logInfo, productionB, setupLogging,
                              termSeveritiesOutB, usingLoggerName)
import qualified Text.JSON.Canonical as CanonicalJSON

import           Pos.Binary (asBinary, serialize')
import qualified Pos.Client.CLI as CLI
import           Pos.Core (CoreConfiguration (..), GenesisConfiguration (..), RichSecrets (..),
                           addressHash, ccGenesis, coreConfiguration, generateFakeAvvm,
                           generateRichSecrets, mkVssCertificate, vcSigningKey, vssMaxTTL,
                           protocolMagic)
import           Pos.Crypto (EncryptedSecretKey (..), SecretKey (..), VssKeyPair, fullPublicKeyF,
                             hashHexF, noPassEncrypt, redeemPkB64F, toPublic, toVssPublicKey)
import           Pos.Launcher (HasConfigurations, withConfigurations)
import           Pos.Util.UserSecret (readUserSecret, takeUserSecret, usKeys, usPrimKey, usVss,
                                      usWallet, writeUserSecretRelease, wusRootKey)

import           Dump (dumpFakeAvvmSeed, dumpGeneratedGenesisData, dumpRichSecrets)
import           KeygenOptions (DumpAvvmSeedsOptions (..), GenKeysOptions (..), KeygenCommand (..),
                                KeygenOptions (..), getKeygenOptions)

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
    rs <- liftIO generateRichSecrets
    dumpRichSecrets path rs
    let pk = toPublic (rsPrimaryKey rs)
    logInfo $
        sformat
            ("Successfully generated primary key and dumped to "%string%
             ", stakeholder id: "%hashHexF%
             ", PK (base64): "%fullPublicKeyF)
            path
            (addressHash pk)
            pk

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
        GCSpec {} -> do
            dumpGeneratedGenesisData (gkoOutDir, gkoKeyPattern)
            logInfo (toText gkoOutDir <> " generated successfully")

genVssCert
    :: (HasConfigurations, WithLogger m, MonadIO m)
    => FilePath -> m ()
genVssCert path = do
    us <- readUserSecret path
    let primKey = fromMaybe (error "No primary key") (us ^. usPrimKey)
        vssKey  = fromMaybe (error "No VSS key") (us ^. usVss)
    let cert = mkVssCertificate
                 protocolMagic
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
    setupLogging Nothing $ productionB <> termSeveritiesOutB debugPlus
    usingLoggerName "keygen" $ withConfigurations koConfigurationOptions $ \_ -> do
        logInfo "Processing command"
        case koCommand of
            RearrangeMask msk       -> rearrange msk
            GenerateKey path        -> genPrimaryKey path
            GenerateVss path        -> genVssCert path
            ReadKey path            -> readKey path
            DumpAvvmSeeds opts      -> dumpAvvmSeeds opts
            GenerateKeysBySpec gkbg -> generateKeysByGenesis gkbg
            DumpGenesisData dgdPath dgdCanonical
                                    -> CLI.dumpGenesisData dgdCanonical dgdPath