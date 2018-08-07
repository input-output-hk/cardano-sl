module Main
       ( main
       ) where

import           Universum

import           Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58)
import qualified Data.List as L
import qualified Data.Text as T
import           Formatting (build, sformat, stext, string, (%))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)
import qualified Text.JSON.Canonical as CanonicalJSON

import           Pos.Binary (asBinary, serialize')
import qualified Pos.Client.CLI as CLI
import           Pos.Core (CoreConfiguration (..), GenesisConfiguration (..),
                     ProtocolMagic, addressHash, ccGenesis, coreConfiguration,
                     vssMaxTTL)
import           Pos.Core.Genesis (RichSecrets (..), generateFakeAvvm,
                     generateRichSecrets)
import           Pos.Core.Ssc (mkVssCertificate, vcSigningKey)
import           Pos.Crypto (EncryptedSecretKey (..), SecretKey (..),
                     VssKeyPair, fullPublicKeyF, hashHexF, noPassEncrypt,
                     redeemPkB64F, toPublic, toVssPublicKey)
import           Pos.Launcher (HasConfigurations, withConfigurations)
import qualified Pos.Util.Log as Log
import           Pos.Util.LoggerConfig (defaultInteractiveConfiguration)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logInfo,
                     namedTrace)
import           Pos.Util.UserSecret (readUserSecret, takeUserSecret, usKeys,
                     usPrimKey, usVss, usWallet, writeUserSecretRelease,
                     wusRootKey)

import           Dump (dumpFakeAvvmSeed, dumpGeneratedGenesisData,
                     dumpRichSecrets)
import           KeygenOptions (DumpAvvmSeedsOptions (..), GenKeysOptions (..),
                     KeygenCommand (..), KeygenOptions (..), getKeygenOptions)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

rearrangeKeyfile :: (MonadIO m, MonadThrow m) => TraceNamed m -> FilePath -> m ()
rearrangeKeyfile logTrace fp = do
    us <- takeUserSecret logTrace fp
    let sk = maybeToList $ us ^. usPrimKey
    writeUserSecretRelease $
        us & usKeys %~ (++ map noPassEncrypt sk)

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

rearrange :: (MonadIO m, MonadThrow m) => TraceNamed m -> FilePath -> m ()
rearrange logTrace msk = mapM_ (rearrangeKeyfile logTrace) =<< liftIO (glob msk)

genPrimaryKey
    :: (MonadIO m, MonadThrow m)
    => TraceNamed m
    -> FilePath
    -> m ()
genPrimaryKey logTrace path = do
    rs <- liftIO generateRichSecrets
    dumpRichSecrets logTrace path rs
    let pk = toPublic (rsPrimaryKey rs)
    logInfo logTrace $
        sformat
            ("Successfully generated primary key and dumped to "%string%
             ", stakeholder id: "%hashHexF%
             ", PK (base64): "%fullPublicKeyF)
            path
            (addressHash pk)
            pk

readKey :: MonadIO m => TraceNamed m -> FilePath -> m ()
readKey logTrace path = do
    us <- readUserSecret logTrace path
    logInfo logTrace $ maybe "No Primary key"
                    (("Primary: " <>) . showKeyWithAddressHash) $
                    view usPrimKey us
    logInfo logTrace $ maybe "No wallet set"
                    (("Wallet set: " <>) . showKeyWithAddressHash . decryptESK . view wusRootKey) $
                    view usWallet us
    logInfo logTrace $ "Keys: " <> (T.concat $ L.intersperse "\n" $
                           map (showKeyWithAddressHash . decryptESK) $
                           view usKeys us)
    logInfo logTrace $ maybe "No vss"
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
    :: MonadIO m
    => TraceNamed m -> DumpAvvmSeedsOptions -> m ()
dumpAvvmSeeds logTrace DumpAvvmSeedsOptions{..} = do
    logInfo logTrace $ "Generating fake avvm data into " <> fromString dasPath
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

    logInfo logTrace $ "Seeds were generated"

generateKeysByGenesis
    :: (HasConfigurations, MonadIO m, MonadThrow m)
    => TraceNamed m -> GenKeysOptions -> m ()
generateKeysByGenesis logTrace GenKeysOptions{..} = do
    case ccGenesis coreConfiguration of
        GCSrc {} ->
            error $ "Launched source file conf"
        GCSpec {} -> do
            dumpGeneratedGenesisData logTrace (gkoOutDir, gkoKeyPattern)
            logInfo logTrace (toText gkoOutDir <> " generated successfully")

genVssCert
    :: (HasConfigurations, MonadIO m)
    => TraceNamed m -> ProtocolMagic -> FilePath -> m ()
genVssCert logTrace pm path = do
    us <- readUserSecret logTrace path
    let primKey = fromMaybe (error "No primary key") (us ^. usPrimKey)
        vssKey  = fromMaybe (error "No VSS key") (us ^. usVss)
    let cert = mkVssCertificate
                 pm
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
    lh <- Log.setupLogging $ defaultInteractiveConfiguration Log.Debug
    let logTrace = appendName "keygen" $ namedTrace lh
    Log.loggerBracket lh "keygen" $
        withConfigurations logTrace Nothing koConfigurationOptions $ \pm _ _ -> do

        logInfo logTrace "Processing command"
        case koCommand of
            RearrangeMask msk       -> rearrange logTrace msk
            GenerateKey path        -> genPrimaryKey logTrace path
            GenerateVss path        -> genVssCert logTrace pm path
            ReadKey path            -> readKey logTrace path
            DumpAvvmSeeds opts      -> dumpAvvmSeeds logTrace opts
            GenerateKeysBySpec gkbg -> generateKeysByGenesis logTrace gkbg
            DumpGenesisData dgdPath dgdCanonical
                                    -> CLI.dumpGenesisData logTrace dgdCanonical dgdPath
