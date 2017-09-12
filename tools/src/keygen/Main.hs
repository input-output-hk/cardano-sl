module Main
       ( main
       ) where

import           Universum

import           Control.Lens               ((?~))
import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.List                  as L
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Formatting                 (build, sformat, (%))
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            (takeDirectory, (</>))
import           System.FilePath.Glob       (glob)
import           System.Wlog                (Severity (Debug), WithLogger, consoleOutB,
                                             lcTermSeverity, logError, logInfo,
                                             setupLogging, usingLoggerName)
import           Text.JSON.Canonical        (fromJSON, parseCanonicalJSON,
                                             prettyCanonicalJSON, toJSON)

import           Pos.Binary                 (asBinary)
import           Pos.Core                   (addressHash)
import           Pos.Crypto                 (EncryptedSecretKey (..), VssKeyPair,
                                             noPassEncrypt, redeemPkB64F, toVssPublicKey)
import           Pos.Crypto.Signing         (SecretKey (..), toPublic)
import           Pos.Genesis                (AddrDistribution, GenesisSpec (..),
                                             genesisDevHdwSecretKeys,
                                             genesisDevSecretKeys, noGenesisDelegation)
import           Pos.Testnet                (genFakeAvvmGenesis, genTestnetData,
                                             generateFakeAvvm, generateKeyfile)
import           Pos.Util.UserSecret        (readUserSecret, usKeys, usPrimKey, usVss,
                                             usWalletSet)
import           Pos.Util.UserSecret        (takeUserSecret, writeUserSecretRelease)
import           Pos.Util.Util              (applyPattern, leftToPanic)
import           Pos.Wallet.Web.Secret      (wusRootKey)

import           Avvm                       (aeCoin, applyBlacklisted,
                                             avvmAddrDistribution, utxo)
import           KeygenOptions              (AvvmBalanceOptions (..),
                                             DumpAvvmSeedsOptions (..),
                                             GenKeysOptions (..), GenesisGenOptions (..),
                                             KeygenCommand (..), KeygenOptions (..),
                                             getKeygenOptions)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

rearrangeKeyfile :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
rearrangeKeyfile fp = do
    us <- takeUserSecret fp
    let sk = maybeToList $ us ^. usPrimKey
    writeUserSecretRelease $
        us & usKeys %~ (++ map noPassEncrypt sk)

-- Reads avvm json file and returns related 'AddrDistribution'
readAvvmGenesis
    :: (MonadIO m, WithLogger m, MonadFail m)
    => AvvmBalanceOptions -> m AddrDistribution
readAvvmGenesis AvvmBalanceOptions {..} = do
    logInfo "Reading avvm data"
    jsonfile <- liftIO $ BSL.readFile asoJsonPath
    case eitherDecode jsonfile of
        Left err       -> error $ toText err
        Right avvmData -> do
            avvmDataFiltered <- applyBlacklisted asoBlacklisted avvmData
            let totalAvvmBalance = sum $ map aeCoin $ utxo avvmDataFiltered
            logInfo $ "Total avvm balance after applying blacklist: " <> show totalAvvmBalance
            pure $ avvmAddrDistribution avvmDataFiltered

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

rearrange :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
rearrange msk = mapM_ rearrangeKeyfile =<< liftIO (glob msk)

genPrimaryKey :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
genPrimaryKey path = do
    _ <- generateKeyfile True Nothing (Just path)
    logInfo $ "Successfully generated primary key " <> (toText path)

readKey :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
readKey path = do
    us <- readUserSecret path
    logInfo $ maybe "No Pimary key"
                    (("Primary: " <>) . showKeyWithAddressHash) $
                    view usPrimKey us
    logInfo $ maybe "No wallet set"
                    (("Wallet set: " <>) . showKeyWithAddressHash . decryptESK . view wusRootKey) $
                    view usWalletSet us
    logInfo $ "Keys: " <> (T.concat $ L.intersperse "\n" $
                           map (showKeyWithAddressHash . decryptESK) $
                           view usKeys us)
    logInfo $ maybe "No vss"
                    (("Vss PK: " <>) . showPvssKey) $
                    view usVss us

showKeyWithAddressHash :: SecretKey -> Text
showKeyWithAddressHash sk = sformat (build%"; address hash: "%build) pk ah
  where
    pk = toPublic sk
    ah = addressHash pk

showPvssKey :: VssKeyPair -> Text
showPvssKey = sformat build . asBinary . toVssPublicKey

decryptESK :: EncryptedSecretKey -> SecretKey
decryptESK (EncryptedSecretKey sk _) = SecretKey sk

dumpKeys :: (MonadIO m, MonadFail m, WithLogger m) => FilePath -> m ()
dumpKeys pat = do
    let keysDir = takeDirectory pat
    liftIO $ createDirectoryIfMissing True keysDir
    for_ (zip3 [1 ..] genesisDevSecretKeys genesisDevHdwSecretKeys) $
        \(i :: Int, k, wk) ->
        generateKeyfile False (Just (k, wk)) $ Just $ applyPattern pat i

dumpAvvmSeeds
    :: (MonadIO m, WithLogger m)
    => DumpAvvmSeedsOptions -> m ()
dumpAvvmSeeds DumpAvvmSeedsOptions{..} = do
    logInfo $ "Generating fake avvm data into " <> fromString dasPath
    liftIO $ createDirectoryIfMissing True dasPath

    when (dasNumber <= 0) $ error $
        "number of seeds should be positive, but it's " <> show dasNumber

    fakeAvvmPubkeys <- forM [1 .. dasNumber] $
        generateFakeAvvm . Just . (\x -> dasPath </> ("key"<>show x<>".seed"))
    forM_ (fakeAvvmPubkeys `zip` [1..dasNumber]) $
        \(rPk,i) -> writeFile (dasPath </> "key"<>show i<>".pk")
                              (sformat redeemPkB64F rPk)

    logInfo $ "Seeds were generated"

genGenesisFiles
    :: (MonadIO m, MonadFail m, WithLogger m)
    => GenesisGenOptions -> m ()
genGenesisFiles GenesisGenOptions{..} = do
    when (isNothing ggoAvvmBalance &&
          isNothing ggoTestBalance &&
          isNothing ggoFakeAvvmBalance) $
        error "At least one of options (AVVM balance or testnet balance) \
              \should be provided"

    logInfo "Generating AVVM distribution"
    mAvvmAddrDistr <- traverse readAvvmGenesis ggoAvvmBalance

    ------ Generating genesis JSON data

    -- avvm address distribution
    let gcdAvvmAddrDistribution = maybe [] one mAvvmAddrDistr

    -- [CSL-1596] TODO: add CLI for it!
    let gcdHeavyDelegation = noGenesisDelegation
    let avvmGenCoreData =
            leftToPanic "Couldn't create genesis core data: " $
                mkGenesisCoreData
                    gcdAvvmAddrDistribution
                    (Map.fromList ggoBootStakeholders)
                    gcdHeavyDelegation
    let genSpec =
            GenesisSpec
                avvmGenCoreData
                ggoTestBalance
                ggoFakeAvvmBalance
                ggoSeed

    ------ Writing/dumping

    -- write genesis.json
    prettyJSON <- prettyCanonicalJSON <$> toJSON genSpec
    let name = "genesis.json"
    let path = ggoGenesisDir </> name
    liftIO $ createDirectoryIfMissing True (takeDirectory path)
    case parseCanonicalJSON (BSLC.pack prettyJSON) of
        Right _ -> do
            liftIO $ BSL.writeFile path $ BSLC.pack prettyJSON
            logInfo (toText name <> " generated successfully")
        Left err ->
            logError ("Generated GenesisCoreData can't be read: " <> toText err)

generateKeysByGenesis
    :: (MonadIO m, MonadFail m, WithLogger m)
    => GenKeysOptions -> m ()
generateKeysByGenesis GenKeysOptions{..} = do
    json <- liftIO $ BSL.readFile gkoGenesisJSON
    case parseCanonicalJSON json of
        Right jsValue -> do
            GenesisSpec{..} <- undefined -- fromJSON @_ @GenesisSpec jsValue
            whenJust gsFakeAvvmBalance $ void . genFakeAvvmGenesis (Just gkoOutDir)
            whenJust gsTestBalance $ void . genTestnetData (Just (gkoOutDir, gkoKeyPattern))
            logInfo (toText gkoOutDir <> " generated successfully")
        Left err ->
            logError ("Couldn't parse " <> toText gkoGenesisJSON <> " reason: " <> toText err)

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

main :: IO ()
main = do
    KeygenOptions{..} <- getKeygenOptions
    setupLogging $ consoleOutB & lcTermSeverity ?~ Debug
    usingLoggerName "keygen" $ do
        logInfo "Processing command"
        case koCommand of
            RearrangeMask msk          -> rearrange msk
            GenerateKey path           -> genPrimaryKey path
            ReadKey path               -> readKey path
            DumpDevGenKeys pat         -> dumpKeys pat
            DumpAvvmSeeds opts         -> dumpAvvmSeeds opts
            GenerateGenesis ggo        -> genGenesisFiles ggo
            GenerateKeysByGenesis gkbg -> generateKeysByGenesis gkbg
