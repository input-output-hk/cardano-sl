module Main
       ( main
       ) where

import           Universum

import           Control.Lens          ((?~))
import           Data.Aeson            (eitherDecode)
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.List             as L
import qualified Data.Text             as T
import           Formatting            (build, sformat, (%))
import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath       (takeDirectory, (</>))
import           System.FilePath.Glob  (glob)
import           System.Wlog           (Severity (Debug), WithLogger, consoleOutB,
                                        lcTermSeverity, logInfo, setupLogging,
                                        usingLoggerName)

import           Pos.Binary            (asBinary)
import           Pos.Core              (addressHash)
import           Pos.Crypto            (EncryptedSecretKey (..), VssKeyPair,
                                        noPassEncrypt, redeemPkB64F, toVssPublicKey)
import           Pos.Crypto.Signing    (SecretKey (..), toPublic)
import           Pos.Genesis           (GenesisAvvmBalances, aeCoin, avvmData,
                                        convertAvvmDataToBalances,
                                        genesisDevHdwSecretKeys, genesisDevSecretKeys)
import           Pos.Testnet           (generateFakeAvvm, generateKeyfile)

import           Pos.Launcher          (applyConfigInfo)
import           Pos.Util.UserSecret   (readUserSecret, takeUserSecret, usKeys, usPrimKey,
                                        usVss, usWalletSet, writeUserSecretRelease)
import           Pos.Util.Util         (applyPattern)
import           Pos.Wallet.Web.Secret (wusRootKey)

import           Avvm                  (applyBlacklisted)
import           KeygenOptions         (AvvmBalanceOptions (..),
                                        DumpAvvmSeedsOptions (..), KeygenCommand (..),
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

-- Reads avvm json file and returns related 'GenesisAvvmBalances'
_readAvvmGenesis
    :: (MonadIO m, WithLogger m, MonadThrow m, MonadFail m)
    => AvvmBalanceOptions -> m GenesisAvvmBalances
_readAvvmGenesis AvvmBalanceOptions {..} = do
    logInfo "Reading avvm data"
    jsonfile <- liftIO $ BSL.readFile asoJsonPath
    case eitherDecode jsonfile of
        Left err       -> error $ toText err
        Right avvmData' -> do
            avvmDataFiltered <- applyBlacklisted asoBlacklisted avvmData'
            let totalAvvmBalance = sum $ map aeCoin $ avvmData avvmDataFiltered
            logInfo $ "Total avvm balance after applying blacklist: " <> show totalAvvmBalance
            pure $ convertAvvmDataToBalances avvmDataFiltered

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

rearrange :: (MonadIO m, MonadThrow m, WithLogger m) => FilePath -> m ()
rearrange msk = mapM_ rearrangeKeyfile =<< liftIO (glob msk)

genPrimaryKey :: (MonadIO m, MonadThrow m, WithLogger m) => FilePath -> m ()
genPrimaryKey path = do
    _ <- generateKeyfile True Nothing (Just path)
    logInfo $ "Successfully generated primary key " <> (toText path)

readKey :: (MonadIO m, MonadThrow m, WithLogger m) => FilePath -> m ()
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

dumpKeys :: (MonadIO m, MonadThrow m, WithLogger m) => FilePath -> m ()
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

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

main :: IO ()
main = do
    KeygenOptions{..} <- getKeygenOptions
    applyConfigInfo koConfigInfo
    setupLogging $ consoleOutB & lcTermSeverity ?~ Debug
    usingLoggerName "keygen" $ do
        logInfo "Processing command"
        case koCommand of
            RearrangeMask msk  -> rearrange msk
            GenerateKey path   -> genPrimaryKey path
            ReadKey path       -> readKey path
            DumpDevGenKeys pat -> dumpKeys pat
            DumpAvvmSeeds opts -> dumpAvvmSeeds opts
