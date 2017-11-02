-- | Generation of genesis data for testnet.

module Dump
       ( dumpKeyfile
       , dumpFakeAvvmSeed
       , dumpGeneratedGenesisData
       ) where

import           Universum

import           Control.Lens          ((?~))
import           Crypto.Random         (MonadRandom)
import qualified Data.Text             as T
import qualified Serokell.Util.Base64  as B64
import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath       ((</>))
import           System.Wlog           (WithLogger, logInfo)

import           Pos.Core.Genesis      (GeneratedSecrets (..), TestnetBalanceOptions (..))
import           Pos.Crypto            (EncryptedSecretKey, SecretKey, VssKeyPair,
                                        noPassEncrypt)
import           Pos.Util.UserSecret   (initializeUserSecret, takeUserSecret, usKeys,
                                        usPrimKey, usVss, usWalletSet,
                                        writeUserSecretRelease)
import           Pos.Wallet.Web.Secret (mkGenesisWalletUserSecret)


dumpGeneratedGenesisData
    :: (MonadIO m, WithLogger m, MonadThrow m, MonadRandom m)
    => (FilePath, FilePath)
    -> TestnetBalanceOptions
    -> GeneratedSecrets
    -> m ()
dumpGeneratedGenesisData (dir, pat) tbo GeneratedSecrets {..} = do
    dumpKeyfiles (dir, pat) tbo gsSecretKeys
    dumpFakeAvvmSeeds dir gsFakeAvvmSeeds

dumpKeyfiles
    :: (MonadIO m, MonadThrow m, WithLogger m, MonadRandom m)
    => (FilePath, FilePath) -- directory and key-file pattern
    -> TestnetBalanceOptions
    -> [(SecretKey, EncryptedSecretKey, VssKeyPair)]
    -> m ()
dumpKeyfiles (dir, pat) TestnetBalanceOptions{..} secrets = do
    let keysDir = dir </> "keys-testnet"
    let richDir = keysDir </> "rich"
    let poorDir = keysDir </> "poor"
    logInfo $ "Generating testnet data into " <> fromString keysDir
    liftIO $ createDirectoryIfMissing True richDir
    liftIO $ createDirectoryIfMissing True poorDir

    let totalStakeholders = tboRichmen + tboPoors
    let (richmen, poors) = splitAt (fromIntegral tboRichmen) secrets

    forM_ (zip richmen [0 .. (tboRichmen - 1)]) $ \(sk, i) ->
        dumpKeyfile True (richDir </> applyPattern pat i) sk
    forM_ (zip poors [0 .. (tboPoors - 1)]) $ \(sk, i) ->
        dumpKeyfile False (poorDir </> applyPattern pat i) sk

    logInfo $ show totalStakeholders <> " keyfiles are generated"

dumpFakeAvvmSeeds
    :: (MonadIO m, WithLogger m)
    => FilePath
    -> [ByteString]
    -> m ()
dumpFakeAvvmSeeds dir seeds = do
    let keysDir = dir </> "keys-fakeavvm"
    logInfo $ "Generating fake avvm data into " <> fromString keysDir
    liftIO $ createDirectoryIfMissing True keysDir
    let faoCount = length seeds

    forM_ (zip seeds [1 .. faoCount]) $ \(seed, i) ->
        dumpFakeAvvmSeed (keysDir </> ("fake-" <> show i <> ".seed")) seed

    logInfo (show faoCount <> " fake avvm seeds are generated")

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

dumpKeyfile
    :: (MonadIO m, MonadThrow m, WithLogger m, MonadRandom m)
    => Bool
    -> FilePath
    -> (SecretKey, EncryptedSecretKey, VssKeyPair)
    -> m ()
dumpKeyfile isPrim fp (sk, hdwSk, vss) = do
    initializeUserSecret fp
    us <- takeUserSecret fp

    writeUserSecretRelease $
        us & (if isPrim
            then usPrimKey .~ Just sk
            else (usKeys %~ (noPassEncrypt sk :))
                . (usWalletSet ?~ mkGenesisWalletUserSecret hdwSk))
        & usVss .~ Just vss

dumpFakeAvvmSeed :: MonadIO m => FilePath -> ByteString -> m ()
dumpFakeAvvmSeed fp seed = flip writeFile (B64.encode seed) fp

-- | Replace "{}" with the result of applying 'show' to the given
-- value. Used in keygen, probably shouldn't be here.
applyPattern :: Show a => FilePath -> a -> FilePath
applyPattern fp a = replace "{}" (show a) fp
  where
    replace :: FilePath -> FilePath -> FilePath -> FilePath
    replace x b = toString . (T.replace `on` toText) x b . toText
