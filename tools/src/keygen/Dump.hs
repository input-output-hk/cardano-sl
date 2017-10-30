-- | Generation of genesis data for testnet.

module Dump
       ( dumpRichSecrets
       , dumpFakeAvvmSeed
       , dumpGeneratedGenesisData
       ) where

import           Universum

import           Control.Lens           ((?~))
import           Crypto.Random          (MonadRandom)
import qualified Data.Text              as T
import qualified Serokell.Util.Base64   as B64
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        ((</>))
import           System.Wlog            (WithLogger, logInfo)

import           Pos.Core.Configuration (HasGeneratedSecrets, generatedSecrets)
import           Pos.Core.Genesis       (GeneratedSecrets (..), RichSecrets (..))
import           Pos.Crypto             (EncryptedSecretKey)
import           Pos.Util.UserSecret    (UserSecret, initializeUserSecret,
                                         mkGenesisWalletUserSecret, takeUserSecret,
                                         usKeys, usPrimKey, usVss, usWallet,
                                         writeUserSecretRelease)

----------------------------------------------------------------------------
-- Dump individual secrets
----------------------------------------------------------------------------

dumpRichSecrets
    :: (MonadIO m, MonadThrow m, WithLogger m, MonadRandom m)
    => FilePath
    -> RichSecrets
    -> m ()
dumpRichSecrets fp RichSecrets {..} =
    dumpUserSecret fp $
    foldl' (.) identity [ usPrimKey .~ Just rsPrimaryKey
                        , usVss .~ Just rsVssKeyPair
                        ]

dumpPoorSecret
    :: (MonadIO m, MonadThrow m, WithLogger m, MonadRandom m)
    => FilePath
    -> EncryptedSecretKey
    -> m ()
dumpPoorSecret fp hdwSk =
    dumpUserSecret fp $
    foldl' (.) identity [ usKeys %~ (hdwSk :)
                        , usWallet ?~ mkGenesisWalletUserSecret hdwSk
                        ]

dumpFakeAvvmSeed :: MonadIO m => FilePath -> ByteString -> m ()
dumpFakeAvvmSeed fp seed = writeFile fp (B64.encode seed)

----------------------------------------------------------------------------
-- Dump all generated secrets
----------------------------------------------------------------------------

dumpGeneratedGenesisData
    :: (MonadIO m, WithLogger m, MonadThrow m, MonadRandom m, HasGeneratedSecrets)
    => (FilePath, FilePath)
    -> m ()
dumpGeneratedGenesisData (dir, pat) = do
    let GeneratedSecrets {..} =
            fromMaybe (error "GeneratedSecrets are unknown") generatedSecrets
    dumpKeyfiles (dir, pat) gsRichSecrets gsPoorSecrets
    dumpFakeAvvmSeeds dir gsFakeAvvmSeeds

dumpKeyfiles
    :: (MonadIO m, MonadThrow m, WithLogger m, MonadRandom m)
    => (FilePath, FilePath) -- directory and key-file pattern
    -> [RichSecrets]
    -> [EncryptedSecretKey]
    -> m ()
dumpKeyfiles (dir, pat) richs poors = do
    let keysDir = dir </> "generated-keys"
    let richDir = keysDir </> "rich"
    let poorDir = keysDir </> "poor"
    logInfo $ "Dumping generated genesis secrets into " <> fromString keysDir
    liftIO $ createDirectoryIfMissing True richDir
    liftIO $ createDirectoryIfMissing True poorDir

    let totalSecrets = length richs + length poors

    let patternize = applyPattern @Int pat
    forM_ (zip richs [1 .. ]) $ \(richSecrets, i) ->
        dumpRichSecrets (richDir </> patternize i) richSecrets
    forM_ (zip poors [1 .. ]) $ \(hdwSk, i) ->
        dumpPoorSecret (poorDir </> patternize i) hdwSk

    logInfo $ show totalSecrets <> " keyfiles are generated"

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

dumpUserSecret
    :: (MonadIO m, MonadThrow m, WithLogger m, MonadRandom m)
    => FilePath
    -> (UserSecret -> UserSecret)
    -> m ()
dumpUserSecret fp operation = do
    initializeUserSecret fp
    us <- takeUserSecret fp
    writeUserSecretRelease (operation us)

-- Replace "{}" with the result of applying 'show' to the given
-- value.
applyPattern :: Show a => FilePath -> a -> FilePath
applyPattern fp a = replace "{}" (show a) fp
  where
    replace :: FilePath -> FilePath -> FilePath -> FilePath
    replace x b = toString . (T.replace `on` toText) x b . toText
