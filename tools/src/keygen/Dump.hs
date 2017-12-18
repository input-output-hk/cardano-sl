-- | Generation of genesis data for testnet.

module Dump
       ( dumpRichSecrets
       , dumpFakeAvvmSeed
       , dumpGeneratedGenesisData
       ) where

import           Universum

import           Control.Lens ((?~))
import qualified Data.Text as T
import           Serokell.Util (enumerate)
import qualified Serokell.Util.Base64 as B64
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.Wlog (WithLogger, logInfo)

import           Pos.Core.Configuration (HasGeneratedSecrets, generatedSecrets)
import           Pos.Core.Genesis (GeneratedSecrets (..), PoorSecret (..), RichSecrets (..),
                                   poorSecretToEncKey)
import           Pos.Crypto (SecretKey)
import           Pos.Util.UserSecret (UserSecret, initializeUserSecret, mkGenesisWalletUserSecret,
                                      takeUserSecret, usKeys, usPrimKey, usVss, usWallet,
                                      writeUserSecretRelease)

----------------------------------------------------------------------------
-- Dump individual secrets
----------------------------------------------------------------------------

dumpDlgIssuerSecret
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => FilePath
    -> SecretKey
    -> m ()
dumpDlgIssuerSecret fp sk = dumpUserSecret fp $ usPrimKey .~ Just sk

dumpRichSecrets
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => FilePath
    -> RichSecrets
    -> m ()
dumpRichSecrets fp RichSecrets {..} =
    dumpUserSecret fp $
    foldl' (.) identity [ usPrimKey .~ Just rsPrimaryKey
                        , usVss .~ Just rsVssKeyPair
                        ]

dumpPoorSecret
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => FilePath
    -> PoorSecret
    -> m ()
dumpPoorSecret fp poorSec = let hdwSk = poorSecretToEncKey poorSec in
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
    :: (MonadIO m, WithLogger m, MonadThrow m, HasGeneratedSecrets)
    => (FilePath, FilePath)
    -> m ()
dumpGeneratedGenesisData (dir, pat) = do
    let GeneratedSecrets {..} =
            fromMaybe (error "GeneratedSecrets are unknown") generatedSecrets
    dumpKeyfiles (dir, pat) gsDlgIssuersSecrets gsRichSecrets gsPoorSecrets
    dumpFakeAvvmSeeds dir gsFakeAvvmSeeds

dumpKeyfiles
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => (FilePath, FilePath) -- directory and key-file pattern
    -> [SecretKey]
    -> [RichSecrets]
    -> [PoorSecret]
    -> m ()
dumpKeyfiles (dir, pat) dlgIssuers richs poors = do
    let keysDir = dir </> "generated-keys"
    let dlgIssuersDir = keysDir </> "dlg-issuers"
    let richDir = keysDir </> "rich"
    let poorDir = keysDir </> "poor"
    logInfo $ "Dumping generated genesis secrets into " <> fromString keysDir
    mapM_ (liftIO . createDirectoryIfMissing True)
        [ dlgIssuersDir
        , richDir
        , poorDir
        ]

    let totalSecrets = length dlgIssuers + length richs + length poors

    let patternize = applyPattern @Int pat
    forM_ (enumerate dlgIssuers) $ \(i, sk) ->
        dumpDlgIssuerSecret (dlgIssuersDir </> patternize i) sk
    forM_ (enumerate richs) $ \(i, richSecrets) ->
        dumpRichSecrets (richDir </> patternize i) richSecrets
    forM_ (enumerate poors) $ \(i, hdwSk) ->
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

    forM_ (enumerate seeds) $ \(i :: Int, seed) ->
        dumpFakeAvvmSeed (keysDir </> ("fake-" <> show i <> ".seed")) seed

    logInfo (show faoCount <> " fake avvm seeds are generated")

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

dumpUserSecret
    :: (MonadIO m, MonadThrow m, WithLogger m)
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
