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

import           Pos.Core.Configuration (HasGeneratedSecrets, generatedSecrets)
import           Pos.Core.Genesis (GeneratedSecrets (..), PoorSecret (..),
                     RichSecrets (..), poorSecretToEncKey)
import           Pos.Crypto (SecretKey)
import           Pos.Util.Trace.Named (TraceNamed, logInfo)
import           Pos.Util.UserSecret (UserSecret, initializeUserSecret,
                     mkGenesisWalletUserSecret, takeUserSecret, usKeys,
                     usPrimKey, usVss, usWallet, writeUserSecretRelease)

----------------------------------------------------------------------------
-- Dump individual secrets
----------------------------------------------------------------------------

dumpDlgIssuerSecret
    :: (MonadIO m, MonadThrow m)
    => TraceNamed m
    -> FilePath
    -> SecretKey
    -> m ()
dumpDlgIssuerSecret logTace fp sk = dumpUserSecret logTace fp $ usPrimKey .~ Just sk

dumpRichSecrets
    :: (MonadIO m, MonadThrow m)
    => TraceNamed m
    -> FilePath
    -> RichSecrets
    -> m ()
dumpRichSecrets logTace fp RichSecrets {..} =
    dumpUserSecret logTace fp $
    foldl' (.) identity [ usPrimKey .~ Just rsPrimaryKey
                        , usVss .~ Just rsVssKeyPair
                        ]

dumpPoorSecret
    :: (MonadIO m, MonadThrow m)
    => TraceNamed m
    -> FilePath
    -> PoorSecret
    -> m ()
dumpPoorSecret logTace fp poorSec = let hdwSk = poorSecretToEncKey poorSec in
    dumpUserSecret logTace fp $
    foldl' (.) identity [ usKeys %~ (hdwSk :)
                        , usWallet ?~ mkGenesisWalletUserSecret hdwSk
                        ]

dumpFakeAvvmSeed :: MonadIO m => FilePath -> ByteString -> m ()
dumpFakeAvvmSeed fp seed = writeFile fp (B64.encode seed)

----------------------------------------------------------------------------
-- Dump all generated secrets
----------------------------------------------------------------------------

dumpGeneratedGenesisData
    :: (MonadIO m, MonadThrow m, HasGeneratedSecrets)
    => TraceNamed m
    -> (FilePath, FilePath)
    -> m ()
dumpGeneratedGenesisData logTace (dir, pat) = do
    let GeneratedSecrets {..} =
            fromMaybe (error "GeneratedSecrets are unknown") generatedSecrets
    dumpKeyfiles logTace (dir, pat) gsDlgIssuersSecrets gsRichSecrets gsPoorSecrets
    dumpFakeAvvmSeeds logTace dir gsFakeAvvmSeeds

dumpKeyfiles
    :: (MonadIO m, MonadThrow m)
    => TraceNamed m
    -> (FilePath, FilePath) -- directory and key-file pattern
    -> [SecretKey]
    -> [RichSecrets]
    -> [PoorSecret]
    -> m ()
dumpKeyfiles logTrace (dir, pat) dlgIssuers richs poors = do
    let keysDir = dir </> "generated-keys"
    let dlgIssuersDir = keysDir </> "dlg-issuers"
    let richDir = keysDir </> "rich"
    let poorDir = keysDir </> "poor"
    logInfo logTrace $ "Dumping generated genesis secrets into " <> fromString keysDir
    mapM_ (liftIO . createDirectoryIfMissing True)
        [ dlgIssuersDir
        , richDir
        , poorDir
        ]

    let totalSecrets = length dlgIssuers + length richs + length poors

    let patternize = applyPattern @Int pat
    forM_ (enumerate dlgIssuers) $ \(i, sk) ->
        dumpDlgIssuerSecret logTrace (dlgIssuersDir </> patternize i) sk
    forM_ (enumerate richs) $ \(i, richSecrets) ->
        dumpRichSecrets logTrace (richDir </> patternize i) richSecrets
    forM_ (enumerate poors) $ \(i, hdwSk) ->
        dumpPoorSecret logTrace (poorDir </> patternize i) hdwSk

    logInfo logTrace $ show totalSecrets <> " keyfiles are generated"

dumpFakeAvvmSeeds
    :: MonadIO m
    => TraceNamed m
    -> FilePath
    -> [ByteString]
    -> m ()
dumpFakeAvvmSeeds logTrace dir seeds = do
    let keysDir = dir </> "keys-fakeavvm"
    logInfo logTrace $ "Generating fake avvm data into " <> fromString keysDir
    liftIO $ createDirectoryIfMissing True keysDir
    let faoCount = length seeds

    forM_ (enumerate seeds) $ \(i :: Int, seed) ->
        dumpFakeAvvmSeed (keysDir </> ("fake-" <> show i <> ".seed")) seed

    logInfo logTrace (show faoCount <> " fake avvm seeds are generated")

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

dumpUserSecret
    :: (MonadIO m, MonadThrow m)
    => TraceNamed m
    -> FilePath
    -> (UserSecret -> UserSecret)
    -> m ()
dumpUserSecret logTace fp operation = do
    initializeUserSecret logTace fp
    us <- takeUserSecret logTace fp
    writeUserSecretRelease (operation us)

-- Replace "{}" with the result of applying 'show' to the given
-- value.
applyPattern :: Show a => FilePath -> a -> FilePath
applyPattern fp a = replace "{}" (show a) fp
  where
    replace :: FilePath -> FilePath -> FilePath -> FilePath
    replace x b = toString . (T.replace `on` toText) x b . toText
