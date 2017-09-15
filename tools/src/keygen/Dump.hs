-- | Generation of genesis data for testnet.

module Dump
       ( dumpKeyfiles
       , dumpKeyfile
       , dumpFakeAvvmGenesis
       , dumpFakeAvvmSeed
       ) where

import           Universum

import           Control.Lens          ((?~))
import qualified Data.Text             as T
import qualified Serokell.Util.Base64  as B64
import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath       ((</>))
import           System.Wlog           (WithLogger, logInfo)

import           Pos.Core.Genesis      (FakeAvvmOptions (..), TestnetBalanceOptions (..),
                                        generateFakeAvvmSeed, generateSecrets)
import           Pos.Crypto            (noPassEncrypt)
import           Pos.Util.UserSecret   (initializeUserSecret, takeUserSecret, usKeys,
                                        usPrimKey, usVss, usWalletSet,
                                        writeUserSecretRelease)
import           Pos.Wallet.Web.Secret (mkGenesisWalletUserSecret)


dumpKeyfiles
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => (FilePath, FilePath) -- directory and key-file pattern
    -> TestnetBalanceOptions
    -> m ()
dumpKeyfiles (dir, pat) TestnetBalanceOptions{..} = do
    let keysDir = dir </> "keys-testnet"
    let richDir = keysDir </> "rich"
    let poorDir = keysDir </> "poor"
    logInfo $ "Generating testnet data into " <> fromString keysDir
    liftIO $ createDirectoryIfMissing True richDir
    liftIO $ createDirectoryIfMissing True poorDir

    let totalStakeholders = tboRichmen + tboPoors

    forM_ [1 .. tboRichmen] $ \i ->
        dumpKeyfile True (richDir </> applyPattern pat i)
    forM_ [1 .. tboPoors] $ \i ->
        dumpKeyfile False (poorDir </> applyPattern pat i)

    logInfo $ show totalStakeholders <> " keyfiles are generated"

dumpFakeAvvmGenesis
    :: (MonadIO m, WithLogger m)
    => FilePath -> FakeAvvmOptions -> m ()
dumpFakeAvvmGenesis dir FakeAvvmOptions{..} = do
    let keysDir = dir </> "keys-fakeavvm"
    logInfo $ "Generating fake avvm data into " <> fromString keysDir
    liftIO $ createDirectoryIfMissing True keysDir

    forM_ [1 .. faoCount] $
        dumpFakeAvvmSeed . (\x -> keysDir </> ("fake-" <> show x <> ".seed"))

    logInfo (show faoCount <> " fake avvm seeds are generated")

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

dumpKeyfile
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => Bool
    -> FilePath
    -> m ()
    -- ^ secret key, vss key pair, vss certificate,
    -- hd wallet account address with bootstrap era distribution
dumpKeyfile isPrim fp = do
    (sk, hdwSk, vss) <- generateSecrets Nothing

    initializeUserSecret fp
    us <- takeUserSecret fp

    writeUserSecretRelease $
        us & (if isPrim
            then usPrimKey .~ Just sk
            else (usKeys %~ (noPassEncrypt sk :))
                . (usWalletSet ?~ mkGenesisWalletUserSecret hdwSk))
        & usVss .~ Just vss

dumpFakeAvvmSeed :: MonadIO m => FilePath -> m ByteString
dumpFakeAvvmSeed fp = do
    seed <- generateFakeAvvmSeed
    seed <$ flip writeFile (B64.encode seed) fp

-- | Replace "{}" with the result of applying 'show' to the given
-- value. Used in keygen, probably shouldn't be here.
applyPattern :: Show a => FilePath -> a -> FilePath
applyPattern fp a = replace "{}" (show a) fp
  where
    replace :: FilePath -> FilePath -> FilePath -> FilePath
    replace x b = toString . (T.replace `on` toText) x b . toText
