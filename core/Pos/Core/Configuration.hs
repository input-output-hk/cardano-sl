{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration
       ( ConfigurationError (..)
       , HasConfiguration
       , withCoreConfigurations
       , withGenesisSpec

       , module Pos.Core.Configuration.BlockVersionData
       , module Pos.Core.Configuration.Core
       , module Pos.Core.Configuration.GeneratedGenesisData
       , module Pos.Core.Configuration.GenesisAvvmBalances
       , module Pos.Core.Configuration.GenesisDelegation
       , module Pos.Core.Configuration.GenesisHash
       , module Pos.Core.Configuration.Protocol
       , module Pos.Core.Configuration.SharedSeed
       , module Pos.Core.Configuration.SystemStart
       ) where

import           Universum

import qualified Crypto.Hash                                 as Hash
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BSL
import           Formatting                                  (sformat, shown, (%))
import           System.Wlog                                 (WithLogger, logInfo)
import qualified Text.JSON.Canonical                         as Canonical

import           Pos.Core.Configuration.BlockVersionData
import           Pos.Core.Configuration.Core
import           Pos.Core.Configuration.GeneratedGenesisData
import           Pos.Core.Configuration.GenesisAvvmBalances
import           Pos.Core.Configuration.GenesisDelegation
import           Pos.Core.Configuration.GenesisHash
import           Pos.Core.Configuration.Protocol
import           Pos.Core.Configuration.SharedSeed
import           Pos.Core.Configuration.SystemStart
import           Pos.Core.Constants                          (genesisDataDigest)
import           Pos.Core.Genesis.Canonical                  ()
import           Pos.Core.Genesis.Generate                   (GeneratedGenesisData (..),
                                                              generateGenesisData)
import           Pos.Core.Genesis.Types                      (GenesisData (..),
                                                              GenesisInitializer (..),
                                                              GenesisSpec (..))
import           Pos.Core.Types                              (Timestamp)
import           Pos.Crypto.Hashing                          (AbstractHash (..),
                                                              unsafeHash)

-- | Coarse catch-all configuration constraint for use by depending modules.
type HasConfiguration =
    ( HasCoreConfiguration
    , HasProtocolConstants
    , HasGenesisBlockVersionData
    , HasGenesisHash
    , HasSharedSeed
    , HasSystemStart
    , HasGeneratedGenesisData
    , HasGenesisDelegation
    , HasGenesisAvvmBalances
    )

-- | Come up with a HasConfiguration constraint using a Configuration.
-- The Configuration record can be parsed from JSON or Yaml, and used to
-- get a GenesisSpec, either from the file itself or from another file:
-- the canonical JSON encoding of a mainnet genesis.
--
-- If the canonical JSON source is given, then it will be hashed and checked
-- against the constant expected genesisDataDigest.
--
-- If the configuration gives a testnet genesis spec, then a start time must
-- be provided, probably sourced from command line arguments.
withCoreConfigurations
    :: ( MonadThrow m
       , WithLogger m
       , MonadIO m
       , Canonical.FromJSON (Either String) GenesisData
       )
    => CoreConfiguration
    -> Maybe Timestamp
    -- ^ Optional system start time.
    --   It must be given when the genesis spec uses a testnet initializer.
    -> (HasConfiguration => m r)
    -> m r
withCoreConfigurations conf@CoreConfiguration{..} mSystemStart act = case ccGenesis of
    -- If a 'GenesisData' source file is given, we check its hash against the
    -- built-in constant, parse it, and use the GenesisData to fill in all of]
    -- the obligations.
    GCSrc fp -> do
        !bytes <- liftIO $ BS.readFile fp

        let digest :: Hash.Digest Hash.Blake2b_256
            digest = Hash.hash bytes
        when (digest /= genesisDataDigest) $
            throwM $ GenesisHashMismatch (show digest) (show genesisDataDigest)
        -- See Pos.Core.Constants. The genesisHash is conceptually distinct
        -- from the genesisDataDigest, they just happen to coincide for
        -- mainnet.
        let theGenesisHash = AbstractHash digest

        gdataJSON <- case Canonical.parseCanonicalJSON (BSL.fromStrict bytes) of
            Left str -> throwM $ GenesisDataParseFailure (fromString str)
            Right it -> return it

        gdata <- case Canonical.fromJSON gdataJSON of
            Left str -> throwM $ GenesisDataParseFailure (fromString str)
            Right it -> return it

        let theProtocolConstants = gdProtocolConsts gdata
            theBlockVersionData = gdBlockVersionData gdata
            theSharedSeed = gdFtsSeed gdata
            theGenesisAvvmBalances = gdAvvmDistr gdata
            theGenesisDelegation = gdHeavyDelegation gdata
            theSystemStart = gdStartTime gdata
            theGeneratedGData = GeneratedGenesisData [] (gdBootStakeholders gdata) (gdVssCerts gdata) Nothing Nothing

        withCoreConfiguration conf $
          withProtocolConstants theProtocolConstants $
          withGenesisBlockVersionData theBlockVersionData $
          withGenesisAvvmBalances theGenesisAvvmBalances $
          withGenesisDelegation theGenesisDelegation $
          withGenesisHash theGenesisHash $
          withSharedSeed theSharedSeed $
          withSystemStart theSystemStart $
          withGeneratedGenesisData theGeneratedGData $
          act

    -- If a 'GenesisSpec' is given, we ensure we have a start time (needed if
    -- it's a testnet initializer) and then make a 'GenesisData' from it.
    GCSpec spec -> do

        theSystemStart <- case mSystemStart of
            Just it -> case gsInitializer spec of
                TestnetInitializer{..} -> do
                    logInfo $ sformat ("withConfiguration using custom system start time "%shown) it
                    return it
                MainnetInitializer{..} -> throwM UnnecessarySystemStartTime
            Nothing -> case gsInitializer spec of
                TestnetInitializer{..} -> throwM MissingSystemStartTime
                MainnetInitializer{..} -> do
                    logInfo $ sformat ("withConfiguration using genesis configured system start time "%shown) miStartTime
                    return miStartTime

        withGenesisSpec theSystemStart conf act

withGenesisSpec
    :: Timestamp
    -> CoreConfiguration
    -> (HasConfiguration => r)
    -> r
withGenesisSpec theSystemStart conf@CoreConfiguration{..} val = case ccGenesis of
    GCSrc _ -> error "withGenesisSpec called with GCSrc"
    GCSpec spec ->
        -- We can immediately get everything we need except for the generated
        -- genesis data. To get that we need the protocol constants and block
        -- version data
        let theProtocolConstants = gsProtocolConstants spec
            theBlockVersionData = gsBlockVersionData spec
            theSharedSeed = gsFtsSeed spec
            theGenesisAvvmBalances = gsAvvmDistr spec
            theGenesisDelegation = gsHeavyDelegation spec
            -- Anything will do for the genesis hash. A hash of "patak" was used
            -- before, and so it remains.
            theGenesisHash = unsafeHash @Text "patak"
         in withCoreConfiguration conf $
                withProtocolConstants theProtocolConstants $
                withGenesisBlockVersionData theBlockVersionData $
                withGenesisAvvmBalances theGenesisAvvmBalances $
                withGenesisDelegation theGenesisDelegation $
                withGenesisHash theGenesisHash $
                withSharedSeed theSharedSeed $
                withSystemStart theSystemStart $
                    let theGeneratedGData = generateGenesisData (gsInitializer spec)
                     in withGeneratedGenesisData theGeneratedGData val


data ConfigurationError =
      -- | A system start time must be given when a testnet genesis is used.
      -- A mainnet genesis has this built-in, so it's not needed.
      MissingSystemStartTime

      -- | Must not give a custom system start time when using a mainnet
      -- genesis.
    | UnnecessarySystemStartTime

    | GenesisDataParseFailure !Text

      -- | The GenesisData canonical JSON hash is different than expected.
    | GenesisHashMismatch !Text !Text

    | ConfigurationInternalError !Text
    deriving (Show)

instance Exception ConfigurationError
