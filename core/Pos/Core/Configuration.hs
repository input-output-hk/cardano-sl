{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration
       ( ConfigurationError (..)
       , HasConfiguration
       , withCoreConfigurations
       , withGenesisSpec

       , canonicalGenesisJson

       , module E
       ) where

import           Universum

import qualified Crypto.Hash                             as Hash
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BSL
import           Formatting                              (sformat, shown, (%))
import           System.Wlog                             (WithLogger, logInfo)
import qualified Text.JSON.Canonical                     as Canonical

import           Pos.Core.Coin                           (coinToInteger)
import           Pos.Core.Configuration.BlockVersionData as E
import           Pos.Core.Configuration.Core             as E
import           Pos.Core.Configuration.GeneratedSecrets as E
import           Pos.Core.Configuration.GenesisData      as E
import           Pos.Core.Configuration.GenesisHash      as E
import           Pos.Core.Configuration.Protocol         as E
import           Pos.Core.Constants                      (genesisDataDigest)
import           Pos.Core.Genesis.Canonical              ()
import           Pos.Core.Genesis.Generate               (GeneratedGenesisData (..),
                                                          generateGenesisData)
import           Pos.Core.Genesis.Types                  (GenesisData (..),
                                                          GenesisInitializer (..),
                                                          GenesisSpec (..),
                                                          getGenesisAvvmBalances)
import           Pos.Core.Types                          (Coin, Timestamp)
import           Pos.Crypto.Hashing                      (AbstractHash (..), unsafeHash)

-- | Coarse catch-all configuration constraint for use by depending modules.
type HasConfiguration =
    ( HasCoreConfiguration
    , HasGenesisData
    , HasGenesisHash
    , HasGeneratedSecrets
    , HasGenesisBlockVersionData
    , HasProtocolConstants
    )

canonicalGenesisJson :: GenesisData -> (BSL.ByteString, Hash.Digest Hash.Blake2b_256)
canonicalGenesisJson theGenesisData = (canonicalJsonBytes, jsonHash)
  where
    jsonHash = Hash.hash $ BSL.toStrict canonicalJsonBytes
    canonicalJsonBytes = Canonical.renderCanonicalJSON $ runIdentity $ Canonical.toJSON theGenesisData

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

        gdataJSON <- case Canonical.parseCanonicalJSON (BSL.fromStrict bytes) of
            Left str -> throwM $ GenesisDataParseFailure (fromString str)
            Right it -> return it

        theGenesisData <- case Canonical.fromJSON gdataJSON of
            Left str -> throwM $ GenesisDataParseFailure (fromString str)
            Right it -> return it

        let (_, theGenesisHash) = canonicalGenesisJson theGenesisData
        when (theGenesisHash /= genesisDataDigest) $
            throwM $ GenesisHashMismatch (show theGenesisHash) (show genesisDataDigest)
        -- See Pos.Core.Constants. The genesisHash is conceptually distinct
        -- from the genesisDataDigest, they just happen to coincide for
        -- mainnet.

        withCoreConfiguration conf $
            withProtocolConstants (gdProtocolConsts theGenesisData) $
            withGenesisBlockVersionData (gdBlockVersionData theGenesisData) $
            withGenesisData theGenesisData $
            withGenesisHash (AbstractHash theGenesisHash) $
            withGeneratedSecrets Nothing $
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
        withProtocolConstants (gsProtocolConstants spec) $
        withGenesisBlockVersionData (gsBlockVersionData spec) $
            let avvmSum = foldr' ((+) . coinToInteger) 0 $ getGenesisAvvmBalances $ gsAvvmDistr spec
                maxTnBalance = fromIntegral $! coinToInteger (maxBound @Coin) - avvmSum
                GeneratedGenesisData {..} =
                    generateGenesisData (gsInitializer spec) maxTnBalance

                theGenesisData =
                   GenesisData
                      { gdBootStakeholders = ggdBootStakeholders
                      , gdHeavyDelegation  = gsHeavyDelegation spec
                      , gdStartTime        = theSystemStart
                      , gdVssCerts         = ggdVssCerts
                      , gdNonAvvmBalances  = ggdNonAvvm
                      , gdBlockVersionData = genesisBlockVersionData
                      , gdProtocolConsts   = protocolConstants
                      , gdAvvmDistr        = ggdAvvm <> gsAvvmDistr spec
                      , gdFtsSeed          = gsFtsSeed spec
                      }
                -- Anything will do for the genesis hash. A hash of "patak" was used
                -- before, and so it remains.
                theGenesisHash = unsafeHash @Text "patak"
             in withCoreConfiguration conf $
                  withGenesisHash theGenesisHash $
                  withGeneratedSecrets ggdSecrets $
                  withGenesisData theGenesisData val

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
