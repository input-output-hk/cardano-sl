module CLI where

import           Universum

import           Data.String.Conv (toS)
import           Options.Applicative (auto, eitherReader, help, long, metavar, option, strOption,
                                      switch)
import           Options.Generic (ParseRecord (..))
import           Pos.Util.Servant (decodeCType)
import           Pos.Wallet.Web.ClientTypes.Instances ()
import           Pos.Wallet.Web.ClientTypes.Types (AccountId, CAccountId (..))
import           Text.Read (readMaybe)

import           Types (Method (..))

data CLI = CLI
    { config            :: FilePath
    -- ^ The path to the config file
    , nodePath          :: FilePath
    -- ^ The path to a valid rocksdb database.
    , secretKeyPath     :: FilePath
    -- ^ The path to the secret key from the database.
    , walletPath        :: FilePath
    -- ^ The path to a valid acid-state database.
    , configurationPath :: FilePath
    -- ^ The path to a valid cardano-sl configuration.
    , configurationProf :: String
    -- ^ The configuration profile to use.
    , systemStart       :: Maybe Int
    -- ^ The systemStart for the application
    , showStats         :: Bool
    -- ^ If true, print the stats for the `wallet-db`
    , queryMethod       :: Maybe Method
    -- ^ If a method is defined, try to call it. For example,
    -- calling `GetWallet` could be useful.
    , addTo             :: Maybe AccountId
    -- ^ If specified, only append addresses to the
    -- given <wallet_id@account_id>
    }


instance ParseRecord CLI where
  parseRecord = CLI
              <$> (strOption (long "config" <> metavar "CONFIG.DHALL"
                             <> help "A path to a config file."
                                      ))
              <*> (strOption (long "nodeDB" <> metavar "rocksdb-path"
                             <> help "A path to a valid rocksdb database."
                                      ))
              <*> (strOption (long "secretKey" <> metavar "secret-key-path"
                             <> help "A path to a valid secreate key for the database."
                                      ))
              <*> (strOption (long "walletDB" <> metavar "acidstate-path"
                             <> help "A path to a valid acidstate database."
                                      ))
              <*> (strOption (long "configPath" <> metavar "configuration-path"
                             <> help "A path to a valid cardano-sl configuration."
                                      ))
              <*> (strOption (long "configProf" <> metavar "configuration-profile"
                             <> help "A valid cardano-sl configuration profile to use."
                                      ))
              <*> optional (option auto (long "systemStart"
                             <> help "A valid node system start to use."))
              <*> switch (long "stats" <> help "Show stats for this wallet.")
              <*> ((readMaybe =<<) <$> (optional (strOption (long "query" <> help "Query a predefined endpoint."))))
              <*> (optional (option (eitherReader readAccountId)
                            (long "add-to" <> metavar "walletId@accountId"
                                           <> help "Append to an existing wallet & account."
              )))


readAccountId :: String -> Either String AccountId
readAccountId str' = case decodeCType (CAccountId (toS str')) of
  Left e  -> Left (toS e)
  Right x -> Right x

