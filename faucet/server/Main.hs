{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}


module Main where

import           Control.Lens (_Wrapped')
import           Control.Monad.Except
import           Data.Aeson (eitherDecode)
import           Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative (Parser, execParser, fullDesc, header,
                     help, helper, info, long, progDesc, short, strOption,
                     (<**>))

import           Servant hiding (header)
import           System.Remote.Monitoring (forkServer, serverMetricStore)
import           System.Remote.Monitoring.Statsd (forkStatsd)
import           System.Wlog (LoggerNameBox, launchFromFile, usingLoggerName)
import           Universum

import           Pos.Infra.Statistics.Ekg
import           Pos.Util.CompileInfo (withCompileInfo)

import           Cardano.Faucet

configOption :: Parser FilePath
configOption = strOption (
               long "config"
            <> short 'c'
            <> help "Path to the config file"
            )

options :: Parser (FilePath, EkgParams)
options = (,) <$> configOption <*> ekgParamsOption

main :: IO ()
main = withCompileInfo $ do
    (cfgFile, (EkgParams eHost ePort)) <- execParser opts
    ekg <- forkServer eHost ePort
    config <- do
        ecfg <- eitherDecode <$> BSL.readFile cfgFile
        either (error . Text.pack . ("Error decoding: " <>)) return ecfg
    runLogger config $ do
      fEnv <- initEnv config (serverMetricStore ekg)
      let server = hoistServer faucetAppAPI (nat fEnv) (faucetHandler (config ^. fcHomePage))
      _statsd <- liftIO $ forkStatsd (config ^. fcStatsdOpts . _Wrapped') (fEnv ^. feStore)
      liftIO $ run (config ^. fcPort) (serve faucetAppAPI server)
  where
    opts = info (options <**> helper) (fullDesc <> progDesc "Run the faucet server" <> header "cardano-faucet - A component for requesting ADA")
    runLogger :: FaucetConfig -> LoggerNameBox IO a -> IO a
    runLogger fc = launchFromFile (fc ^. fcLoggerConfigFile) "faucet"
    nat :: FaucetEnv -> M a -> Handler a
    nat e = Handler . ExceptT . usingLoggerName "server" . runM e
