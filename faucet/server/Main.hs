{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson (eitherDecode)
import           Data.ByteString.Lazy as BSL
import           Data.Monoid ((<>))
import           Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Prometheus (prometheus, PrometheusSettings(..))
-- import Prometheus (register)
-- import Prometheus.Metric.GHC (ghcMetrics)
import           Options.Applicative (Parser, execParser, fullDesc, header,
                     help, helper, info, long, progDesc, short, strOption,
                     (<**>))

import           Servant hiding (header)
import           System.Remote.Monitoring (forkServer, serverMetricStore)
import           System.Remote.Monitoring.Statsd (forkStatsd)
import           System.Wlog (LoggerNameBox, launchFromFile, usingLoggerName)
import qualified Data.Map.Strict as Map

import           Pos.Infra.Statistics.Ekg
import           Pos.Util.CompileInfo (withCompileInfo)

import           System.Remote.Monitoring.Prometheus
import           Cardano.Faucet
import           Cardano.Faucet.Swagger

configOption :: Parser FilePath
configOption = strOption (
               long "config"
            <> short 'c'
            <> help "Path to the config file"
            )

options :: Parser (FilePath, EkgParams)
options = (,) <$> configOption <*> ekgParamsOption

helpMap :: HelpMap
helpMap = Map.fromList
  [
    ("num-withdrawals", ("Number of withdrawals observed", Nothing))
  , ("wallet-balance", ("Balance of faucet wallet", Nothing))
  ]

main :: IO ()
main = withCompileInfo $ do
    (cfgFile, (EkgParams eHost ePort)) <- execParser opts
    ekg <- forkServer eHost ePort
    config <- do
        ecfg <- eitherDecode <$> BSL.readFile cfgFile
        either (error . ("Error decoding: " ++)) return ecfg
    runLogger config $ do
      fEnv <- initEnv config (serverMetricStore ekg)
      registerEKGStore (fEnv ^. feStore) (metricInfo .~ helpMap $ defaultOptions)
      let server = hoistServer faucetDocAPI (nat fEnv) faucetHandler
          promMiddleware = prometheus $ PrometheusSettings ["metrics"] True True
      _statsd <- liftIO $ forkStatsd (config ^. fcStatsdOpts . _Wrapped') (fEnv ^. feStore)
      liftIO $ run (config ^. fcPort) $ promMiddleware $ serve faucetDocAPI server
  where
    opts = info (options <**> helper) (fullDesc <> progDesc "Run the faucet server" <> header "cardano-faucet - A component for requesting ADA")
    runLogger :: FaucetConfig -> LoggerNameBox IO a -> IO a
    runLogger fc = launchFromFile (fc ^. fcLoggerConfigFile) "faucet"
    nat :: FaucetEnv -> M a -> Handler a
    nat e = Handler . ExceptT . usingLoggerName "server" . runM e
