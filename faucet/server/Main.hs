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
import           Network.Wai.Handler.Warp (run)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Servant
import           System.Environment (getArgs)
import           System.Remote.Monitoring (forkServer, serverMetricStore)
import           System.Remote.Monitoring.Statsd (forkStatsd)
import           System.Wlog (launchFromFile, usingLoggerName)

import           Cardano.Faucet
import           Cardano.Faucet.Swagger

main :: IO ()
main = withCompileInfo $ do
    ekg <- forkServer "localhost" 8001
    args <- getArgs
    config <- case args of
      [ "--config", cfgFile ] -> do
        ecfg <- eitherDecode <$> BSL.readFile cfgFile
        either (error . ("Error decoding: " ++)) return ecfg
      _ -> error "Need a --config argument pointing to a json file"
    runLogger config $ do
      fEnv <- initEnv config (serverMetricStore ekg)
      let server = hoistServer faucetDocAPI (nat fEnv) faucetHandler
      _statsd <- liftIO $ forkStatsd (config ^. fcStatsdOpts . _Wrapped') (fEnv ^. feStore)
      liftIO $ run (config ^. fcPort) (serve faucetDocAPI server)
  where
    -- runLogger :: LoggerNameBox IO a -> IO a
    runLogger fc = launchFromFile (fc ^. fcLoggerConfigFile) "faucet"
    nat :: FaucetEnv -> M a -> Handler a
    nat e = Handler . ExceptT . usingLoggerName "server" . runM e
