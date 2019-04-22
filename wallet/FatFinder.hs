{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Main (main) where

import           Control.Lens (makeLensesWith)
import           Control.Monad.Reader ()
import           Formatting (fprint, shown, (%), int, build)
import           Pos.Chain.Genesis (GenesisHash, configGenesisHash)
import           Pos.Chain.Txp (txpTxs, txInputs, txOutputs, Tx)
import           Pos.DB (MonadDBRead (dbGet, dbGetSerBlock, dbGetSerBlund, dbGetSerUndo, dbIterSource),
                     NodeDBs, closeNodeDBs, dbGetDefault, dbIterSourceDefault,
                     openNodeDBs, getBlock)
import           Pos.DB.Block (dbGetSerBlockRealDefault, getFirstGenesisBlockHash, resolveForwardLink,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault)
import           Pos.Util (HasLens (lensOf), postfixLFields)
import           System.Environment (lookupEnv)
import           Universum
import Pos.Launcher
import           Options.Applicative (execParser, info, fullDesc)
import Pos.Client.CLI.Options
import Pos.Chain.Block
import           Pos.Util.Trace (Trace, fromTypeclassWlog, Severity(Info))
import           Data.Functor.Contravariant (contramap)
import           Pos.Util.Wlog.Compatibility (setupTestLogging)
import           Pos.Chain.Block (HeaderHash)
import qualified Data.List.NonEmpty as NE
import Pos.DB.Block

data KeyToyContext = KeyToyContext { _ktNodeDBs :: NodeDBs }
makeLensesWith postfixLFields ''KeyToyContext

instance HasLens NodeDBs KeyToyContext NodeDBs where
    lensOf = _ktNodeDBs_L

main :: IO ()
main = do
  setupTestLogging

  let
    logTrace :: Trace IO (Severity, Text)
    logTrace = fromTypeclassWlog
    traceInfo :: Trace IO Text
    traceInfo = contramap ((,) Info) logTrace

  opts <- execParser $ info configurationOptionsParser fullDesc
  maybeDbPath <- lookupEnv "DB_PATH"
  print maybeDbPath
  case maybeDbPath of
    Nothing -> pure ()
    Just dbPath -> do
      bracket (openNodeDBs False dbPath) closeNodeDBs $ \db -> do
        withConfigurations traceInfo Nothing Nothing False opts $ \genesisConfig _walletConfig _txpConfig _ntpConfig -> do
          let genesisHash = configGenesisHash genesisConfig
          runReaderT (iterateOverAllBlocks genesisHash) (KeyToyContext db)

type KeyToy = ReaderT KeyToyContext IO

instance MonadDBRead KeyToy where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault
    dbGetSerBlund = dbGetSerBlundRealDefault

iterateOverAllBlocks :: GenesisHash -> KeyToy ()
iterateOverAllBlocks genesisHash = do
  let
    go :: HeaderHash -> Int -> KeyToy ()
    go hh fatTxs = do
      blk <- getBlock genesisHash hh
      newCount <- case blk of
        Just (Right blk') -> do
          let
            txs = blk' ^. mainBlockTxPayload . txpTxs
            go2 :: Int -> Tx -> KeyToy Int
            go2 count tx = do
              let
                ins = length $ NE.toList (tx ^. txInputs)
                outs = length $ NE.toList (tx ^. txOutputs)
                condition = ins > 300
                --condition = (ins > 1) && (outs == 1)
              meos <- getHeaderEpochOrSlot hh
              if condition then do
                liftIO $ fprint (build % " headerhash: " % shown % " ins: " % int % " outs: " % int % "\n") meos hh ins outs
                pure (count + 1)
              else
                pure count
          foldlM go2 fatTxs txs
        _ -> pure fatTxs
      nextHh <- resolveForwardLink hh
      case nextHh of
        Nothing -> do
          print fatTxs
          print ("done"::String)
        Just nextHh' -> go nextHh' newCount
  genhash <- getFirstGenesisBlockHash genesisHash
  go genhash 0
  pure ()
