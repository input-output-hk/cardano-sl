{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

-- | This module provides a function to run Auxx's command.

module Command.Run
       ( runCmd
       ) where

import           Universum

import           Control.Exception.Safe     (throwString)
import           Data.ByteString.Base58     (bitcoinAlphabet, encodeBase58)
import           Data.List                  ((!!))
import           Formatting                 (build, int, sformat, stext, (%))
import           NeatInterpolation          (text)
import qualified Text.JSON.Canonical        as CanonicalJSON

import           Pos.Auxx                   (makePubKeyAddressAuxx)
import           Pos.Binary                 (serialize')
import           Pos.Communication          (MsgType (..), Origin (..), SendActions,
                                             dataFlow, immediateConcurrentConversations)
import           Pos.Constants              (isDevelopment)
import           Pos.Core                   (addressHash, coinF)
import           Pos.Core.Address           (makeAddress)
import           Pos.Core.Configuration     (genesisSecretKeys)
import           Pos.Core.Types             (AddrAttributes (..), AddrSpendingData (..))
import           Pos.Crypto                 (emptyPassphrase, encToPublic,
                                             fullPublicKeyHexF, hashHexF, noPassEncrypt,
                                             safeCreatePsk, withSafeSigner)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util.UserSecret        (readUserSecret, usKeys, usPrimKey)
import           Pos.Wallet                 (addSecretKey, getSecretKeysPlain)

import qualified Command.Rollback           as Rollback
import qualified Command.Tx                 as Tx
import           Command.Types              (Command (..))
import qualified Command.Update             as Update
import           Mode                       (AuxxMode, CmdCtx (..), getCmdCtx, getBalance)


helpMsg :: Text
helpMsg = [text|
Avaliable commands:
   balance <address>              -- check balance on given address (may be any address)
   send <N> [<address> <coins>]+  -- create and send transaction with given outputs
                                     from own address #N
   send-to-all-genesis <duration> <conc> <delay> <sendmode> <csvfile>
                                  -- create and send transactions from all genesis addresses for <duration>
                                     seconds, delay in ms.  conc is the number of threads that send
                                     transactions concurrently. sendmode can be one of "neighbours",
                                     "round-robin", and "send-random".
   vote <N> <decision> <upid>     -- send vote with given hash of proposal id (in base16) and
                                     decision, from own address #N
   propose-update <N> [vote-all] <block ver> <max tx size> <max proposal size> <software ver> <propose_file>?
                                  -- propose an update with given versions and other data
                                     with one positive vote for it, from own address #N
                                     if vote-all flag is set then votes from all secret keys also will be sent
   listaddr                       -- list own addresses
   delegate-light <N> <M> <eStart> <eEnd>?
                                  -- delegate secret key #N to pk <M> light version (M is encoded in base58),
                                     where eStart is cert start epoch, eEnd -- expire epoch
   delegate-heavy <N> <M> <e>     -- delegate secret key #N to pk <M> heavyweight (M is encoded in base58),
                                     e is current epoch.
   add-key-pool <N>               -- add key from intial pool
   add-key <file> [primary]       -- add key from file, if primary flag is set then add only primary key

   addr-distr <N> boot
   addr-distr <N> [<M>:<coinPortion>]+
                                  -- print the address for pk <N> (encoded in base58) with the specified distribution,
                                  -- where <M> is stakeholder id (pk hash), and the coin portion can be a coefficient
                                  -- in [0..1] or a percentage (ex. 42%)

   rollback <N> <file>            -- Rollback <N> blocks (genesis or main doesn't matter) and dump transactions from
                                  -- them to <file> in binary format.

   send-from-file <file>          -- Read transactions in binary format from <file> and submit them to the network.
                                  -- <file> should be in format produced by 'rollback' command.

   help                           -- show this message
   quit                           -- shutdown node wallet
|]

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

runCmd ::
       HasConfigurations
    => SendActions AuxxMode
    -> Command
    -> AuxxMode ()
runCmd _ (Balance addr) =
    getBalance addr >>=
    putText . sformat ("Current balance: "%coinF)
runCmd sendActions (Send idx outputs) = Tx.send sendActions idx outputs
runCmd sendActions (SendToAllGenesis stagp) =
    Tx.sendToAllGenesis sendActions stagp
runCmd sendActions (Vote idx decision upId) =
    Update.vote sendActions idx decision upId
runCmd sendActions (ProposeUpdate params) =
    Update.propose sendActions params
runCmd _ (HashInstaller path) = Update.hashInstaller path
runCmd _ Help = putText helpMsg
runCmd _ ListAddresses = do
   addrs <- map encToPublic <$> getSecretKeysPlain
   putText "Available addresses:"
   for_ (zip [0 :: Int ..] addrs) $ \(i, pk) -> do
       addr <- makePubKeyAddressAuxx pk
       putText $ sformat ("    #"%int%":   addr:      "%build%"\n"%
                          "          pk base58: "%stext%"\n"%
                          "          pk hex:    "%fullPublicKeyHexF%"\n"%
                          "          pk hash:   "%hashHexF)
                    i addr (toBase58Text pk) pk (addressHash pk)
  where
    toBase58Text = decodeUtf8 . encodeBase58 bitcoinAlphabet . serialize'
runCmd sendActions (DelegateLight i delegatePk startEpoch lastEpochM dry) = do
    CmdCtx{ccPeers} <- getCmdCtx
    issuerSk <- (!! i) <$> getSecretKeysPlain
    withSafeSigner issuerSk (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
            let psk = safeCreatePsk ss delegatePk (startEpoch, fromMaybe 1000 lastEpochM)
            if dry
            then do
                putText "Delegation light dry run not implemented"
            else do
                dataFlow
                    "pskLight"
                    (immediateConcurrentConversations sendActions ccPeers)
                    (MsgTransaction OriginSender) psk
                putText "Sent lightweight cert"
runCmd sendActions (DelegateHeavy i delegatePk curEpoch dry) = do
    CmdCtx {ccPeers} <- getCmdCtx
    issuerSk <- (!! i) <$> getSecretKeysPlain
    withSafeSigner issuerSk (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
            let psk = safeCreatePsk ss delegatePk curEpoch
            if dry
            then do
                putText $ sformat ("JSON: key "%hashHexF%", value "%stext)
                          (addressHash $ encToPublic issuerSk)
                          (decodeUtf8 $
                                CanonicalJSON.renderCanonicalJSON $
                                runIdentity $
                                CanonicalJSON.toJSON psk)
            else do
               dataFlow
                   "pskHeavy"
                   (immediateConcurrentConversations sendActions ccPeers)
                   (MsgTransaction OriginSender)
                   psk
               putText "Sent heavyweight cert"
runCmd _ (AddKeyFromPool i) = do
    unless isDevelopment $
        throwString "AddKeyFromPool should be used only in dev mode"
    CmdCtx {..} <- getCmdCtx
    let secrets = fromMaybe (error "Secret keys are unknown") genesisSecretKeys
    let key = secrets !! i
    addSecretKey $ noPassEncrypt key
runCmd _ (AddKeyFromFile f primary) = do
    secret <- readUserSecret f
    if primary then do
        let primSk = fromMaybe (error "Primary key not found") (secret ^. usPrimKey)
        addSecretKey $ noPassEncrypt primSk
    else
        mapM_ addSecretKey $ secret ^. usKeys
runCmd _ (AddrDistr pk asd) = do
    putText $ pretty addr
  where
    addr = makeAddress (PubKeyASD pk) (AddrAttributes Nothing asd)
runCmd _ (Rollback rollbackNum rollbackDumpPath) =
    Rollback.rollbackAndDump rollbackNum rollbackDumpPath
runCmd sendActions (SendTxsFromFile filePath) =
    Tx.sendTxsFromFile sendActions filePath
runCmd _ Quit = pure ()
