{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
       ( main
       ) where

import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.Trans.Either (EitherT (..))
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import qualified Data.ByteString            as BS
import           Data.ByteString.Base58     (bitcoinAlphabet, encodeBase58)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  ((!!))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as S (fromList, toList)
import           Data.String.QQ             (s)
import qualified Data.Text                  as T
import           Data.Time.Units            (convertUnit)
import           Formatting                 (build, int, sformat, stext, (%))
import           Mockable                   (Production, delay, runProduction)
import           Network.Transport.Abstract (Transport, hoistTransport)
import           System.IO                  (hFlush, stdout)
import           System.Wlog                (logDebug, logError, logInfo, logWarning)
#if !(defined(mingw32_HOST_OS))
import           System.Exit                (ExitCode (ExitSuccess))
import           System.Posix.Process       (exitImmediately)
#endif
import           Serokell.Util              (ms, sec)
import           Universum

import           Pos.Binary                 (Raw, encode)
import qualified Pos.CLI                    as CLI
import           Pos.Communication          (NodeId, OutSpecs, SendActions, Worker',
                                             WorkerSpec, dataFlow, delegationRelays,
                                             relayPropagateOut, submitTx,
                                             submitUpdateProposal, submitVote, txRelays,
                                             usRelays, worker)
import           Pos.Constants              (genesisBlockVersionData, isDevelopment)
import           Pos.Crypto                 (Hash, SecretKey, SignTag (SignUSVote),
                                             emptyPassphrase, encToPublic, fakeSigner,
                                             hash, hashHexF, noPassEncrypt,
                                             safeCreateProxySecretKey, safeSign, toPublic,
                                             unsafeHash, withSafeSigner)
import           Pos.Data.Attributes        (mkAttributes)
import           Pos.Discovery              (findPeers, getPeers)
import           Pos.Genesis                (genesisDevSecretKeys,
                                             genesisStakeDistribution, genesisUtxo)
import           Pos.Launcher               (BaseParams (..), LoggingParams (..),
                                             bracketTransport, loggerBracket, stakesDistr)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Ssc.SscAlgo            (SscAlgo (..))
import           Pos.Txp                    (TxOut (..), TxOutAux (..), txaF)
import           Pos.Types                  (coinF, makePubKeyAddress)
import           Pos.Update                 (BlockVersionData (..),
                                             BlockVersionModifier (..), UpdateData (..),
                                             UpdateVote (..), mkUpdateProposalWSign)
import           Pos.Util.UserSecret        (readUserSecret, usKeys)
import           Pos.Util.Util              (powerLift)
import           Pos.Wallet                 (MonadWallet, addSecretKey, getBalance,
                                             getSecretKeys)
import           Pos.Wallet.Light           (LightWalletMode, WalletParams (..),
                                             runWalletStaticPeers)
import           Pos.WorkMode               (RealMode, RealModeContext)

import           Command                    (Command (..), parseCommand)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..))
import           WalletOptions              (WalletAction (..), WalletOptions (..),
                                             getWalletOptions)

data CmdCtx =
  CmdCtx
    { skeys :: [SecretKey]
    , na    :: [NodeId]
    }


helpMsg :: Text
helpMsg = [s|
Avaliable commands:
   balance <address>              -- check balance on given address (may be any address)
   send <N> [<address> <coins>]+  -- create and send transaction with given outputs
                                     from own address #N
   send-to-all-genesis <coins> <delay>
                                  -- create and send transactions from all genesis addresses,
                                     delay in ms to themselves with the given amount of coins
   vote <N> <decision> <upid>     -- send vote with given hash of proposal id (in base16) and
                                     decision, from own address #N
   propose-update <N> <block ver> <script ver> <slot duration> <max block size> <software ver> <propose_file>?
                                  -- propose an update with given versions and other data
                                     with one positive vote for it, from own address #N
   listaddr                       -- list own addresses
   delegate-light <N> <M> <eStart> <eEnd>?
                                  -- delegate secret key #N to pk <M> light version (M is encoded in base58),
                                     where eStart is cert start epoch, eEnd -- expire epoch
   delegate-heavy <N> <M> <e>     -- delegate secret key #N to pk <M> heavyweight (M is encoded in base58),
                                     e is current epoch.
   add-key-pool <N>               -- add key from intial pool
   add-key <file>                 -- add key from file
   help                           -- show this message
   quit                           -- shutdown node wallet
|]

runCmd :: MonadWallet ssc ctx m => SendActions m -> Command -> CmdCtx -> m ()
runCmd _ (Balance addr) _ =
    getBalance addr >>=
    putText . sformat ("Current balance: "%coinF)
runCmd sendActions (Send idx outputs) CmdCtx{na} = do
    skeys <- getSecretKeys
    etx <-
        withSafeSigner (skeys !! idx) (pure emptyPassphrase) $ \mss ->
        runEitherT $ do
            ss <- mss `whenNothing` throwError "Invalid passphrase"
            EitherT $
                submitTx
                    sendActions
                    ss
                    na
                    (map (flip TxOutAux []) outputs)
    case etx of
        Left err -> putText $ sformat ("Error: "%stext) err
        Right tx -> putText $ sformat ("Submitted transaction: "%txaF) tx
runCmd sendActions (SendToAllGenesis amount delay_) CmdCtx{..} = do
    for_ skeys $ \key -> do
        let txOut = TxOut {
            txOutAddress = makePubKeyAddress (toPublic key),
            txOutValue = amount
        }
        etx <-
            submitTx
                sendActions
                (fakeSigner key)
                na
                (NE.fromList [TxOutAux txOut []])
        case etx of
            Left err -> putText $ sformat ("Error: "%stext) err
            Right tx -> putText $ sformat ("Submitted transaction: "%txaF) tx
        delay $ ms delay_
runCmd sendActions v@(Vote idx decision upid) CmdCtx{na} = do
    logDebug $ "Submitting a vote :" <> show v
    skey <- (!! idx) <$> getSecretKeys
    msignature <- withSafeSigner skey (pure emptyPassphrase) $ mapM $
                        \ss -> pure $ safeSign SignUSVote ss (upid, decision)
    case msignature of
        Nothing -> putText "Invalid passphrase"
        Just signature -> do
            let voteUpd = UpdateVote
                    { uvKey        = encToPublic skey
                    , uvProposalId = upid
                    , uvDecision   = decision
                    , uvSignature  = signature
                }
            if null na
                then putText "Error: no addresses specified"
                else do
                    submitVote sendActions na voteUpd
                    putText "Submitted vote"
runCmd sendActions ProposeUpdate{..} CmdCtx{na} = do
    logDebug "Proposing update..."
    skey <- (!! puIdx) <$> getSecretKeys
    (diffFile :: Maybe (Hash Raw)) <-
        runMaybeT $ do
            filePath <- MaybeT $ pure puFilePath
            fileData <- liftIO $ BS.readFile filePath
            let h = unsafeHash fileData
            liftIO $
                putText $
                sformat ("Read file succesfuly, its hash: " %hashHexF) h
            pure h
    let BlockVersionData {..} = genesisBlockVersionData
    let bvm =
            BlockVersionModifier
            { bvmScriptVersion     = puScriptVersion
            , bvmSlotDuration      = convertUnit (sec puSlotDurationSec)
            , bvmMaxBlockSize      = puMaxBlockSize
            , bvmMaxHeaderSize     = bvdMaxHeaderSize
            , bvmMaxTxSize         = bvdMaxTxSize
            , bvmMaxProposalSize   = bvdMaxProposalSize
            , bvmMpcThd            = bvdMpcThd
            , bvmHeavyDelThd       = bvdHeavyDelThd
            , bvmUpdateVoteThd     = bvdUpdateVoteThd
            , bvmUpdateProposalThd = bvdUpdateProposalThd
            , bvmUpdateImplicit    = bvdUpdateImplicit
            , bvmUpdateSoftforkThd = bvdUpdateSoftforkThd
            , bvmTxFeePolicy       = Nothing
            }
    let udata' h = HM.fromList [(puSystemTag, UpdateData h h h h)]
    let udata = maybe (error "Failed to read prop file") udata' diffFile
    let whenCantCreate = error . mappend "Failed to create update proposal: "
    withSafeSigner skey (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
            let updateProposal = either whenCantCreate identity $
                    mkUpdateProposalWSign
                        puBlockVersion
                        bvm
                        puSoftwareVersion
                        udata
                        (mkAttributes ())
                        ss
            if null na
                then putText "Error: no addresses specified"
                else do
                    submitUpdateProposal sendActions ss na updateProposal
                    let id = hash updateProposal
                    putText $
                      sformat ("Update proposal submitted, upId: "%hashHexF) id
runCmd _ Help _ = putText helpMsg
runCmd _ ListAddresses _ = do
   addrs <- map encToPublic <$> getSecretKeys
   putText "Available addresses:"
   for_ (zip [0 :: Int ..] addrs) $ \(i, pk) ->
       putText $ sformat ("    #"%int%":   "%build%" (PK: "%stext%")")
                    i (makePubKeyAddress pk) (toBase58Text pk)
  where
    toBase58Text = decodeUtf8 . encodeBase58 bitcoinAlphabet . encode
runCmd sendActions (DelegateLight i delegatePk startEpoch lastEpochM) CmdCtx{na} = do
   issuerSk <- (!! i) <$> getSecretKeys
   withSafeSigner issuerSk (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
          let psk = safeCreateProxySecretKey ss delegatePk (startEpoch, fromMaybe 1000 lastEpochM)
          for_ na $ \nodeId ->
             dataFlow "pskLight" sendActions nodeId psk
   putText "Sent lightweight cert"
runCmd sendActions (DelegateHeavy i delegatePk curEpoch) CmdCtx{na} = do
   issuerSk <- (!! i) <$> getSecretKeys
   withSafeSigner issuerSk (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
          let psk = safeCreateProxySecretKey ss delegatePk curEpoch
          for_ na $ \nodeId ->
             dataFlow "pskHeavy" sendActions nodeId psk
   putText "Sent heavyweight cert"
runCmd _ (AddKeyFromPool i) CmdCtx{..} = do
   let key = skeys !! i
   addSecretKey $ noPassEncrypt key
runCmd _ (AddKeyFromFile f) _ = do
    secret <- readUserSecret f
    mapM_ addSecretKey $ secret ^. usKeys
runCmd _ Quit _ = pure ()

-- This solution is hacky, but will work for now
runCmdOuts :: OutSpecs
runCmdOuts = relayPropagateOut $ mconcat
                [ usRelays @(RealModeContext SscGodTossing) @(RealMode SscGodTossing)
                , delegationRelays @SscGodTossing @(RealModeContext SscGodTossing) @(RealMode SscGodTossing)
                , txRelays @SscGodTossing @(RealModeContext SscGodTossing) @(RealMode SscGodTossing)
                ]

evalCmd :: MonadWallet ssc ctx m => SendActions m -> Command -> CmdCtx -> m ()
evalCmd _ Quit _      = pure ()
evalCmd sa cmd cmdCtx = runCmd sa cmd cmdCtx >> evalCommands sa cmdCtx

evalCommands :: MonadWallet ssc ctx m => SendActions m -> CmdCtx -> m ()
evalCommands sa cmdCtx = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err   -> putStrLn err >> evalCommands sa cmdCtx
        Right cmd_ -> evalCmd sa cmd_ cmdCtx

initialize :: MonadWallet ssc ctx m => WalletOptions -> m [NodeId]
initialize WalletOptions{..} = do
    peers <- S.toList <$> getPeers
    bool (pure peers) getPeersUntilSome (null peers)
  where
    -- FIXME this is dangerous. If rmFindPeers doesn't block, for instance
    -- because it's a constant empty set of peers, we'll spin forever.
    getPeersUntilSome = do
        liftIO $ hFlush stdout
        logWarning "Discovering peers, because current peer list is empty"
        peers <- S.toList <$> findPeers
        if null peers
        then getPeersUntilSome
        else pure peers

runWalletRepl :: MonadWallet ssc ctx m => WalletOptions -> Worker' m
runWalletRepl wo sa = do
    na <- initialize wo
    putText "Welcome to Wallet CLI Node"
    let keysPool = if isDevelopment then genesisDevSecretKeys else []
    evalCmd sa Help (CmdCtx keysPool na)

runWalletCmd :: MonadWallet ssc ctx m => WalletOptions -> Text -> Worker' m
runWalletCmd wo str sa = do
    na <- initialize wo
    let strs = T.splitOn "," str
    let keysPool = if isDevelopment then genesisDevSecretKeys else []
    let cmdCtx = CmdCtx keysPool na
    for_ strs $ \scmd -> do
        let mcmd = parseCommand scmd
        case mcmd of
            Left err   -> putStrLn err
            Right cmd' -> runCmd sa cmd' cmdCtx
    putText "Command execution finished"
    putText " " -- for exit by SIGPIPE
    liftIO $ hFlush stdout
#if !(defined(mingw32_HOST_OS))
    delay $ sec 3
    liftIO $ exitImmediately ExitSuccess
#endif

main :: IO ()
main = do
    opts@WalletOptions {..} <- getWalletOptions
    --filePeers <- maybe (return []) CLI.readPeersFile
    --                   (CLI.dhtPeersFile woCommonArgs)
    let allPeers = woPeers -- ++ filePeers
    let logParams =
            LoggingParams
            { lpRunnerTag     = "smart-wallet"
            , lpHandlerPrefix = CLI.logPrefix woCommonArgs
            , lpConfigPath    = CLI.logConfig woCommonArgs
            }
        baseParams = BaseParams { bpLoggingParams = logParams }

    let sysStart = CLI.sysStart woCommonArgs
    let params =
            WalletParams
            { wpDbPath      = Just woDbPath
            , wpRebuildDb   = woRebuildDb
            , wpKeyFilePath = woKeyFilePath
            , wpSystemStart = sysStart
            , wpGenesisKeys = woDebug
            , wpBaseParams  = baseParams
            , wpGenesisUtxo =
                  genesisUtxo $
                  if isDevelopment
                      then stakesDistr
                               (CLI.flatDistr woCommonArgs)
                               (CLI.bitcoinDistr woCommonArgs)
                               (CLI.richPoorDistr woCommonArgs)
                               (CLI.expDistr woCommonArgs)
                      else genesisStakeDistribution
            }

    loggerBracket logParams $ runProduction $
      bracketTransport TCP.Unaddressable $ \transport -> do
        let transport' :: Transport LightWalletMode
            transport' = hoistTransport
                (powerLift :: forall t . Production t -> LightWalletMode t)
                transport

            plugins :: ([ WorkerSpec LightWalletMode ], OutSpecs)
            plugins = first pure $ case woAction of
                Repl    -> worker runCmdOuts $ runWalletRepl opts
                Cmd cmd -> worker runCmdOuts $ runWalletCmd opts cmd
                Serve __webPort __webDaedalusDbPath -> error "light wallet server is disabled"
                -- Serve webPort webDaedalusDbPath -> worker walletServerOuts $ \sendActions ->
                --     walletServeWebLite sendActions webDaedalusDbPath False webPort

        case CLI.sscAlgo woCommonArgs of
            GodTossingAlgo -> do
                logInfo "Using MPC coin tossing"
                liftIO $ hFlush stdout
                runWalletStaticPeers transport' (S.fromList allPeers) params plugins
            NistBeaconAlgo ->
                logError "Wallet does not support NIST beacon!"
