{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
{-# LANGUAGE RankNTypes     #-}

module Main
       ( main
       ) where

import           Universum

import qualified Data.ByteString            as BS
import           Data.ByteString.Base58     (bitcoinAlphabet, encodeBase58)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  ((!!))
import qualified Data.Set                   as S (fromList)
import qualified Data.Text                  as T
import           NeatInterpolation          (text)
#if !(defined(mingw32_HOST_OS))
import           System.Exit                (ExitCode (ExitSuccess))
import           System.Posix.Process       (exitImmediately)
#endif
import           Data.Time.Units            (convertUnit)
import           Formatting                 (build, int, sformat, shown, stext, string,
                                             (%))
import           Mockable                   (Production, delay, runProduction)
import           Network.Transport.Abstract (Transport, hoistTransport)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..))
import           Node.Conversation          (ConversationActions (..))
import           Node.Message.Class         (Message (..))
import           Serokell.Util              (sec)
import           System.IO                  (hFlush, stdout)
import           System.Wlog                (logDebug, logInfo)
import           System.Wlog.CanLog         (WithLogger)

import           Pos.Binary                 (Raw, serialize')
import qualified Pos.Client.CLI             as CLI
import           Pos.Communication          (Conversation (..), MsgType (..), Origin (..),
                                             OutSpecs (..), SendActions (..), Worker,
                                             WorkerSpec, dataFlow, delegationRelays,
                                             immediateConcurrentConversations,
                                             relayPropagateOut, submitUpdateProposal,
                                             submitVote, txRelays, usRelays, worker)
import           Pos.Constants              (genesisBlockVersionData, isDevelopment)
import           Pos.Core                   (addressHash, coinF)
import           Pos.Core.Address           (makeAddress)
import           Pos.Core.Context           (HasCoreConstants, giveStaticConsts)
import           Pos.Core.Types             (AddrAttributes (..), AddrSpendingData (..))
import           Pos.Crypto                 (Hash, SignTag (SignUSVote), emptyPassphrase,
                                             encToPublic, fullPublicKeyHexF, hash,
                                             hashHexF, noPassEncrypt, safeCreatePsk,
                                             safeSign, unsafeHash, withSafeSigner)
import           Pos.Data.Attributes        (mkAttributes)
import           Pos.Genesis                (devBalancesDistr, devGenesisContext,
                                             genesisContextProduction,
                                             genesisDevSecretKeys, gtcUtxo)
import           Pos.Launcher               (BaseParams (..), LoggingParams (..),
                                             bracketTransport, loggerBracket)
import           Pos.Rubbish                (LightWalletMode, WalletParams (..),
                                             makePubKeyAddressRubbish,
                                             runWalletStaticPeers)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Txp                    (unGenesisUtxo)
import           Pos.Update                 (BlockVersionData (..),
                                             BlockVersionModifier (..), SystemTag,
                                             UpdateData (..), UpdateVote (..),
                                             mkUpdateProposalWSign)
import           Pos.Util.UserSecret        (readUserSecret, usKeys)
import           Pos.Util.Util              (powerLift)
import           Pos.Wallet                 (addSecretKey, getBalance, getSecretKeys)
import           Pos.WorkMode               (RealMode, RealModeContext)

import           Command                    (CmdCtx (..), Command (..),
                                             ProposeUpdateSystem (..), parseCommand)
import           RubbishOptions             (RubbishAction (..), RubbishOptions (..),
                                             getRubbishOptions)
import qualified Tx


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

   addr-distr <N> boot
   addr-distr <N> [<M>:<coinPortion>]+
                                  -- print the address for pk <N> (encoded in base58) with the specified distribution,
                                  -- where <M> is stakeholder id (pk hash), and the coin portion can be a coefficient
                                  -- in [0..1] or a percentage (ex. 42%)

   help                           -- show this message
   quit                           -- shutdown node wallet
|]

runCmd :: HasCoreConstants => SendActions LightWalletMode -> Command -> CmdCtx -> LightWalletMode ()
runCmd _ (Balance addr) _ =
    getBalance addr >>=
    putText . sformat ("Current balance: "%coinF)
runCmd sendActions (Send idx outputs) ctx = Tx.send sendActions idx outputs ctx
runCmd sendActions (SendToAllGenesis stagp) ctx =
    Tx.sendToAllGenesis sendActions stagp ctx
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
                    submitVote (immediateConcurrentConversations sendActions na) voteUpd
                    putText "Submitted vote"
runCmd sendActions ProposeUpdate{..} CmdCtx{na} = do
    logDebug "Proposing update..."
    skey <- (!! puIdx) <$> getSecretKeys
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
            , bvmSoftforkRule      = Nothing
            , bvmTxFeePolicy       = Nothing
            , bvmUnlockStakeEpoch  = Nothing
            }
    updateData <- mapM updateDataElement puUpdates
    let udata = HM.fromList updateData
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
                    submitUpdateProposal (immediateConcurrentConversations sendActions na) ss updateProposal
                    let id = hash updateProposal
                    putText $
                      sformat ("Update proposal submitted, upId: "%hashHexF) id
runCmd _ Help _ = putText helpMsg
runCmd _ ListAddresses _ = do
   addrs <- map encToPublic <$> getSecretKeys
    -- Light wallet doesn't know current slot, so let's assume
    -- it's 0-th epoch. It's enough for our current needs.
   putText "Available addresses:"
   for_ (zip [0 :: Int ..] addrs) $ \(i, pk) -> do
       addr <- makePubKeyAddressRubbish pk
       putText $ sformat ("    #"%int%":   addr:      "%build%"\n"%
                          "          pk base58: "%stext%"\n"%
                          "          pk hex:    "%fullPublicKeyHexF%"\n"%
                          "          pk hash:   "%hashHexF)
                    i addr (toBase58Text pk) pk (addressHash pk)
  where
    toBase58Text = decodeUtf8 . encodeBase58 bitcoinAlphabet . serialize'
runCmd sendActions (DelegateLight i delegatePk startEpoch lastEpochM) CmdCtx{na} = do
   issuerSk <- (!! i) <$> getSecretKeys
   withSafeSigner issuerSk (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
          let psk = safeCreatePsk ss delegatePk (startEpoch, fromMaybe 1000 lastEpochM)
          dataFlow "pskLight" (immediateConcurrentConversations sendActions na) (MsgTransaction OriginSender) psk
   putText "Sent lightweight cert"
runCmd sendActions (DelegateHeavy i delegatePk curEpoch) CmdCtx{na} = do
   issuerSk <- (!! i) <$> getSecretKeys
   withSafeSigner issuerSk (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
          let psk = safeCreatePsk ss delegatePk curEpoch
          dataFlow "pskHeavy" (immediateConcurrentConversations sendActions na) (MsgTransaction OriginSender) psk
   putText "Sent heavyweight cert"
runCmd _ (AddKeyFromPool i) CmdCtx{..} = do
   let key = skeys !! i
   addSecretKey $ noPassEncrypt key
runCmd _ (AddKeyFromFile f) _ = do
    secret <- readUserSecret f
    mapM_ addSecretKey $ secret ^. usKeys
runCmd _ (AddrDistr pk asd) _ = do
    putText $ pretty addr
  where
    addr = makeAddress (PubKeyASD pk) (AddrAttributes Nothing asd)
runCmd _ Quit _ = pure ()

dummyHash :: Hash Raw
dummyHash = unsafeHash (0 :: Integer)

hashFile :: MonadIO m => Maybe FilePath -> m (Hash Raw)
hashFile Nothing  = pure dummyHash
hashFile (Just filename) = do
    fileData <- liftIO $ BS.readFile filename
    let h = unsafeHash fileData
    putText $ sformat ("Read file "%string%" succesfuly, its hash: "%hashHexF) filename h
    pure h

updateDataElement :: MonadIO m => ProposeUpdateSystem -> m (SystemTag, UpdateData)
updateDataElement ProposeUpdateSystem{..} = do
    diffHash <- hashFile pusBinDiffPath
    installerHash <- hashFile pusInstallerPath
    pure (pusSystemTag, UpdateData diffHash installerHash dummyHash dummyHash)

-- This solution is hacky, but will work for now
runCmdOuts :: HasCoreConstants => OutSpecs
runCmdOuts = relayPropagateOut $ mconcat
                [ usRelays @(RealModeContext SscGodTossing) @(RealMode SscGodTossing)
                , delegationRelays @SscGodTossing @(RealModeContext SscGodTossing) @(RealMode SscGodTossing)
                , txRelays @SscGodTossing @(RealModeContext SscGodTossing) @(RealMode SscGodTossing)
                ]

evalCmd :: HasCoreConstants => SendActions LightWalletMode -> Command -> CmdCtx -> LightWalletMode ()
evalCmd _ Quit _      = pure ()
evalCmd sa cmd cmdCtx = runCmd sa cmd cmdCtx >> evalCommands sa cmdCtx

evalCommands :: HasCoreConstants => SendActions LightWalletMode -> CmdCtx -> LightWalletMode ()
evalCommands sa cmdCtx = do
    putStr @Text "> "
    liftIO $ hFlush stdout
    line <- getLine
    let cmd = parseCommand line
    case cmd of
        Left err   -> putStrLn err >> evalCommands sa cmdCtx
        Right cmd_ -> evalCmd sa cmd_ cmdCtx

runWalletRepl :: HasCoreConstants => CmdCtx -> Worker LightWalletMode
runWalletRepl cmdCtx sa = do
    putText "Welcome to Wallet CLI Node"
    evalCmd sa Help cmdCtx

runWalletCmd :: HasCoreConstants => CmdCtx -> Text -> Worker LightWalletMode
runWalletCmd cmdCtx str sa = do
    let strs = T.splitOn "," str
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
main = giveStaticConsts $ do
    RubbishOptions {..} <- getRubbishOptions
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

    print logParams

    let sysStart = CLI.sysStart woCommonArgs
    let devBalanceDistr =
            devBalancesDistr
                (CLI.flatDistr woCommonArgs)
                (CLI.richPoorDistr woCommonArgs)
                (CLI.expDistr woCommonArgs)
    let wpGenesisContext
            | isDevelopment = devGenesisContext devBalanceDistr
            | otherwise = genesisContextProduction
    let params =
            WalletParams
            { wpDbPath      = Just woDbPath
            , wpRebuildDb   = woRebuildDb
            , wpKeyFilePath = woKeyFilePath
            , wpSystemStart = sysStart
            , wpGenesisKeys = woDebug
            , wpBaseParams  = baseParams
            , ..
            }

    loggerBracket logParams $ runProduction $
      bracketTransport TCP.Unaddressable $ \transport -> do
        logInfo $ if isDevelopment
            then "Development Mode"
            else "Production Mode"
        logInfo $ sformat ("Length of genesis utxo: "%shown)
            (length $ unGenesisUtxo $ wpGenesisContext ^. gtcUtxo)
        let transport' :: Transport LightWalletMode
            transport' = hoistTransport
                (powerLift :: forall t . Production t -> LightWalletMode t)
                transport

            worker' specs w = worker specs $ \sa -> w (addLogging sa)

            cmdCtx = CmdCtx
                      { skeys = if isDevelopment then genesisDevSecretKeys else []
                      , na = woPeers
                      , genesisBalanceDistr = devBalanceDistr
                      }

            plugins :: HasCoreConstants => ([WorkerSpec LightWalletMode], OutSpecs)
            plugins = first pure $ case woAction of
                Repl    -> worker' runCmdOuts $ runWalletRepl cmdCtx
                Cmd cmd -> worker' runCmdOuts $ runWalletCmd cmdCtx cmd

        logInfo "Using MPC coin tossing"
        liftIO $ hFlush stdout
        runWalletStaticPeers woNodeDbPath transport' (S.fromList allPeers) params plugins

addLogging :: forall m. WithLogger m => SendActions m -> SendActions m
addLogging SendActions{..} = SendActions{
      enqueueMsg = error "unused"
    , withConnectionTo = aux
    }
  where
    aux nid k = withConnectionTo nid $ \peerData -> fmap auxConv (k peerData)
    auxConv (Conversation k) = Conversation (\acts -> k (auxActs acts))

    auxActs :: (Message snd, Message rcv)
            => ConversationActions snd rcv m -> ConversationActions snd rcv m
    auxActs (ConversationActions{..}) = ConversationActions {
        send = \body -> do
                 logDebug $ sformat ("Light wallet sending " % stext) (formatMessage body)
                 send body
      , recv = \limit -> do
                 mRcv <- recv limit
                 logDebug $
                   case mRcv of
                     Nothing  -> sformat ("Light wallet received end of input")
                     Just rcv -> sformat ("Light wallet received " % stext) (formatMessage rcv)
                 return mRcv
      }
