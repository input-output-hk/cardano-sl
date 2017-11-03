{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Command.Proc
       ( createCommandProcs
       ) where

import           Universum

import           Data.List                  ((!!))
import qualified Data.Map                   as Map
import           Formatting                 (build, int, sformat, stext, (%))
import           System.Wlog                (logError, logInfo)
import qualified Text.JSON.Canonical        as CanonicalJSON

import           Pos.Client.KeyStorage      (addSecretKey, getSecretKeysPlain)
import           Pos.Client.Txp.Balances    (getBalance)
import           Pos.Communication          (MsgType (..), Origin (..), SendActions,
                                             dataFlow, immediateConcurrentConversations)
import           Pos.Core                   (AddrStakeDistribution (..), Address,
                                             StakeholderId, addressHash, mkMultiKeyDistr,
                                             unsafeGetCoin)
import           Pos.Core.Address           (makeAddress)
import           Pos.Core.Configuration     (HasConfiguration, genesisSecretKeys)
import           Pos.Core.Types             (AddrAttributes (..), AddrSpendingData (..))
import           Pos.Crypto                 (PublicKey, emptyPassphrase, encToPublic,
                                             fullPublicKeyF, hashHexF, noPassEncrypt,
                                             safeCreatePsk, unsafeCheatingHashCoerce,
                                             withSafeSigner)
import           Pos.DB.Class               (MonadGState (..))
import           Pos.Infra.Configuration    (HasInfraConfiguration)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Txp                    (TxOut (..))
import           Pos.Update                 (BlockVersionModifier (..))
import           Pos.Util.CompileInfo       (HasCompileInfo)
import           Pos.Util.UserSecret        (WalletUserSecret (..), readUserSecret,
                                             usKeys, usPrimKey, usWallet, userSecret)
import           Pos.Util.Util              (eitherToFail)

import           Command.BlockGen           (generateBlocks)
import           Command.Help               (mkHelpMessage)
import qualified Command.Rollback           as Rollback
import qualified Command.Tx                 as Tx
import           Command.TyProjection       (tyAddrDistrPart, tyAddrStakeDistr, tyAddress,
                                             tyBlockVersion, tyBlockVersionModifier,
                                             tyBool, tyByte, tyCoin, tyCoinPortion,
                                             tyEither, tyEpochIndex, tyFilePath, tyHash,
                                             tyInt, tyProposeUpdateSystem, tyPublicKey,
                                             tyScriptVersion, tySecond, tySendMode,
                                             tySoftwareVersion, tyStakeholderId,
                                             tySystemTag, tyTxOut, tyValue, tyWord,
                                             tyWord32)
import qualified Command.Update             as Update
import           Lang.Argument              (getArg, getArgMany, getArgOpt, getArgSome)
import           Lang.Command               (CommandProc (..))
import           Lang.Name                  (Name)
import           Lang.Value                 (AddKeyParams (..), AddrDistrPart (..),
                                             GenBlocksParams (..),
                                             ProposeUpdateParams (..),
                                             ProposeUpdateSystem (..),
                                             RollbackParams (..), SendMode (..),
                                             Value (..))
import           Mode                       (AuxxMode, CmdCtx (..), deriveHDAddressAuxx,
                                             getCmdCtx, makePubKeyAddressAuxx)
import           Repl                       (PrintAction)

createCommandProcs ::
       (HasConfigurations, HasCompileInfo)
    => PrintAction AuxxMode
    -> SendActions AuxxMode
    -> [CommandProc AuxxMode]
createCommandProcs printAction sendActions = fix $ \commands -> [

    CommandProc
    { cpName = "L"
    , cpArgumentConsumer = getArgMany tyValue "elem"
    , cpExec = return . ValueList
    , cpHelp = "construct a list"
    },

    CommandProc
    { cpName = "pk"
    , cpArgumentConsumer = getArg tyInt "i"
    , cpExec = fmap ValuePublicKey . toLeft . Right
    , cpHelp = "public key for secret #i"
    },

    CommandProc
    { cpName = "s"
    , cpArgumentConsumer = getArg (tyPublicKey `tyEither` tyInt) "pk"
    , cpExec = fmap ValueStakeholderId . toLeft . Right
    , cpHelp = "stakeholder id (hash) of the specified public key"
    },

    CommandProc
    { cpName = "addr"
    , cpArgumentConsumer =
        (,) <$> getArg (tyPublicKey `tyEither` tyInt) "pk"
            <*> getArgOpt tyAddrStakeDistr "distr"
    , cpExec = \(pk', mDistr) -> do
        pk <- toLeft pk'
        addr <- case mDistr of
            Nothing -> makePubKeyAddressAuxx pk
            Just distr -> return $
                makeAddress (PubKeyASD pk) (AddrAttributes Nothing distr)
        return $ ValueAddress addr
    , cpHelp = "address for the specified public key. a stake distribution \
               \ can be specified manually (by default it uses the current epoch \
               \ to determine whether we want to use bootstrap distr)"
    },

    CommandProc
    { cpName = "addr-hd"
    , cpArgumentConsumer = getArg tyInt "i"
    , cpExec = \i -> do
        sks <- getSecretKeysPlain
        sk <- evaluateWHNF (sks !! i) -- WHNF is sufficient to force possible errors
                                      -- from using (!!). I'd use NF but there's no
                                      -- NFData instance for secret keys.
        addrHD <- deriveHDAddressAuxx sk
        return $ ValueAddress addrHD
    , cpHelp = "address of the HD wallet for the specified public key"
    },

    CommandProc
    { cpName = "tx-out"
    , cpArgumentConsumer = do
        txOutAddress <- getArg tyAddress "addr"
        txOutValue <- getArg tyCoin "value"
        return TxOut{..}
    , cpExec = return . ValueTxOut
    , cpHelp = "construct a transaction output"
    },

    CommandProc
    { cpName = "dp"
    , cpArgumentConsumer = do
        adpStakeholderId <- getArg (tyStakeholderId `tyEither` tyPublicKey `tyEither` tyInt) "s"
        adpCoinPortion <- getArg tyCoinPortion "p"
        return (adpStakeholderId, adpCoinPortion)
    , cpExec = \(sId', cp) -> do
        sId <- toLeft sId'
        return $ ValueAddrDistrPart (AddrDistrPart sId cp)
    , cpHelp = "construct an address distribution part"
    },

    CommandProc
    { cpName = "distr"
    , cpArgumentConsumer = getArgSome tyAddrDistrPart "dp"
    , cpExec = \parts -> do
        distr <- case parts of
            AddrDistrPart sId coinPortion :| []
              | coinPortion == maxBound ->
                return $ SingleKeyDistr sId
            _ -> eitherToFail $
                 mkMultiKeyDistr . Map.fromList $
                 map (\(AddrDistrPart s cp) -> (s, cp)) $
                 toList parts
        return $ ValueAddrStakeDistribution distr
    , cpHelp = "construct an address distribution (use 'dp' for each part)"
    },

    procConst "neighbours" $ ValueSendMode SendNeighbours,
    procConst "round-robin" $ ValueSendMode SendRoundRobin,
    procConst "send-random" $ ValueSendMode SendRandom,

    procConst "boot" $ ValueAddrStakeDistribution BootstrapEraDistr,

    procConst "true" $ ValueBool True,
    procConst "false" $ ValueBool False,

    CommandProc
    { cpName = "balance"
    , cpArgumentConsumer = getArg (tyAddress `tyEither` tyPublicKey `tyEither` tyInt) "addr"
    , cpExec = \addr' -> do
        addr <- toLeft addr'
        balance <- getBalance addr
        return $ ValueNumber (fromIntegral . unsafeGetCoin $ balance)
    , cpHelp = "check the amount of coins on the specified address"
    },

    CommandProc
    { cpName = "bvd"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> ValueBlockVersionData <$> gsAdoptedBVData
    , cpHelp = "return current (adopted) BlockVersionData"
    },

    CommandProc
    { cpName = "send-to-all-genesis"
    , cpArgumentConsumer = do
        stagpDuration <- getArg tyInt "dur"
        stagpConc <- getArg tyInt "conc"
        stagpDelay <- getArg tyInt "delay"
        stagpMode <- getArg tySendMode "mode"
        stagpTpsSentFile <- getArg tyFilePath "file"
        return Tx.SendToAllGenesisParams{..}
    , cpExec = \stagp -> do
        Tx.sendToAllGenesis sendActions stagp
        return ValueUnit
    , cpHelp = "create and send transactions from all genesis addresses \
               \ for <duration> seconds, <delay> in ms. <conc> is the \
               \ number of threads that send transactions concurrently. \
               \ <mode> is either 'neighbours', 'round-robin', or 'send-random'"
    },

    CommandProc
    { cpName = "send-from-file"
    , cpArgumentConsumer = getArg tyFilePath "file"
    , cpExec = \filePath -> do
        Tx.sendTxsFromFile sendActions filePath
        return ValueUnit
    , cpHelp = ""
    },

    CommandProc
    { cpName = "send"
    , cpArgumentConsumer =
        (,) <$> getArg tyInt "i"
            <*> getArgSome tyTxOut "out"
    , cpExec = \(i, outputs) -> do
        Tx.send sendActions i outputs
        return ValueUnit
    , cpHelp = "send from #i to specified transaction outputs \
               \ (use 'tx-out' to build them)"
    },

    CommandProc
    { cpName = "vote"
    , cpArgumentConsumer =
        (,,) <$> getArg tyInt "i"
             <*> getArg tyBool "agree"
             <*> getArg tyHash "up-id"
    , cpExec = \(i, decision, upId) -> do
        Update.vote sendActions i decision upId
        return ValueUnit
    , cpHelp = "send vote for update proposal <up-id> and \
               \ decision <agree> ('true' or 'false'), \
               \ using secret key #i"
    },

    CommandProc
    { cpName = "bvm"
    , cpArgumentConsumer = do
        bvmScriptVersion <- getArgOpt tyScriptVersion "script-version"
        bvmSlotDuration <- getArgOpt tySecond "slot-duration"
        bvmMaxBlockSize <- getArgOpt tyByte "max-block-size"
        bvmMaxHeaderSize <- getArgOpt tyByte "max-header-size"
        bvmMaxTxSize <- getArgOpt tyByte "max-tx-size"
        bvmMaxProposalSize <- getArgOpt tyByte "max-proposal-size"
        bvmMpcThd <- getArgOpt tyCoinPortion "mpc-thd"
        bvmHeavyDelThd <- getArgOpt tyCoinPortion "heavy-del-thd"
        bvmUpdateVoteThd <- getArgOpt tyCoinPortion "update-vote-thd"
        bvmUpdateProposalThd <- getArgOpt tyCoinPortion "update-proposal-thd"
        let bvmUpdateImplicit = Nothing -- TODO
        let bvmSoftforkRule = Nothing -- TODO
        let bvmTxFeePolicy = Nothing -- TODO
        bvmUnlockStakeEpoch <- getArgOpt tyEpochIndex "unlock-stake-epoch"
        pure BlockVersionModifier{..}
    , cpExec = return . ValueBlockVersionModifier
    , cpHelp = "construct a BlockVersionModifier"
    },

    CommandProc
    { cpName = "upd-bin"
    , cpArgumentConsumer = do
        pusSystemTag <- getArg tySystemTag "system"
        pusInstallerPath <- getArgOpt tyFilePath "installer-path"
        pusBinDiffPath <- getArgOpt tyFilePath "bin-diff-path"
        pure ProposeUpdateSystem{..}
    , cpExec = return . ValueProposeUpdateSystem
    , cpHelp = "construct a part of the update proposal for binary update"
    },

    CommandProc
    { cpName = "propose-update"
    , cpArgumentConsumer = do
        puSecretKeyIdx <- getArg tyInt "i"
        puVoteAll <- getArg tyBool "vote-all"
        puBlockVersion <- getArg tyBlockVersion "block-version"
        puSoftwareVersion <- getArg tySoftwareVersion "software-version"
        puBlockVersionModifier <- getArg tyBlockVersionModifier "bvm"
        puUpdates <- getArgMany tyProposeUpdateSystem "update"
        pure ProposeUpdateParams{..}
    , cpExec = \params ->
        -- FIXME: confuses existential/universal. A better solution
        -- is to have two ValueHash constructors, one with universal and
        -- one with existential (relevant via singleton-style GADT) quantification.
        ValueHash . unsafeCheatingHashCoerce <$> Update.propose sendActions params
    , cpHelp = "propose an update with one positive vote for it \
               \ using secret key #i"
    },

    CommandProc
    { cpName = "hash-installer"
    , cpArgumentConsumer = getArg tyFilePath "file"
    , cpExec = \filePath -> do
        Update.hashInstaller filePath
        return ValueUnit
    , cpHelp = ""
    },

    CommandProc
    { cpName = "delegate-light"
    , cpArgumentConsumer =
      (,,,) <$> getArg tyInt "i"
            <*> getArg tyPublicKey "pk"
            <*> getArg tyEpochIndex "start"
            <*> getArgOpt tyEpochIndex "end"
    , cpExec = \(i, delegatePk, startEpoch, lastEpochM) -> do
        CmdCtx{ccPeers} <- getCmdCtx
        issuerSk <- (!! i) <$> getSecretKeysPlain
        withSafeSigner issuerSk (pure emptyPassphrase) $ \case
            Nothing -> logError "Invalid passphrase"
            Just ss -> do
                let psk = safeCreatePsk ss delegatePk (startEpoch, fromMaybe 1000 lastEpochM)
                dataFlow
                    "pskLight"
                    (immediateConcurrentConversations sendActions ccPeers)
                    (MsgTransaction OriginSender) psk
                logInfo "Sent lightweight cert"
        return ValueUnit
    , cpHelp = ""
    },

    CommandProc
    { cpName = "delegate-heavy"
    , cpArgumentConsumer =
      (,,,) <$> getArg tyInt "i"
            <*> getArg tyPublicKey "pk"
            <*> getArg tyEpochIndex "cur"
            <*> getArg tyBool "dry"
    , cpExec = \(i, delegatePk, curEpoch, dry) -> do
        CmdCtx {ccPeers} <- getCmdCtx
        issuerSk <- (!! i) <$> getSecretKeysPlain
        withSafeSigner issuerSk (pure emptyPassphrase) $ \case
            Nothing -> logError "Invalid passphrase"
            Just ss -> do
                let psk = safeCreatePsk ss delegatePk curEpoch
                if dry
                then do
                    printAction $
                        sformat ("JSON: key "%hashHexF%", value "%stext)
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
                    logInfo "Sent heavyweight cert"
        return ValueUnit
    , cpHelp = ""
    },

    CommandProc
    { cpName = "generate-blocks"
    , cpArgumentConsumer = do
        bgoBlockN <- getArg tyWord32 "n"
        bgoSeed <- getArgOpt tyInt "seed"
        return GenBlocksParams{..}
    , cpExec = \params -> do
        generateBlocks params
        return ValueUnit
    , cpHelp = "generate <n> blocks"
    },

    CommandProc
    { cpName = "add-key-pool"
    , cpArgumentConsumer = getArg tyInt "i"
    , cpExec = \i -> do
        CmdCtx {..} <- getCmdCtx
        let secrets = fromMaybe (error "Secret keys are unknown") genesisSecretKeys
            key = secrets !! i
        evaluateNF_ key
        addSecretKey $ noPassEncrypt key
        return ValueUnit
    , cpHelp = ""
    },

    CommandProc
    { cpName = "add-key"
    , cpArgumentConsumer = do
        akpFile <- getArg tyFilePath "file"
        akpPrimary <- getArg tyBool "primary"
        return AddKeyParams {..}
    , cpExec = \AddKeyParams {..} -> do
        secret <- readUserSecret akpFile
        if akpPrimary then do
            let primSk = fromMaybe (error "Primary key not found") (secret ^. usPrimKey)
            addSecretKey $ noPassEncrypt primSk
        else
            mapM_ addSecretKey $ secret ^. usKeys
        return ValueUnit
    , cpHelp = ""
    },

    CommandProc
    { cpName = "rollback"
    , cpArgumentConsumer = do
        rpNum <- getArg tyWord "n"
        rpDumpPath <- getArg tyFilePath "dump-file"
        pure RollbackParams{..}
    , cpExec = \RollbackParams{..} -> do
        Rollback.rollbackAndDump rpNum rpDumpPath
        return ValueUnit
    , cpHelp = ""
    },

    CommandProc
    { cpName = "listaddr"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> do
        sks <- getSecretKeysPlain
        printAction "Available addresses:"
        for_ (zip [0 :: Int ..] sks) $ \(i, sk) -> do
            let pk = encToPublic sk
            addr <- makePubKeyAddressAuxx pk
            addrHD <- deriveHDAddressAuxx sk
            printAction $
                sformat ("    #"%int%":   addr:      "%build%"\n"%
                         "          pk:        "%fullPublicKeyF%"\n"%
                         "          pk hash:   "%hashHexF%"\n"%
                         "          HD addr:   "%build)
                    i addr pk (addressHash pk) addrHD
        walletMB <- (^. usWallet) <$> (view userSecret >>= atomically . readTVar)
        whenJust walletMB $ \wallet -> do
            addrHD <- deriveHDAddressAuxx (_wusRootKey wallet)
            printAction $
                sformat ("    Wallet address:\n"%
                         "          HD addr:   "%build)
                    addrHD
        return ValueUnit
    , cpHelp = ""
    },

    CommandProc
    { cpName = "help"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> do
        printAction (mkHelpMessage commands)
        return ValueUnit
    , cpHelp = "display this message"
    }]

procConst :: Applicative m => Name -> Value -> CommandProc m
procConst name value =
    CommandProc
    { cpName = name
    , cpArgumentConsumer = pure ()
    , cpExec = \() -> pure value
    , cpHelp = "constant"
    }

class ToLeft a b where
    toLeft :: Either a b -> AuxxMode a

instance (ToLeft a b, ToLeft b c) => ToLeft a (Either b c) where
    toLeft = toLeft <=< traverse toLeft

instance ToLeft PublicKey Int where
    toLeft = either return getPublicKeyFromIndex

instance ToLeft StakeholderId PublicKey where
    toLeft = return . either identity addressHash

instance (HasConfiguration, HasInfraConfiguration) => ToLeft Address PublicKey where
    toLeft = either return makePubKeyAddressAuxx

getPublicKeyFromIndex :: Int -> AuxxMode PublicKey
getPublicKeyFromIndex i = do
    sks <- getSecretKeysPlain
    let sk = sks !! i
        pk = encToPublic sk
    evaluateNF pk
