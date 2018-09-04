{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Command.Proc
       ( createCommandProcs
       ) where

import           Universum

import           Data.Constraint (Dict (..))
import           Data.Default (def)
import           Data.List ((!!))
import qualified Data.Map as Map
import           Formatting (build, int, sformat, stext, (%))
import           System.Wlog (CanLog, HasLoggerName, logError, logInfo, logWarning)
import qualified Text.JSON.Canonical as CanonicalJSON

import           Pos.Client.KeyStorage (addSecretKey, getSecretKeysPlain)
import           Pos.Client.Txp.Balances (getBalance)
import           Pos.Core (AddrStakeDistribution (..), HeavyDlgIndex (..), SoftwareVersion (..),
                           StakeholderId, addressHash, mkMultiKeyDistr, unsafeGetCoin)
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..), makeAddress)
import           Pos.Core.Configuration (genesisSecretKeys)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Core.Txp (TxOut (..))
import           Pos.Crypto (ProtocolMagic, PublicKey, emptyPassphrase, encToPublic, fullPublicKeyF,
                             hashHexF, noPassEncrypt, safeCreatePsk, unsafeCheatingHashCoerce,
                             withSafeSigner)
import           Pos.DB.Class (MonadGState (..))
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Update (BlockVersionModifier (..))
import           Pos.Util.UserSecret (WalletUserSecret (..), readUserSecret, usKeys, usPrimKey,
                                      usWallet, userSecret)
import           Pos.Util.Util (eitherToThrow)

import           Command.BlockGen (generateBlocks)
import           Command.Help (mkHelpMessage)
import qualified Command.Rollback as Rollback
import qualified Command.Tx as Tx
import           Command.TyProjection (tyAddrDistrPart, tyAddrStakeDistr, tyAddress,
                                       tyApplicationName, tyBlockVersion, tyBlockVersionModifier,
                                       tyBool, tyByte, tyCoin, tyCoinPortion, tyEither,
                                       tyEpochIndex, tyFilePath, tyHash, tyInt,
                                       tyProposeUpdateSystem, tyPublicKey, tyScriptVersion,
                                       tySecond, tySoftwareVersion, tyStakeholderId, tySystemTag,
                                       tyTxOut, tyValue, tyWord, tyWord32)
import qualified Command.Update as Update
import           Lang.Argument (getArg, getArgMany, getArgOpt, getArgSome, typeDirectedKwAnn)
import           Lang.Command (CommandProc (..), UnavailableCommand (..))
import           Lang.Name (Name)
import           Lang.Value (AddKeyParams (..), AddrDistrPart (..), GenBlocksParams (..),
                             ProposeUpdateParams (..), ProposeUpdateSystem (..),
                             RollbackParams (..), Value (..))
import           Mode (MonadAuxxMode, deriveHDAddressAuxx, makePubKeyAddressAuxx)
import           Repl (PrintAction)

createCommandProcs ::
       forall m. (MonadIO m, CanLog m, HasLoggerName m)
    => Maybe ProtocolMagic
    -> Maybe (Dict (MonadAuxxMode m))
    -> PrintAction m
    -> Maybe (Diffusion m)
    -> [CommandProc m]
createCommandProcs mpm hasAuxxMode printAction mDiffusion = rights . fix $ \commands -> [

    return CommandProc
    { cpName = "L"
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = getArgMany tyValue "elem"
    , cpExec = return . ValueList
    , cpHelp = "construct a list"
    },

    let name = "pk" in
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = getArg tyInt "i"
    , cpExec = fmap ValuePublicKey . toLeft . Right
    , cpHelp = "public key for secret #i"
    },

    let name = "s" in
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = getArg (tyPublicKey `tyEither` tyInt) "pk"
    , cpExec = fmap ValueStakeholderId . toLeft . Right
    , cpHelp = "stakeholder id (hash) of the specified public key"
    },

    let name = "addr" in
    needsAuxxMode name >>= \Dict ->
    needsProtocolMagic name >>= \pm ->
    let nm = makeNetworkMagic pm in
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = map
        $ typeDirectedKwAnn "distr" tyAddrStakeDistr
    , cpArgumentConsumer =
        (,) <$> getArg (tyPublicKey `tyEither` tyInt) "pk"
            <*> getArgOpt tyAddrStakeDistr "distr"
    , cpExec = \(pk', mDistr) -> do
        pk <- toLeft pk'
        addr <- case mDistr of
            Nothing -> makePubKeyAddressAuxx nm pk
            Just distr -> return $
                makeAddress (PubKeyASD pk) (AddrAttributes Nothing distr nm)
        return $ ValueAddress addr
    , cpHelp = "address for the specified public key. a stake distribution \
             \ can be specified manually (by default it uses the current epoch \
             \ to determine whether we want to use bootstrap distr)"
    },

    let name = "addr-hd" in
    needsAuxxMode name >>= \Dict ->
    needsProtocolMagic name >>= \pm ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = getArg tyInt "i"
    , cpExec = \i -> do
        sks <- getSecretKeysPlain
        sk <- evaluateWHNF (sks !! i) -- WHNF is sufficient to force possible errors
                                      -- from using (!!). I'd use NF but there's no
                                      -- NFData instance for secret keys.
        let nm = makeNetworkMagic pm
        addrHD <- deriveHDAddressAuxx nm sk
        return $ ValueAddress addrHD
    , cpHelp = "address of the HD wallet for the specified public key"
    },

    return CommandProc
    { cpName = "tx-out"
    , cpArgumentPrepare = map
        $ typeDirectedKwAnn "addr" tyAddress
    , cpArgumentConsumer = do
        txOutAddress <- getArg tyAddress "addr"
        txOutValue <- getArg tyCoin "value"
        return TxOut{..}
    , cpExec = return . ValueTxOut
    , cpHelp = "construct a transaction output"
    },

    let name = "dp" in
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = do
        adpStakeholderId <- getArg (tyStakeholderId `tyEither` tyPublicKey `tyEither` tyInt) "s"
        adpCoinPortion <- getArg tyCoinPortion "p"
        return (adpStakeholderId, adpCoinPortion)
    , cpExec = \(sId', cp) -> do
        sId <- toLeft sId'
        return $ ValueAddrDistrPart (AddrDistrPart sId cp)
    , cpHelp = "construct an address distribution part"
    },

    let name = "distr" in
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = getArgSome tyAddrDistrPart "dp"
    , cpExec = \parts -> do
        distr <- case parts of
            AddrDistrPart sId coinPortion :| []
              | coinPortion == maxBound ->
                return $ SingleKeyDistr sId
            _ -> eitherToThrow $
                 mkMultiKeyDistr . Map.fromList $
                 map (\(AddrDistrPart s cp) -> (s, cp)) $
                 toList parts
        return $ ValueAddrStakeDistribution distr
    , cpHelp = "construct an address distribution (use 'dp' for each part)"
    },

    return . procConst "boot" $ ValueAddrStakeDistribution BootstrapEraDistr,

    return . procConst "true" $ ValueBool True,
    return . procConst "false" $ ValueBool False,

    let name = "balance" in
    needsAuxxMode name >>= \Dict ->
    needsProtocolMagic name >>= \pm ->
    let nm = makeNetworkMagic pm in
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = getArg (tyAddress `tyEither` tyPublicKey `tyEither` tyInt) "addr"
    , cpExec = \addr' -> do
        addr <- case addr' of
            Left a    -> pure a
            Right pki -> makePubKeyAddressAuxx nm =<< toLeft pki
        balance <- getBalance addr
        return $ ValueNumber (fromIntegral . unsafeGetCoin $ balance)
    , cpHelp = "check the amount of coins on the specified address"
    },

    let name = "bvd" in
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> ValueBlockVersionData <$> gsAdoptedBVData
    , cpHelp = "return current (adopted) BlockVersionData"
    },

    let name = "send-to-all-genesis" in
    needsProtocolMagic name >>= \pm ->
    needsDiffusion name >>= \diffusion ->
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = do
        stagpGenesisTxsPerThread <- getArg tyInt "genesisTxsPerThread"
        stagpTxsPerThread <- getArg tyInt "txsPerThread"
        stagpConc <- getArg tyInt "conc"
        stagpDelay <- getArg tyInt "delay"
        stagpTpsSentFile <- getArg tyFilePath "file"
        return Tx.SendToAllGenesisParams{..}
    , cpExec = \stagp -> do
        Tx.sendToAllGenesis pm diffusion stagp
        return ValueUnit
    , cpHelp = "create and send transactions from all genesis addresses \
               \ for <duration> seconds, <delay> in ms. <conc> is the \
               \ number of threads that send transactions concurrently."
    },

    let name = "send-from-file" in
    needsDiffusion name >>= \diffusion ->
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = getArg tyFilePath "file"
    , cpExec = \filePath -> do
        Tx.sendTxsFromFile diffusion filePath
        return ValueUnit
    , cpHelp = ""
    },

    let name = "send" in
    needsProtocolMagic name >>= \pm ->
    needsDiffusion name >>= \diffusion ->
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer =
        (,) <$> getArg tyInt "i"
            <*> getArgSome tyTxOut "out"
    , cpExec = \(i, outputs) -> do
        Tx.send pm diffusion i outputs
        return ValueUnit
    , cpHelp = "send from #i to specified transaction outputs \
               \ (use 'tx-out' to build them)"
    },

    let name = "vote" in
    needsProtocolMagic name >>= \pm ->
    needsDiffusion name >>= \diffusion ->
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer =
        (,,) <$> getArg tyInt "i"
             <*> getArg tyBool "agree"
             <*> getArg tyHash "up-id"
    , cpExec = \(i, decision, upId) -> do
        Update.vote pm diffusion i decision upId
        return ValueUnit
    , cpHelp = "send vote for update proposal <up-id> and \
               \ decision <agree> ('true' or 'false'), \
               \ using secret key #i"
    },

    return CommandProc
    { cpName = "bvm"
    , cpArgumentPrepare = identity
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

    return CommandProc
    { cpName = "upd-bin"
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = do
        pusSystemTag <- getArg tySystemTag "system"
        pusInstallerPath <- getArgOpt tyFilePath "installer-path"
        pusBinDiffPath <- getArgOpt tyFilePath "bin-diff-path"
        pure ProposeUpdateSystem{..}
    , cpExec = return . ValueProposeUpdateSystem
    , cpHelp = "construct a part of the update proposal for binary update"
    },

    let name = "software" in
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = do
        appName <- getArg tyApplicationName "name"
        number <- getArg tyWord32 "n"
        pure (appName, number)
    , cpExec = \(svAppName, svNumber) -> do
        return $ ValueSoftwareVersion SoftwareVersion{..}
    , cpHelp = "Construct a software version from application name and number"
    },

    let name = "propose-update" in
    needsProtocolMagic name >>= \pm ->
    needsDiffusion name >>= \diffusion ->
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = map
        $ typeDirectedKwAnn "bvm" tyBlockVersionModifier
        . typeDirectedKwAnn "update" tyProposeUpdateSystem
        . typeDirectedKwAnn "block-version" tyBlockVersion
        . typeDirectedKwAnn "software-version" tySoftwareVersion
    , cpArgumentConsumer = do
        puSecretKeyIdx <- getArg tyInt "i"
        puVoteAll <- getArg tyBool "vote-all"
        puBlockVersion <- getArg tyBlockVersion "block-version"
        puSoftwareVersion <- getArg tySoftwareVersion "software-version"
        puBlockVersionModifier <- fromMaybe def <$> getArgOpt tyBlockVersionModifier "bvm"
        puUpdates <- getArgMany tyProposeUpdateSystem "update"
        pure ProposeUpdateParams{..}
    , cpExec = \params ->
        -- FIXME: confuses existential/universal. A better solution
        -- is to have two ValueHash constructors, one with universal and
        -- one with existential (relevant via singleton-style GADT) quantification.
        ValueHash . unsafeCheatingHashCoerce <$> Update.propose pm diffusion params
    , cpHelp = "propose an update with one positive vote for it \
               \ using secret key #i"
    },

    return CommandProc
    { cpName = "hash-installer"
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = getArg tyFilePath "file"
    , cpExec = \filePath -> do
        Update.hashInstaller printAction filePath
        return ValueUnit
    , cpHelp = ""
    },

    let name = "delegate-heavy" in
    needsProtocolMagic name >>= \pm ->
    needsDiffusion name >>= \diffusion ->
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer =
      (,,,) <$> getArg tyInt "i"
            <*> getArg tyPublicKey "pk"
            <*> getArg tyEpochIndex "cur"
            <*> getArg tyBool "dry"
    , cpExec = \(i, delegatePk, curEpoch, dry) -> do
        issuerSk <- (!! i) <$> getSecretKeysPlain
        withSafeSigner issuerSk (pure emptyPassphrase) $ \case
            Nothing -> logError "Invalid passphrase"
            Just ss -> do
                let psk = safeCreatePsk pm ss delegatePk (HeavyDlgIndex curEpoch)
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
                    sendPskHeavy diffusion psk
                    logInfo "Sent heavyweight cert"
        return ValueUnit
    , cpHelp = ""
    },

    let name = "generate-blocks" in
    needsProtocolMagic name >>= \pm ->
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = do
        bgoBlockN <- getArg tyWord32 "n"
        bgoSeed <- getArgOpt tyInt "seed"
        return GenBlocksParams{..}
    , cpExec = \params -> do
        generateBlocks pm params
        return ValueUnit
    , cpHelp = "generate <n> blocks"
    },

    let name = "add-key-pool" in
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = getArgMany tyInt "i"
    , cpExec = \is -> do
        when (null is) $ logWarning "Not adding keys from pool (list is empty)"
        let secrets = fromMaybe (error "Secret keys are unknown") genesisSecretKeys
        forM_ is $ \i -> do
            key <- evaluateNF $ secrets !! i
            addSecretKey $ noPassEncrypt key
        return ValueUnit
    , cpHelp = ""
    },

    let name = "add-key" in
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = do
        akpFile <- getArg tyFilePath "file"
        akpPrimary <- getArg tyBool "primary"
        return AddKeyParams {..}
    , cpExec = \AddKeyParams {..} -> do
        secret <- readUserSecret akpFile
        if akpPrimary then do
            let primSk = fromMaybe (error "Primary key not found") (secret ^. usPrimKey)
            addSecretKey $ noPassEncrypt primSk
        else do
            let ks = secret ^. usKeys
            printAction $ sformat ("Adding "%build%" secret keys") (length ks)
            mapM_ addSecretKey ks
        return ValueUnit
    , cpHelp = ""
    },

    let name = "rollback" in
    needsProtocolMagic name >>= \pm ->
    needsAuxxMode name >>= \Dict ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = do
        rpNum <- getArg tyWord "n"
        rpDumpPath <- getArg tyFilePath "dump-file"
        pure RollbackParams{..}
    , cpExec = \RollbackParams{..} -> do
        Rollback.rollbackAndDump pm rpNum rpDumpPath
        return ValueUnit
    , cpHelp = ""
    },

    let name = "listaddr" in
    needsAuxxMode name >>= \Dict ->
    needsProtocolMagic name >>= \pm ->
    let nm = makeNetworkMagic pm in
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> do
        sks <- getSecretKeysPlain
        printAction "Available addresses:"
        for_ (zip [0 :: Int ..] sks) $ \(i, sk) -> do
            let pk = encToPublic sk
            addr <- makePubKeyAddressAuxx nm pk
            addrHD <- deriveHDAddressAuxx nm sk
            printAction $
                sformat ("    #"%int%":   addr:      "%build%"\n"%
                         "          pk:        "%fullPublicKeyF%"\n"%
                         "          pk hash:   "%hashHexF%"\n"%
                         "          HD addr:   "%build)
                    i addr pk (addressHash pk) addrHD
        walletMB <- (^. usWallet) <$> (view userSecret >>= atomically . readTVar)
        whenJust walletMB $ \wallet -> do
            addrHD <- deriveHDAddressAuxx nm (_wusRootKey wallet)
            printAction $
                sformat ("    Wallet address:\n"%
                         "          HD addr:   "%build)
                    addrHD
        return ValueUnit
    , cpHelp = ""
    },

    return CommandProc
    { cpName = "help"
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> do
        printAction (mkHelpMessage commands)
        return ValueUnit
    , cpHelp = "display this message"
    }]
  where
    needsAuxxMode :: Name -> Either UnavailableCommand (Dict (MonadAuxxMode m))
    needsAuxxMode name =
        maybe (Left $ UnavailableCommand name "AuxxMode is not available") Right hasAuxxMode
    needsDiffusion :: Name -> Either UnavailableCommand (Diffusion m)
    needsDiffusion name =
        maybe (Left $ UnavailableCommand name "Diffusion layer is not available") Right mDiffusion
    needsProtocolMagic :: Name -> Either UnavailableCommand ProtocolMagic
    needsProtocolMagic name =
        maybe (Left $ UnavailableCommand name "ProtocolMagic is not available") Right mpm

procConst :: Applicative m => Name -> Value -> CommandProc m
procConst name value =
    CommandProc
    { cpName = name
    , cpArgumentPrepare = identity
    , cpArgumentConsumer = pure ()
    , cpExec = \() -> pure value
    , cpHelp = "constant"
    }

class ToLeft m a b where
    toLeft :: Either a b -> m a

instance (Monad m, ToLeft m a b, ToLeft m b c) => ToLeft m a (Either b c) where
    toLeft = toLeft <=< traverse toLeft

instance MonadAuxxMode m => ToLeft m PublicKey Int where
    toLeft = either return getPublicKeyFromIndex

instance MonadAuxxMode m => ToLeft m StakeholderId PublicKey where
    toLeft = return . either identity addressHash

getPublicKeyFromIndex :: MonadAuxxMode m => Int -> m PublicKey
getPublicKeyFromIndex i = do
    sks <- getSecretKeysPlain
    let sk = sks !! i
        pk = encToPublic sk
    evaluateNF pk
