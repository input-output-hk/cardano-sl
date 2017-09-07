{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

-- | This module provides a function to run Rubbish's command.

module Command.Run
       ( runCmd
       ) where

import           Universum

import           Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58)
import           Data.List              ((!!))
import           Formatting             (build, int, sformat, stext, (%))
import           NeatInterpolation      (text)

import           Pos.Binary             (serialize')
import           Pos.Communication      (MsgType (..), Origin (..), SendActions, dataFlow,
                                         immediateConcurrentConversations)
import           Pos.Core               (addressHash, coinF)
import           Pos.Core.Address       (makeAddress)
import           Pos.Core.Context       (HasCoreConstants)
import           Pos.Core.Types         (AddrAttributes (..), AddrSpendingData (..))
import           Pos.Crypto             (emptyPassphrase, encToPublic, fullPublicKeyHexF,
                                         hashHexF, noPassEncrypt, safeCreatePsk,
                                         withSafeSigner)
import           Pos.Rubbish            (LightWalletMode, makePubKeyAddressRubbish)
import           Pos.Util.UserSecret    (readUserSecret, usKeys)
import           Pos.Wallet             (addSecretKey, getBalance, getSecretKeys)

import qualified Command.Tx             as Tx
import           Command.Types          (CmdCtx (..), Command (..))
import qualified Command.Update         as Update


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
runCmd sendActions (Vote idx decision upId) ctx =
    Update.vote sendActions idx decision upId ctx
runCmd sendActions (ProposeUpdate params) ctx =
    Update.propose sendActions params ctx
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
