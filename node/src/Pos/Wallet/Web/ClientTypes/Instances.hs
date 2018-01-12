{-# LANGUAGE TypeFamilies #-}

-- | Instances for client types

module Pos.Wallet.Web.ClientTypes.Instances () where

import           Universum

import qualified Data.ByteArray                       as ByteArray
import qualified Data.ByteString                      as BS
import           Data.List                            (intersperse, partition)
import           Data.Text                            (splitOn)
import qualified Data.Text.Buildable
import           Data.Version                         (showVersion)
import           Formatting                           (bprint, build, builder, int,
                                                       sformat, shown, string, (%))
import           Serokell.Util                        (listJsonIndent)
import qualified Serokell.Util.Base16                 as Base16
import           Servant.API                          (FromHttpApiData (..))
import           Servant.Multipart                    (FromMultipart (..), lookupFile,
                                                       lookupInput)

import           Pos.Core                             (Address, Coin, decodeTextAddress,
                                                       mkCoin)
import           Pos.Crypto                           (PassPhrase, decodeHash,
                                                       passphraseLength)
import           Pos.Txp.Core.Types                   (TxId)
import           Pos.Util.LogSafe                     (SecureLog (..), buildUnsecure)
import           Pos.Util.Servant                     (FromCType (..),
                                                       HasTruncateLogPolicy (..),
                                                       OriginType, ToCType (..),
                                                       WithTruncatedLog (..))
import           Pos.Wallet.Web.ClientTypes.Functions (addressToCId, cIdToAddress,
                                                       mkCCoin, mkCTxId,
                                                       ptxCondToCPtxCond, txIdToCTxId)
import           Pos.Wallet.Web.ClientTypes.Types     (AccountId (..), ApiVersion (..),
                                                       CAccount (..), CAccountId (..),
                                                       CAccountInit (..),
                                                       CAccountMeta (..), CAddress (..),
                                                       CCoin (..),
                                                       CElectronCrashReport (..),
                                                       CFilePath (..), CHash (..),
                                                       CId (..), CInitialized (..),
                                                       CPaperVendWalletRedeem (..),
                                                       CPassPhrase (..), CProfile (..),
                                                       CPtxCondition, CTx (..),
                                                       CTxId (..), CTxMeta (..),
                                                       CUpdateInfo (..), CWallet (..),
                                                       CWallet, CWalletAssurance,
                                                       CWalletInit (..), CWalletMeta (..),
                                                       CWalletRedeem (..),
                                                       ClientInfo (..), SinceTime (..),
                                                       ScrollLimit (..), ScrollOffset (..),
                                                       SyncProgress (..))
import           Pos.Wallet.Web.Pending.Types         (PtxCondition)

-- TODO [CSM-407] Maybe revert dependency between Functions and Instances modules?
-- This would allow to get tid of functions like 'ptxCondToCPtxCond' :/

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

-- TODO [CSM-407] Move these instances to Types.hs module.
-- I don't want to do it now because we have pending refactoring which reordered
-- everything where

instance Buildable (SecureLog (CId __)) where
    build _ = "<id>"

instance Buildable (SecureLog CAccountId) where
    build _ = "<account id>"

instance Buildable (SecureLog CTxId) where
    build _ = "<tx id>"

instance Buildable CWalletAssurance where
    build = bprint shown

instance Buildable CWalletMeta where
    build CWalletMeta{..} =
        bprint ("("%build%"/"%build%")")
               cwAssurance cwUnit

instance Buildable (SecureLog CWalletMeta) where
    build = buildUnsecure

instance Buildable (CWalletInit) where
    build CWalletInit{..} = "<wallet init>"

instance Buildable (SecureLog CWalletInit) where
    build _ = "<wallet init>"

instance Buildable CWallet where
    build CWallet{..} =
        bprint ("{ id="%build
                %" meta="%build
                %" accs="%build
                %" amount="%build
                %" pass="%build
                %" passlu="%build
                %" }")
        cwId
        cwMeta
        cwAccountsNumber
        cwAmount
        cwHasPassphrase
        cwPassphraseLU

instance Buildable CAccountMeta where
    build CAccountMeta{..} = "<meta>"

instance Buildable (SecureLog CAccountMeta) where
    build _ = "<meta>"

instance Buildable CAccountInit where
    build CAccountInit{..} =
        bprint ("{ id="%build
                %" meta="%build
                %" }")
        caInitWId
        caInitMeta

instance Buildable (SecureLog CAccountInit) where
    build _ = "<account init>"

instance Buildable CAccount where
    build CAccount{..} =
        bprint ("{ id="%build
                %" meta="%build
                %" amount="%build%"\n"
                %" addrs="%listJsonIndent 4
                %" }")
        caId
        caMeta
        caAmount
        caAddresses

instance Buildable CAddress where
    build CAddress{..} =
        bprint ("{ id="%build%"\n"
                %" amount="%build
                %" used="%build
                %" change="%build
                %" }")
        cadId
        cadAmount
        cadIsUsed
        cadIsChange

instance Buildable CTxMeta where
    build CTxMeta{..} = bprint ("{ date="%build%" }") ctmDate

instance Buildable (SecureLog CTxMeta) where
    build _ = "<tx meta>"

instance Buildable CPtxCondition where
    build = bprint shown

instance Buildable CTx where
    build CTx{..} =
        bprint ("{ id="%build
                %" amount="%build
                %" confirms="%build
                %" meta="%build%"\n"
                %" inputs="%builder%"\n"
                %" outputs="%builder%"\n"
                %" local="%build
                %" outgoing="%build
                %" condition="%build
                %" }")
        ctId
        ctAmount
        ctConfirmations
        ctMeta
        (buildTxEnds ctInputs)
        (buildTxEnds ctOutputs)
        ctIsLocal
        ctIsOutgoing
        ctCondition
      where
        buildTxEnds =
            mconcat . intersperse ", " .
            map (uncurry $ bprint (build%" - "%build))

instance Buildable CProfile where
    build CProfile{..} =
        bprint ("{ cpLocale="%build%" }") cpLocale

instance Buildable (SecureLog CProfile) where
    build = buildUnsecure

instance Buildable CUpdateInfo where
    build CUpdateInfo{..} =
        bprint ("{ softver="%build
                %" blockver="%build
                %" scriptver="%build
                %" implicit="%build
                %" for="%build
                %" against="%build
                %" pos stake="%build
                %" neg stake="%build
                %" }")
        cuiSoftwareVersion
        cuiBlockVesion  -- TODO [CSM-407] lol what is it?
        cuiScriptVersion
        cuiImplicit
        cuiVotesFor
        cuiVotesAgainst
        cuiPositiveStake
        cuiNegativeStake

instance Buildable SyncProgress where
    build SyncProgress{..} =
        bprint ("progress="%build%"/"%build%" peers="%build)
               _spLocalCD _spNetworkCD _spPeers

instance Buildable CWalletRedeem where
    build CWalletRedeem{..} =
        bprint (build%" -> "%build) crSeed crWalletId

instance Buildable (SecureLog CWalletRedeem) where
    build _ = "<wallet redeem info>"

instance Buildable CPaperVendWalletRedeem where
    build CPaperVendWalletRedeem{..} =
        bprint (build%" -> "%build) pvSeed pvWalletId

instance Buildable (SecureLog CPaperVendWalletRedeem) where
    build (SecureLog CPaperVendWalletRedeem{..}) =
        "<papervend wallet redeem info>"

instance Buildable CInitialized where
    build CInitialized{..} =
        bprint (build%"/"%build)
               cPreInit cTotalTime

instance Buildable (SecureLog CInitialized) where
    build = buildUnsecure

instance Buildable (SecureLog SinceTime) where
    build = buildUnsecure

instance Buildable (SecureLog ScrollOffset) where
    build = buildUnsecure

instance Buildable (SecureLog ScrollLimit) where
    build = buildUnsecure

instance Buildable (SecureLog CFilePath) where
    build _ = "<filepath>"

----------------------------------------------------------------------------
-- Convertions
----------------------------------------------------------------------------

type instance OriginType CPassPhrase = PassPhrase

-- These two instances nearly duplicate `instance Bi PassPhrase`
-- in `Pos.Binary.Crypto`.
instance FromCType CPassPhrase where
    decodeCType (CPassPhrase text) = do
        bs <- Base16.decode text
        let bl = BS.length bs
        -- Currently passphrase may be either 32-byte long or empty (for
        -- unencrypted keys).
        if bl == 0 || bl == passphraseLength
            then pure $ ByteArray.convert bs
            else fail . toString $ sformat
                 ("Expected password length 0 or "%int%", not "%int)
                 passphraseLength bl

instance ToCType CPassPhrase where
    encodeCType = CPassPhrase . Base16.encode . ByteArray.convert


type instance OriginType CAccountId = AccountId

instance FromCType CAccountId where
    decodeCType (CAccountId url) =
        case splitOn "@" url of
            [part1, part2] -> do
                aiWId  <- addressToCId <$> decodeTextAddress part1
                aiIndex <- maybe (Left "Invalid wallet index") Right $
                            readMaybe $ toString part2
                return AccountId{..}
            _ -> Left "Expected 2 parts separated by '@'"

instance ToCType CAccountId where
    encodeCType = CAccountId . sformat build


type instance OriginType CCoin = Coin

instance FromCType CCoin where
    decodeCType =
        maybe (Left "Invalid coins amount") Right .
        fmap mkCoin . readMaybe . toString . getCCoin

instance ToCType CCoin where
    encodeCType = mkCCoin


type instance OriginType (CId w) = Address

instance FromCType (CId w) where
    decodeCType = cIdToAddress

instance ToCType (CId w) where
    encodeCType = addressToCId


type instance OriginType CTxId = TxId

instance ToCType CTxId where
    encodeCType = txIdToCTxId

instance FromCType CTxId where
    decodeCType (CTxId (CHash h)) = decodeHash h

type instance OriginType CPtxCondition = Maybe PtxCondition

instance ToCType CPtxCondition where
    encodeCType = ptxCondToCPtxCond

----------------------------------------------------------------------------
-- Servant
----------------------------------------------------------------------------

instance FromHttpApiData Coin where
    parseUrlPiece = fmap mkCoin . parseUrlPiece

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress

instance FromHttpApiData (CId w) where
    parseUrlPiece = pure . CId . CHash

instance FromHttpApiData CAccountId where
    parseUrlPiece = fmap CAccountId . parseUrlPiece

-- FIXME: unsafe (temporary, will be removed probably in future)
-- we are not checking whether received Text is really valid CTxId
instance FromHttpApiData CTxId where
    parseUrlPiece = pure . mkCTxId

instance FromHttpApiData CPassPhrase where
    parseUrlPiece = pure . CPassPhrase

instance FromHttpApiData SinceTime where
    parseUrlPiece = fmap SinceTime . parseUrlPiece

instance FromHttpApiData ScrollOffset where
    parseUrlPiece = fmap ScrollOffset . parseUrlPiece

instance FromHttpApiData ScrollLimit where
    parseUrlPiece = fmap ScrollLimit . parseUrlPiece

instance FromMultipart CElectronCrashReport where
    fromMultipart form = do
        let look t = lookupInput t form
        CElectronCrashReport
          <$> look "ver"
          <*> look "platform"
          <*> look "process_type"
          <*> look "guid"
          <*> look "_version"
          <*> look "_productName"
          <*> look "prod"
          <*> look "_companyName"
          <*> lookupFile "upload_file_minidump" form

----------------------------------------------------------------------------
-- Truncated Buildable
----------------------------------------------------------------------------

instance HasTruncateLogPolicy CTx where
    -- Related logs are printed often.
    -- Tx history changes increamentally, order is newest first
    truncateLogPolicy = take 5

instance HasTruncateLogPolicy CWallet where
    -- Related logs are printed seldom.
    -- Contains constantly chaning info (balance)
    truncateLogPolicy = identity

instance HasTruncateLogPolicy CAccount where
    -- Currently each wallet has single account, so same logic as for 'CWallet'
    -- instance.
    truncateLogPolicy = identity

instance HasTruncateLogPolicy CAddress where
    -- Their number has the same order as number of transactions made from
    -- parent account. However, addresses with no money consistute the majority
    -- and are not very interesting.
    truncateLogPolicy addrs =
        let (withoutMoney, withMoney) = partition ((== zeroMoney) . cadAmount) addrs
        in take 5 (withMoney <> withoutMoney)
      where
        zeroMoney = encodeCType minBound

instance Buildable (WithTruncatedLog CAccount) where
    build (WithTruncatedLog CAccount{..}) =
        bprint ("{ id="%build
                %" meta="%build
                %" amount="%build%"\n"
                %" addrs="%build
                %" }")
        caId
        caMeta
        caAmount
        (WithTruncatedLog caAddresses)

-- TODO [CSM-466] deal with this hack
instance Buildable (WithTruncatedLog ([CTx], Word)) where
    build (WithTruncatedLog (ctxs, size)) =
        bprint ("Num: "%build%", entries: \n"%build)
            size (WithTruncatedLog ctxs)

-- TODO [CSM-466] deal with this hack especially
instance (Buildable e, Buildable (WithTruncatedLog a)) =>
         Buildable (WithTruncatedLog (Either e a)) where
    build (WithTruncatedLog x) =
        case x of
            Left e  -> bprint ("Failure: "%build) e
            Right a -> bprint build (WithTruncatedLog a)

instance Buildable ApiVersion where
    build ApiVersion0 = "ApiVersion0"

instance Buildable ClientInfo where
    build ClientInfo {..} =
        bprint ("{ gitRevision="%build
                %" apiVersion="%build
                %" softwareVersion="%build
                %" cabalVersion="%string
                %" }")
        ciGitRevision
        ciApiVersion
        ciSoftwareVersion
        (showVersion ciCabalVersion)
