{-# LANGUAGE TypeFamilies #-}

-- | Instances for client types

module Pos.Wallet.Web.ClientTypes.Instances () where

import           Universum

import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import           Data.List (partition)
import           Data.Text (splitOn)
import qualified Data.Text.Buildable
import           Formatting (bprint, build, int, sformat, (%))
import qualified Serokell.Util.Base16 as Base16
import           Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import           Servant.Multipart (FromMultipart (..), Mem, lookupFile, lookupInput)

import           Pos.Core (Address, Coin(..), coinToInteger, decodeTextAddress, checkCoin, mkCoin, unsafeGetCoin)
import           Pos.Core.Txp (TxId)
import           Pos.Crypto (PassPhrase, decodeHash, hashHexF, passphraseLength)
import           Pos.Util.Servant (FromCType (..), HasTruncateLogPolicy (..), OriginType,
                                   ToCType (..), WithTruncatedLog (..))
import           Pos.Wallet.Web.ClientTypes.Types (AccountId (..), CAccount (..), CAccountId (..),
                                                   CAddress (..), CCoin (..),
                                                   CElectronCrashReport (..), CHash (..), CId (..),
                                                   CPassPhrase (..), CPtxCondition (..), CTx (..),
                                                   CTxId (..), CWallet (..), ScrollLimit (..),
                                                   ScrollOffset (..), mkCTxId)
import           Pos.Wallet.Web.Pending.Types (PtxCondition (..))

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
            then Right $ ByteArray.convert bs
            else Left $ sformat
                 ("Expected password length 0 or "%int%", not "%int)
                 passphraseLength bl

instance ToCType CPassPhrase where
    encodeCType = CPassPhrase . Base16.encode . ByteArray.convert


type instance OriginType CAccountId = AccountId

instance FromCType CAccountId where
    decodeCType (CAccountId url) =
        case splitOn "@" url of
            [part1, part2] -> do
                aiWId  <- encodeCType <$> decodeTextAddress part1
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
    encodeCType = CCoin . show . unsafeGetCoin


type instance OriginType (CId w) = Address

instance ToCType (CId w) where
    -- TODO: this is not completely safe. If someone changes
    -- implementation of Buildable Address. It should be probably more
    -- safe to introduce `class PSSimplified` that would have the same
    -- implementation has it is with Buildable Address but then person
    -- will know it will probably change something for purescript.
    encodeCType = CId . CHash . sformat build

instance FromCType (CId w) where
    decodeCType (CId (CHash h)) = decodeTextAddress h


type instance OriginType CTxId = TxId

instance ToCType CTxId where
    encodeCType = mkCTxId . sformat hashHexF

instance FromCType CTxId where
    decodeCType (CTxId (CHash h)) = decodeHash h

type instance OriginType CPtxCondition = Maybe PtxCondition

instance ToCType CPtxCondition where
    encodeCType = maybe CPtxNotTracked $ \case
        PtxCreating{}       -> CPtxCreating
        PtxApplying{}       -> CPtxApplying
        PtxInNewestBlocks{} -> CPtxInBlocks
        PtxPersisted{}      -> CPtxInBlocks
        PtxWontApply{}      -> CPtxWontApply

----------------------------------------------------------------------------
-- Servant
----------------------------------------------------------------------------

-- ToHttpApiData instances are for benchmarking.

instance ToHttpApiData (CId a) where
    toQueryParam (CId (CHash text)) = text

instance ToHttpApiData CAccountId where
    toQueryParam (CAccountId text) = text

instance ToHttpApiData ScrollOffset where
    toQueryParam (ScrollOffset num) = toText (show num :: Text)

instance ToHttpApiData ScrollLimit where
    toQueryParam (ScrollLimit num) = toText (show num :: Text)

instance ToHttpApiData CPassPhrase where
    toQueryParam (CPassPhrase text) = text

instance ToHttpApiData Coin where
    toQueryParam = pretty . coinToInteger

instance FromHttpApiData Coin where
    parseUrlPiece p = do
        c <- Coin <$> parseQueryParam p
        checkCoin c
        pure c

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

instance FromHttpApiData ScrollOffset where
    parseUrlPiece = fmap ScrollOffset . parseUrlPiece

instance FromHttpApiData ScrollLimit where
    parseUrlPiece = fmap ScrollLimit . parseUrlPiece

instance FromMultipart Mem CElectronCrashReport where
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
-- Logs truncating policies
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
