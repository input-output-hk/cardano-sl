{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | client types

-- (this module will be moved later to anywhere else,
-- just to have a starting point)

module Pos.Wallet.Web.ClientTypes
      ( CAddress
      , CCurrency (..)
      , CHash
      , CTType (..)
      , CProfile (..)
      , CPwHash
      , CTx (..)
      , CTxId
      , CTxMeta (..)
      , CTExMeta (..)
      , CWallet (..)
      , CWalletType (..)
      , CWalletMeta (..)
      , addressToCAddress
      , cAddressToAddress
      , mkCTx
      , mkCTxId
      , txIdToCTxId
      , ctTypeMeta
      ) where

import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Universum

import           Data.Default          (Default, def)
import           Data.Hashable         (Hashable (..))
import           Data.Time.Clock.POSIX (POSIXTime)
import           Formatting            (build, sformat)
import           Pos.Aeson.Types       ()
import           Pos.Types             (Address (..), Coin, Tx, TxId, decodeTextAddress,
                                        sumCoins, txOutAddress, txOutValue, txOutputs)
import           Pos.Types.Coin        (unsafeIntegerToCoin)

-- | currencies handled by client
-- Note: Cardano does not deal with other currency than ADA yet
data CCurrency
    = ADA
    | BTC
    | ETH
    deriving (Show, Read, Generic)

-- | Client hash
newtype CHash = CHash Text deriving (Show, Eq, Generic, Buildable)

instance Hashable CHash where
    hashWithSalt s (CHash h) = hashWithSalt s h

-- | Client address
newtype CAddress = CAddress CHash deriving (Show, Eq, Generic, Hashable, Buildable)

-- | transform Address into CAddress
-- TODO: this is not complitely safe. If someone changes implementation of Buildable Address. It should be probably more safe to introduce `class PSSimplified` that would have the same implementation has it is with Buildable Address but then person will know it will probably change something for purescript.
addressToCAddress :: Address -> CAddress
addressToCAddress = CAddress . CHash . sformat build

cAddressToAddress :: CAddress -> Either Text Address
cAddressToAddress (CAddress (CHash h)) = decodeTextAddress h

-- | Client transaction id
newtype CTxId = CTxId CHash deriving (Show, Eq, Generic, Hashable)

mkCTxId :: Text -> CTxId
mkCTxId = CTxId . CHash

-- | transform TxId into CTxId
txIdToCTxId :: TxId -> CTxId
txIdToCTxId = mkCTxId . sformat build

mkCTx :: Address -> (TxId, Tx, Bool) -> CTxMeta -> CTx
mkCTx addr (txId, tx, isOutgoing) = CTx (txIdToCTxId txId) outputCoins . meta
  where
    outputCoins = unsafeIntegerToCoin . sumCoins . map txOutValue $
        filter (xor isOutgoing . (== addr) . txOutAddress) $ txOutputs tx
    meta = if isOutgoing then CTOut else CTIn

----------------------------------------------------------------------------
-- wallet
----------------------------------------------------------------------------

-- | A wallet can be used as personal or shared wallet
data CWalletType
    = CWTPersonal
    | CWTShared
    deriving (Show, Generic)

-- | Meta data of CWallet
-- Includes data which are not provided by Cardano
data CWalletMeta = CWalletMeta
    { cwType     :: CWalletType
    , cwCurrency :: CCurrency
    , cwName     :: Text
    } deriving (Show, Generic)

instance Default CWalletMeta where
    def = CWalletMeta CWTPersonal ADA ""

-- | Client Wallet (CW)
-- (Flow type: walletType)
data CWallet = CWallet
    { cwAddress :: CAddress
    , cwAmount  :: Coin
    , cwMeta    :: CWalletMeta
    } deriving (Show, Generic)

----------------------------------------------------------------------------
-- profile
----------------------------------------------------------------------------

-- | Password hash of client profile
type CPwHash = Text -- or Base64 or something else

-- | Client profile (CP)
-- all data of client are "meta data" - that is not provided by Cardano
-- (Flow type: accountType)
data CProfile = CProfile
    { cpName        :: Text
    , cpEmail       :: Text
    , cpPhoneNumber :: Text
    , cpPwHash      :: CPwHash
    , cpPwCreated   :: POSIXTime
    , cpLocale      :: Text
    , cpPicture     :: Text -- TODO: base64
    } deriving (Show, Generic)

----------------------------------------------------------------------------
-- transactions
----------------------------------------------------------------------------

-- | meta data of transactions
data CTxMeta = CTxMeta
    { ctmCurrency    :: CCurrency
    , ctmTitle       :: Text
    , ctmDescription :: Text
    , ctmDate        :: POSIXTime
    } deriving (Show, Generic)

-- | type of transactions
-- It can be an input / output / exchange transaction
data CTType
    = CTIn CTxMeta
    | CTOut CTxMeta
    -- | CTInOut CTExMeta -- Ex == exchange
    deriving (Show, Generic)

ctTypeMeta :: CTType -> CTxMeta
ctTypeMeta (CTIn meta)  = meta
ctTypeMeta (CTOut meta) = meta

-- | Client transaction (CTx)
-- Provides all Data about a transaction needed by client.
-- It includes meta data which are not part of Cardano, too
-- (Flow type: transactionType)
data CTx = CTx
    { ctId     :: CTxId
    , ctAmount :: Coin
    , ctType   :: CTType -- it includes all "meta data"
    } deriving (Show, Generic)

-- | meta data of exchanges
data CTExMeta = CTExMeta
    { cexCurrency    :: CCurrency
    , cexTitle       :: Text
    , cexDescription :: Text
    , cexDate        :: Text -- TODO jk: should be NominalDiffTime
    , cexRate        :: Text
    , cexLabel       :: Text -- counter part of client's 'exchange' value
    , cexAddress     :: CAddress
    } deriving (Show, Generic)
