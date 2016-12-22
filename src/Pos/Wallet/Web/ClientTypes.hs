{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
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
      ) where

import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import           Universum

import           Formatting      (build, sformat)
import           Pos.Aeson.Types ()
import           Pos.Types       (Address (..), Coin)


-- | currencies handled by client
-- Note: Cardano does not deal with other currency than ADA yet
data CCurrency
    = ADA
    | BTC
    | ETH
    deriving (Show, Generic)

-- | Client hash
newtype CHash = CHash Text deriving (Show, Generic)

-- | Client address
newtype CAddress = CAddress CHash deriving (Show, Generic)

-- | transform Address into CAddress
-- TODO: this is not complitely safe. If someone changes implementation of Buildable Address. It should be probably more safe to introduce `class PSSimplified` that would have the same implementation has it is with Buildable Address but then person will know it will probably change something for purescript.
addressToCAddress :: Address -> CAddress
addressToCAddress = CAddress . CHash . sformat build

-- | Client transaction id
newtype CTxId = CTxId CHash deriving (Show, Generic)

-- | transform TxId into CTxId
-- txIdToCTxId :: TxId -> CTxId
-- txIdToCTxId = undefined

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
    , cwLastUsed :: Bool
    } deriving (Show, Generic)

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
    , cpPwCreated   :: Text -- TODO jk: should be NominalDiffTime
    , cpLocale      :: Text
    } deriving (Show, Generic)

----------------------------------------------------------------------------
-- transactions
----------------------------------------------------------------------------

-- | meta data of transactions
data CTxMeta = CTxMeta
    { ctmCurrency    :: CCurrency
    , ctmTitle       :: Text
    , ctmDescription :: Text
    , ctmDate        :: Text -- TODO jk: should be NominalDiffTime
    } deriving (Show, Generic)

-- | type of transactions
-- It can be an input / output / exchange transaction
data CTType
    = CTIn CTxMeta
    | CTOut CTxMeta
    | CTInOut CTExMeta -- Ex == exchange
    deriving (Show, Generic)

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
