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
      ( CCurrency (..)
      , CTType (..)
      , CProfile (..)
      , CPwHash
      , CTx (..)
      , CTxMeta (..)
      , CTExMeta (..)
      , CWallet (..)
      , CWalletType (..)
      , CWalletMeta (..)
      ) where

import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)
import           GHC.Generics    (Generic)
import           Universum

import           Pos.Types       (Address, Coin, TxId)


-- | currencies handled by client
-- Note: Cardano does not deal with other currency than ADA yet
data CCurrency
    = ADA
    | BTC
    | ETH
    deriving (Show,Generic)

----------------------------------------------------------------------------
-- wallet
----------------------------------------------------------------------------
-- | Can be used as personal or shared wallet
data CWalletType
    = CWTPersonal
    | CWTShared
    deriving (Show, Generic)

-- | Client Wallet (CW)
-- (Flow type: walletType)
data CWallet = CWallet
    { cwAddress :: Address
    , cwAmount  :: Coin
    , cwMeta    :: CWalletMeta
    } deriving (Show, Generic)

-- | Meta data of CWallet
-- Includes data which are not provided by Cardano
data CWalletMeta = CWalletMeta
    { cwType     :: CWalletType
    , cwCurrency :: CCurrency
    , cwName     :: Text
    , cwLastUsed :: Bool
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
    , cpPwCreated   :: NominalDiffTime
    , cpLocale      :: Text
    } deriving (Show, Generic)

----------------------------------------------------------------------------
-- transactions
----------------------------------------------------------------------------

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
    { ctId     :: TxId
    , ctAmount :: Coin
    , ctType   :: CTType -- it includes all "meta data"
    } deriving (Show, Generic)

-- | meta data of transactions
data CTxMeta = CTxMeta
    { ctmCurrency    :: CCurrency
    , ctmTitle       :: Text
    , ctmDescription :: Text
    , ctmDate        :: NominalDiffTime
    } deriving (Show, Generic)

-- | meta data of exchanges
data CTExMeta = CTExMeta
    { cexCurrency    :: CCurrency
    , cexTitle       :: Text
    , cexDescription :: Text
    , cexDate        :: NominalDiffTime
    , cexRate        :: Text
    , cexLabel       :: Text -- counter part of client's 'exchange' value
    , cexAddress     :: Address
    } deriving (Show, Generic)
