{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | client types

-- (this module will be moved later to anywhere else,
-- just to have a starting point)

module Pos.Wallet.Web.ClientTypes
       ( CWallet (..)
       , CCurrency (..)
       ) where

import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           Universum

import           Pos.Types       (Address, Coin, TxId)


-- | currencies handled by client
data CCurrency
  = ADA
  | BTC
  | ETH

----------------------------------------------------------------------------
-- wallet
----------------------------------------------------------------------------
-- | Can be used as personal or shared wallet
data CWalletType
  = CWTPersonal
  | CWTShared

-- | Client Wallet (CW)
-- (Flow type: walletType)
data CWallet = CWallet
  { cwAddress  :: Address
  , cwAmount   :: Coin
  -- "meta data"
  , cwType     :: CWalletType
  , cwCurrency :: CCurrency
  , cwName     :: Text
  , cwLastUsed :: Bool
}


----------------------------------------------------------------------------
-- profile
----------------------------------------------------------------------------

-- | Password hash of client profile
type CPwHash = Text -- or Base64 or something else

-- | Client profile (CP)
-- (Flow type: accountType)
data CProfile = CProfile
  { -- all are "meta data"
  cpName          :: Text
  , cpEmail       :: Text
  , cpPhoneNumber :: Text
  , cpPwHash      :: CPwHash
  , cpPwCreated   :: UTCTime
  , cpLocale      :: Text
}

----------------------------------------------------------------------------
-- transactions
----------------------------------------------------------------------------

-- | type of transactions
-- (client has 'card', 'adaExpand', 'adaIncome', 'exchange',
-- but we do need just in- / output and both)
data CTType
  = CTIn -- TxIn
  | CTOut
  | CTInOut


-- | Client transaction (CT)
-- TODO jk: It's need more work on this!!
-- (Flow type: transactionType)
data CTx = CTx {
  ctId                    :: TxId
  -- "meta data"
  , ctType                :: CTType
  , ctAmount              :: Coin -- TxOut txOutValue
  , ctCurrency            :: CCurrency
  , ctTitle               :: Text
  , ctDescription         :: Text
  , ctDate                :: UTCTime
  , ctExchangeRate        :: Maybe Text
  , ctExchangeDescription :: Maybe Text -- client's 'exchange' value
  , ctAddress             :: Maybe Address
}
