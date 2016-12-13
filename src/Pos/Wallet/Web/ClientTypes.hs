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
-- Note: Cardano does not deal with other currency than ADA yet
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
  { cwAddress :: Address
  , cwAmount  :: Coin
  , cwMeta    :: CWalletMeta
  }

-- | Meta data of CWallet
-- Includes data which are not provided by Cardano
data CWalletMeta = CWalletMeta
  { cwType     :: CWalletType
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
-- all data of client are "meta data" - that is not provided by Cardano
-- (Flow type: accountType)
data CProfile = CProfile
  { cpName        :: Text
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
-- It can be an input / output / exchange transaction
data CTType
  = CTIn CTxMeta
  | CTOut CTxMeta
  | CTInOut CTExMeta -- Ex == exchange


-- | Client transaction (CTx)
-- Provides all Data about a transaction needed by client.
-- It includes meta data which are not part of Cardano, too
-- (Flow type: transactionType)
data CTx = CTx
  { ctId     :: TxId
  , ctAmount :: Coin
  , ctType   :: CTType -- it includes all "meta data"
  }

-- | meta data of transactions
data CTxMeta = CTxMeta
  { ctmCurrency    :: CCurrency
  , ctmTitle       :: Text
  , ctmDescription :: Text
  , ctmDate        :: UTCTime
  }

-- | meta data of exchanges
data CTExMeta = CTExMeta
  { cexCurrency    :: CCurrency
  , cexTitle       :: Text
  , cexDescription :: Text
  , cexDate        :: UTCTime
  , cexRate        :: Text
  , cexLabel       :: Text -- counter part of client's 'exchange' value
  , cexAddress     :: Address
  }
