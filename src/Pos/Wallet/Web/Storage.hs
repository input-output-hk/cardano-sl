{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- @jens: this document is inspired by https://github.com/input-output-hk/rscoin-haskell/blob/master/src/RSCoin/Explorer/Storage.hs
module Pos.Wallet.Web.Storage
       (
         Storage (..)
       , Query
       , Update
       , getDummyAttribute
       , setDummyAttribute
       ) where

import           Control.Lens  (makeClassy, view, (.=))
import           Data.Default  (Default, def)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Universum

data Storage = Storage
    {
      _dummyAttribute :: Int
    }

makeClassy ''Storage

deriveSafeCopySimple 0 'base ''Storage

instance Default Storage where
    def =
        Storage
        {
          _dummyAttribute = 0
        }

type Query a = forall m. MonadReader Storage m => m a
type Update a = forall m. (MonadThrow m, MonadState Storage m) => m a

getDummyAttribute :: Query Int
getDummyAttribute = view dummyAttribute

setDummyAttribute :: Int -> Update ()
setDummyAttribute x = dummyAttribute .= x
