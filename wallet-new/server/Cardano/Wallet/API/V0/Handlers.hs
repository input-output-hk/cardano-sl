{-# LANGUAGE OverloadedStrings #-}
module Cardano.Wallet.API.V0.Handlers where

import qualified Cardano.Wallet.API.V0 as V0
import Cardano.Wallet.API.Types

import Data.Text
import Servant

handlers :: Server V0.API
handlers = apiVersion

apiVersion :: Handler APIVersion
apiVersion = return V0
