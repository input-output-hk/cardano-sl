{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Cardano.Wallet.Main (wallet_main)
import Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)

main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) wallet_main
