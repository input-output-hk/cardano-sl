module Pos.Client.CLI.Util
       ( printFlags
       ) where

import           Universum

import           Pos.Constants       (isDevelopment)
import           Pos.Util            (inAssertMode)
printFlags :: IO ()
printFlags = do
    if isDevelopment
        then putText "[Attention] We are in DEV mode"
        else putText "[Attention] We are in PRODUCTION mode"
    inAssertMode $ putText "Asserts are ON"
