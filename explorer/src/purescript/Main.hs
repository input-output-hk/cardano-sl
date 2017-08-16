module Main
       ( main
       ) where

import           Data.Proxy                         (Proxy (..))
import           Language.PureScript.Bridge         (BridgePart, buildBridge,
                                                     defaultBridge, mkSumType, typeName,
                                                     writePSTypes, (<|>), (^==))
import           Language.PureScript.Bridge.PSTypes (psInt)
import           Universum

import qualified Pos.Explorer.Socket                as PS
import qualified Pos.Explorer.Web.ClientTypes       as CT
import qualified Pos.Explorer.Web.Error             as CE

import           PSOptions                          (Args (..), getPSOptions)
import           PSTypes                            (psPosixTime)

main :: IO ()
main = do
    Args {..} <- getPSOptions
    writePSTypes
      bridgePath
      (buildBridge customBridge)
      [ mkSumType (Proxy @CT.CAddress)
      , mkSumType (Proxy @CT.CAddressSummary)
      , mkSumType (Proxy @CT.CBlockEntry)
      , mkSumType (Proxy @CT.CBlockSummary)
      , mkSumType (Proxy @CT.CAddressType)
      , mkSumType (Proxy @CT.CHash)
      , mkSumType (Proxy @CT.CNetworkAddress)
      , mkSumType (Proxy @CT.CTxBrief)
      , mkSumType (Proxy @CT.CTxEntry)
      , mkSumType (Proxy @CT.CTxId)
      , mkSumType (Proxy @CT.CTxSummary)
      , mkSumType (Proxy @CE.ExplorerError)
      , mkSumType (Proxy @CT.CCoin)
      , mkSumType (Proxy @PS.ClientEvent)
      , mkSumType (Proxy @PS.ServerEvent)
      , mkSumType (Proxy @PS.Subscription)
      , mkSumType (Proxy @CT.EpochIndex)
      , mkSumType (Proxy @CT.LocalSlotIndex)
      , mkSumType (Proxy @CT.CGenesisSummary)
      , mkSumType (Proxy @CT.CGenesisAddressInfo)
      ]
  where
      customBridge =
          defaultBridge     <|>
          posixTimeBridge   <|>
          wordBridge        <|>
          word8Bridge       <|>
          word16Bridge      <|>
          word32Bridge      <|>
          word64Bridge

posixTimeBridge :: BridgePart
posixTimeBridge = typeName ^== "NominalDiffTime" >> pure psPosixTime

wordBridge :: BridgePart
wordBridge = typeName ^== "Word" >> pure psInt

word8Bridge :: BridgePart
word8Bridge = typeName ^== "Word8" >> pure psInt

word16Bridge :: BridgePart
word16Bridge = typeName ^== "Word16" >> pure psInt

word32Bridge :: BridgePart
word32Bridge = typeName ^== "Word32" >> pure psInt

word64Bridge :: BridgePart
word64Bridge = typeName ^== "Word64" >> pure psInt
