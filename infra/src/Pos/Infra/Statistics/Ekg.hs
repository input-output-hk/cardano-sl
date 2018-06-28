{-# LANGUAGE ApplicativeDo #-}

module Pos.Infra.Statistics.Ekg
       ( EkgParams (..)
       , ekgParamsOption
       ) where

import qualified Options.Applicative as Opt
import           Pos.Infra.Util.TimeWarp (NetworkAddress, addrParser)
import           Pos.Util.OptParse (fromParsec)
import           Universum

data EkgParams = EkgParams
    { ekgHost :: !ByteString
    , ekgPort :: !Int
    } deriving (Show)

ekgParamsOption :: Opt.Parser EkgParams
ekgParamsOption = do
    addr <- ekgServerOption
    pure $ EkgParams
        { ekgHost = fst addr
        , ekgPort = fromIntegral (snd addr)
        }

ekgServerOption :: Opt.Parser NetworkAddress
ekgServerOption = Opt.option (fromParsec addrParser) $
    Opt.long "ekg-server" <>
    Opt.metavar "IP:PORT" <>
    Opt.help "Host and port for the EKG server"
