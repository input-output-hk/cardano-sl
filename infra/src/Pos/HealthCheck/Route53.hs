module Pos.HealthCheck.Route53
       ( route53HealthCheckOption
       ) where

import qualified Options.Applicative as Opt
import           Pos.Util.TimeWarp (NetworkAddress, addrParser)
import           Pos.Util.OptParse (fromParsec)
import           Universum

route53HealthCheckOption :: Opt.Parser NetworkAddress
route53HealthCheckOption = Opt.option (fromParsec addrParser) $
    Opt.long "route53-health-check" <>
    Opt.metavar "IP:PORT" <>
    Opt.help "Host and port for the Route53 DNS health check."
