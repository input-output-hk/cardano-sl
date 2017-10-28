{-# LANGUAGE QuasiQuotes #-}

module Command.Help
       ( helpMessage
       ) where

import           Universum

import           NeatInterpolation (text)

helpMessage :: Text
helpMessage = [text|
Avaliable commands:
   balance <address>              -- check balance on given address (may be any address)
   send <N> [<address> <coins>]+  -- create and send transaction with given outputs
                                     from own address #N
   send-to-all-genesis <duration> <conc> <delay> <sendmode> <csvfile>
                                  -- create and send transactions from all genesis addresses for <duration>
                                     seconds, delay in ms.  conc is the number of threads that send
                                     transactions concurrently. sendmode can be one of "neighbours",
                                     "round-robin", and "send-random".
   vote <N> <decision> <upid>     -- send vote with given hash of proposal id (in base16) and
                                     decision, from own address #N
   propose-update <N> [vote-all] <block ver> <software ver> <script ver> <slot duration> <max block size> <propose_file>?
                                  -- propose an update with given versions and other data
                                     with one positive vote for it, from own address #N
                                     if vote-all flag is set then votes from all secret keys also will be sent

   propose-unlock-stake-epoch <N> <block ver> <software ver> <epoch>
                                  -- propose an update with the specified unlock stake epoch,
                                  -- with one positive vote for it, from our own address #N

   listaddr                       -- list own addresses
   delegate-light <N> <M> <eStart> <eEnd>?
                                  -- delegate secret key #N to pk <M> light version (M is encoded in base58),
                                     where eStart is cert start epoch, eEnd -- expire epoch
   delegate-heavy <N> <M> <e>     -- delegate secret key #N to pk <M> heavyweight (M is encoded in base58),
                                     e is current epoch.
   add-key-pool <N>               -- add key from intial pool
   add-key <file> [primary]       -- add key from file, if primary flag is set then add only primary key

   addr-distr <N> boot
   addr-distr <N> [<M>:<coinPortion>]+
                                  -- print the address for pk <N> (encoded in base58) with the specified distribution,
                                  -- where <M> is stakeholder id (pk hash), and the coin portion can be a coefficient
                                  -- in [0..1] or a percentage (ex. 42%)

   rollback <N> <file>            -- Rollback <N> blocks (genesis or main doesn't matter) and dump transactions from
                                  -- them to <file> in binary format.

   generate-blocks <N> <seed>?    -- Generate blocks. Parameters are number of blocks and seed.
   send-from-file <file>          -- Read transactions in binary format from <file> and submit them to the network.
                                  -- <file> should be in format produced by 'rollback' command.

   help                           -- show this message
   quit                           -- shutdown node wallet
|]
