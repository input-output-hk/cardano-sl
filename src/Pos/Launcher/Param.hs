-- | Parameters for launching everything.

module Pos.Launcher.Param
       ( LoggingParams (..)
       , BaseParams (..)
       , NodeParams (..)
       , Backpressure (..)
       , noBackpressure
       , parseBackpressure
       ) where

import           Pos.Crypto          (SecretKey)
import           Pos.Security.CLI    (AttackTarget, AttackType)
import           Pos.Txp.Toil.Types  (Utxo)
import           Pos.Types           (Timestamp)
import           Pos.Update.Params   (UpdateParams)
import           Pos.Util.UserSecret (UserSecret)
import           System.Wlog         (LoggerName)
import           Data.Time.Units     (Microsecond)
import           Data.Word           (Word32)
import qualified Text.Parsec.String  as P
import qualified Text.Parsec.Char    as P
import qualified Text.Parsec.Number  as P
import           Universum

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpRunnerTag     :: !LoggerName        -- ^ Prefix for logger, like "time-slave"
    , lpHandlerPrefix :: !(Maybe FilePath)  -- ^ Prefix of path for all logs
    , lpConfigPath    :: !(Maybe FilePath)  -- ^ Path to logger configuration
    , lpEkgPort       :: !(Maybe Int)
    } deriving (Show)

-- | Contains basic & networking parameters for running node.
data BaseParams = BaseParams
    { bpLoggingParams :: !LoggingParams  -- ^ Logger parameters
    } deriving (Show)

-- | Contains algorithm specific & storage parameters for Node.
data NodeParams = NodeParams
    { npDbPathM       :: !FilePath          -- ^ Path to node's database.
    , npRebuildDb     :: !Bool              -- ^ @True@ if data-base should be rebuilt
    , npSystemStart   :: !Timestamp         -- ^ System start
    , npSecretKey     :: !SecretKey         -- ^ Primary secret key of node
    , npUserSecret    :: !UserSecret        -- ^ All node secret keys
    , npBaseParams    :: !BaseParams        -- ^ See 'BaseParams'
    , npCustomUtxo    :: !Utxo              -- ^ predefined custom utxo
    , npJLFile        :: !(Maybe FilePath)  -- @georgeee please write comment to this field when you see this sign, i made it very long on purpose so it won't fit even in your huge monitor
    , npAttackTypes   :: ![AttackType]      -- ^ List of attack types used by malicious emulation
    , npAttackTargets :: ![AttackTarget]    -- ^ List of targets to attack by malicious emulation
    , npPropagation   :: !Bool              -- ^ Whether to propagate txs, ssc data, blocks to neighbors
    , npReportServers :: ![Text]            -- ^ List of report server URLs
    , npUpdateParams  :: !UpdateParams      -- ^ Params for update system
    , npUseNTP        :: !Bool
    , npBackpressure  :: !Backpressure
    } deriving (Show)

data Backpressure = Backpressure
    { -- | Threshold and delay for the first level. When the estimate exceeds
      --   the threshold, delay for this interval.
      bpressLevelOne :: (Word32, Microsecond)
      -- | Threshold and delay for the second level. When the estimate exceeds
      --   the threshold, delay for this interval.
      --   If this threshold is less than the level one threshold, then the
      --   level two delay will never be induced.
    , bpressLevelTwo :: (Word32, Microsecond)
    }
    deriving Show

noBackpressure :: Backpressure
noBackpressure = Backpressure
    { bpressLevelOne = (maxBound, 0)
    , bpressLevelTwo = (maxBound, 0)
    }

parseBackpressure :: P.Parser Backpressure
parseBackpressure = do
    lowerThreshold <- P.decimal
    _ <- P.char ' '
    lowerDelay <- P.decimal
    _ <- P.char ' '
    upperThreshold <- P.decimal
    _ <- P.char ' '
    upperDelay <- P.decimal
    pure $ Backpressure
        { bpressLevelOne = (lowerThreshold, lowerDelay)
        , bpressLevelTwo = (upperThreshold, upperDelay)
        }
