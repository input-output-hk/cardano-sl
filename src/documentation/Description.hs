-- | Descriptions for each endpoint, for Swagger-documentation.

module Description where

import           Universum

blocksLastDescription
  , blocksTotalNumberDescription
  , blocksPagesDescription
  , blocksPagesTotalDescription
  , blocksSummaryDescription
  , blocksTxsDescription
  , txsLastDescription
  , txsSummaryDescription
  , addressSummaryDescription
  , epochSlotSearchDescription :: Text
blocksLastDescription           = "Get the list of last blocks entries."
blocksTotalNumberDescription    = "Get the total number of blocks/slots currently available. Total number of main blocks is equal to the difficulty of the topmost (tip) header. Total number of anchor blocks is equal to the current epoch's index + 1."
blocksPagesDescription          = "Get the list of blocks, contained in pages."
blocksPagesTotalDescription     = "Get the list of total pages."
blocksSummaryDescription        = "Get block's summary information."
blocksTxsDescription            = "Get brief information about transactions."
txsLastDescription              = "Get information about the N latest transactions."
txsSummaryDescription           = "Get summary information about a transaction."
addressSummaryDescription       = "Get summary information about an address."
epochSlotSearchDescription      = "Search the blocks by epoch and slot. Slot is optional." 
