-- | Descriptions for each endpoint, for Swagger-documentation.

module Description where

import           Universum

blocksLastDescription :: Text
blocksLastDescription = "Get the list of last blocks entries."

blocksTotalNumberDescription :: Text
blocksTotalNumberDescription = "Get the total number of blocks/slots currently available. Total number of main blocks is equal to the difficulty of the topmost (tip) header. Total number of anchor blocks is equal to the current epoch's index + 1."

blocksPagesDescription :: Text
blocksPagesDescription = "Get the list of blocks, contained in pages."

blocksPagesTotalDescription :: Text
blocksPagesTotalDescription = "Get the list of total pages."

blocksSummaryDescription :: Text
blocksSummaryDescription = "Get block's summary information."

blocksTxsDescription :: Text
blocksTxsDescription = "Get brief information about transactions."

txsLastDescription :: Text
txsLastDescription = "Get information about the N latest transactions."

txsSummaryDescription :: Text
txsSummaryDescription = "Get summary information about a transaction."

addressSummaryDescription :: Text
addressSummaryDescription = "Get summary information about an address."

epochPagesDescription :: Text
epochPagesDescription = "Get epoch pages, all the paged slots in the epoch."

epochSlotsDescription :: Text
epochSlotsDescription = "Get the slot information in an epoch."
