module Explorer.I18n.Types where

-- Add all i18n types here to generate lenses from it

type Translation =
    { common :: Common
    , navigation :: Nav
    , hero :: Hero
    , dashboard :: Dashboard
    , notfound :: NotFound
    , address :: Address
    , tx :: Transaction
    , block :: Block
    , genesisBlock :: GenesisBlock
    , footer :: Footer
    }

-- common translations

type Common =
    { cBack :: String
    , cADA :: String
    , cBCshort :: String
    , cBCong :: String
    , cApi :: String
    , cTransaction :: String
    , cTransactions :: String
    , cTransactionFeed :: String
    , cCalculator :: String
    , cNetwork :: String
    , cVersion :: String
    , cAddress :: String
    , cAddresses :: String
    , cSummary :: String
    , cBlock :: String
    , cGenesis :: String
    , cHash :: String
    , cHashes :: String
    , cEpoch :: String
    , cEpochs :: String
    , cSlot :: String
    , cSlots :: String
    , cAge :: String
    , cTotalSent :: String
    , cBlockLead :: String
    , cSize :: String
    , cExpand :: String
    , cCollapse :: String
    , cNoData :: String
    , cTitle :: String
    , cCopyright :: String
    , cUnknown :: String
    , cTotalOutput :: String
    , cOf :: String
    , cNotAvailable :: String
    , cLoading :: String
    , cBack2Dashboard :: String
    , cYes :: String
    , cNo :: String
    , cDays :: String
    , cHours :: String
    , cMinutes :: String
    , cSeconds :: String
    , cDateFormat :: String
    , cDecimalSeparator :: String
    , cGroupSeparator :: String
    }

-- translations of main navigation

type Nav =
    { navHome :: String
    , navBlockchain :: String
    , navMarket :: String
    , navCharts :: String
    , navTools :: String
    }

-- translations of hero

type Hero =
    { hrSubtitle :: String
    , hrSearch :: String
    , hrTime :: String
    }

-- translations of dashboard

type Dashboard =
    { dbTitle :: String
    , dbLastBlocks :: String
    , dbLastBlocksDescription :: String
    , dbPriceAverage :: String
    , dbPriceForOne :: String
    , dbPriceSince :: String
    , dbTotalSupply :: String
    , dbTotalAmountOf :: String
    , dbTotalAmountOfTransactions :: String
    , dbExploreBlocks :: String
    , dbExploreTransactions :: String
    , dbBlockchainOffer :: String
    , dbBlockSearch :: String
    , dbBlockSearchDescription :: String
    , dbAddressSearch :: String
    , dbAddressSearchDescription :: String
    , dbTransactionSearch :: String
    , dbTransactionSearchDescription :: String
    , dbApiDescription :: String
    , dbGetAddress :: String
    , dbResponse :: String
    , dbCurl :: String
    , dbNode :: String
    , dbJQuery :: String
    , dbGetApiKey :: String
    , dbMoreExamples :: String
    , dbAboutBlockchain :: String
    , dbAboutBlockchainDescription :: String
    }

-- translations of address detail page

type Address =
    { addScan :: String
    , addQrCode :: String
    , addFinalBalance :: String
    , addNotFound :: String
    }

-- translations of transaction detail page

type Transaction =
    { txTime :: String
    , txIncluded :: String
    , txRelayed :: String
    , txEmpty :: String
    , txFees :: String
    , txNotFound :: String
    }

-- translations of block detail page

type Block =
    { blFees :: String
    , blEstVolume :: String
    , blPrevBlock :: String
    , blNextBlock :: String
    , blRoot :: String
    , blEpochSlotNotFound :: String
    , blSlotNotFound :: String
    , blSlotEmpty :: String
    }

-- translations of block detail page

type GenesisBlock =
    { gblNotFound :: String
    , gblNumberRedeemedAddresses :: String
    , gblNumberNotRedeemedAddresses :: String
    , gblNumberAddressesToRedeem :: String
    , gblRedeemedAmountTotal :: String
    , gblNonRedeemedAmountTotal :: String
    , gblFilterAll :: String
    , gblFilterRedeemed :: String
    , gblFilterNonRedeemed :: String
    , gblAddressesNotFound :: String
    , gblAddressesError :: String
    , gblAddressRedeemAmount :: String
    , gblAddressIsRedeemed :: String
    }

-- translations of footer

type Footer =
    { fooIohkSupportP :: String
    , fooGithub :: String
    , fooEmail :: String
    , fooTwitter :: String
    , fooDaedalusPlatform :: String
    , fooWhyCardano :: String
    , fooCardanoRoadmap :: String
    , fooCardanoSource :: String
    , fooCardanoDocumentation :: String
    , fooCardanoFoundation :: String
    , fooCardanoFoundationYoutube :: String
    , fooCardanoFoundationTwitter :: String
    , fooCardanoHub :: String
    , fooCardanoChat :: String
    , fooCardanoForum :: String
    , fooCardanoReddit :: String
    , fooCardanoCommunity :: String
    , fooCardanoTestnet :: String
    , fooCardanoOpenSource :: String
    , fooIOHK :: String
    , fooIOHKBlog :: String
    , fooIOHKYoutube :: String
    , fooDisclaimerPt1 :: String
    , fooDisclaimerPt2 :: String
    , fooProject :: String
    , fooProtocol :: String
    , fooFoundation :: String
    , fooLearnMore :: String
    }

-- translations of 404

type NotFound =
    { nfTitle :: String
    , nfDescription :: String
    }
