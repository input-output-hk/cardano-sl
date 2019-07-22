-- Python Scraper Schema

create schema scraper;

create table scraper.blocks (
                        cbeBlkHash        text      PRIMARY KEY
                      , cbeEpoch          smallint
                      , cbeSlot           smallint
                      , cbeBlkHeight      integer
                      , cbeTimeIssued     timestamp without time zone
                      , cbeTxNum          integer
                      , cbeTotalSent      bigint
                      , cbeSize           integer
                      , cbeBlockLead      text
                      , cbeFees           bigint
                      , cbsPrevHash       text
                      , cbsNextHash       text
                      , cbsMerkleRoot     text
                      );


create table scraper.tx (
                        ctsId               text      PRIMARY KEY
                      , ctsTxTimeIssued     timestamp without time zone
                      , ctsBlockTimeIssued  timestamp without time zone
                      , ctsBlockHash        text
                      , ctsTotalInput       bigint
                      , ctsTotalOutput      bigint
                      , ctsFees             bigint
                      );


create table scraper.txinput (
                        ctsId               text
                      , ctsIdIndex          smallint
                      , ctsInputAddr        text
                      , ctsInput            bigint
                      , constraint pk_txinput primary key (ctsId, ctsIdIndex)
                      );


create table scraper.txoutput (
                        ctsId               text
                      , ctsIdIndex          smallint
                      , ctsOutputAddr       text
                      , ctsOutput           bigint
                      , constraint pk_txoutput primary key (ctsId, ctsIdIndex)
                      );
