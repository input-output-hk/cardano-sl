-- Python Scraper Schema
drop database if exists explorer_python_api;
create database explorer_python_api;
\connect explorer_python_api;
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
create index i_blocks_cbeBlkHash on scraper.blocks (cbeBlkHash asc);

create table scraper.tx (
                        ctsId               text      PRIMARY KEY
                      , ctsTxTimeIssued     timestamp without time zone
                      , ctsBlockTimeIssued  timestamp without time zone
                      , ctsBlockHash        text
                      , ctsTotalInput       bigint
                      , ctsTotalOutput      bigint
                      , ctsFees             bigint
                      );
create index i_tx_ctsId on scraper.tx (ctsId asc);
create index i_tx_ctsTxTimeIssued on scraper.tx (ctsTxTimeIssued asc);

create table scraper.txinput (
                        ctsId               text
                      , ctsIdIndex          smallint
                      , ctsTxTimeIssued     timestamp without time zone
                      , ctsInputAddr        text
                      , ctsInput            bigint
                      , constraint pk_txinput primary key (ctsId, ctsIdIndex)
                      );
create index i_txinput_ctsId on scraper.txinput (ctsId asc);
create index i_txinput_ctsIdIndex on scraper.txinput (ctsIdIndex asc);
create index i_txinput_ctsTxTimeIssued on scraper.txinput (ctsTxTimeIssued asc);
create index i_txinput_ctsInputAddr_ctsId on scraper.txinput (ctsInputAddr asc, ctsId asc);

create table scraper.txoutput (
                        ctsId               text
                      , ctsIdIndex          smallint
                      , ctsTxTimeIssued     timestamp without time zone
                      , ctsOutputAddr       text
                      , ctsOutput           bigint
                      , constraint pk_txoutput primary key (ctsId, ctsIdIndex)
                      );
create index i_txoutput_ctsId on scraper.txoutput (ctsId asc);
create index i_txoutput_ctsIdIndex on scraper.txoutput (ctsIdIndex asc);
create index i_txoutput_ctsTxTimeIssued on scraper.txoutput (ctsTxTimeIssued asc);
create index i_txoutput_ctsOutputAddr_ctsId on scraper.txoutput (ctsOutputAddr asc, ctsId asc);

drop user if exists explorer_python_api;
create user explorer_python_api;
grant all privileges on database explorer_python_api to explorer_python_api;
grant all privileges on schema scraper to explorer_python_api;
grant all privileges on all tables in schema scraper to explorer_python_api;
