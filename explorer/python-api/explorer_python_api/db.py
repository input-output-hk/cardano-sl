import records

def get_db_tip(DB):
    db_tip = DB.query('''select cbeEpoch, cbeSlot from scraper.blocks order by cbeEpoch DESC, cbeSlot DESC limit 1''').first()
    if db_tip is None:
        db_tip = (0, -1)
    else:
        db_tip = (db_tip['cbeepoch'], db_tip['cbeslot'])
    return(db_tip)


def commit_blockwrite(DB, blockwrite):
    if len(blockwrite) > 0:
        DB.bulk_query('''
            insert into scraper.blocks (cbeBlkHash, cbeEpoch, cbeSlot, cbeBlkHeight, cbeTimeIssued, cbeTxNum, cbeTotalSent, cbeSize, cbeBlockLead, cbeFees, cbsPrevHash, cbsNextHash, cbsMerkleRoot)
            values (:cbeBlkHash, :cbeEpoch, :cbeSlot, :cbeBlkHeight, :cbeTimeIssued, :cbeTxNum, :cbeTotalSent, :cbeSize, :cbeBlockLead, :cbeFees, :cbsPrevHash, :cbsNextHash, :cbsMerkleRoot)
            on conflict (cbeBlkHash) do update
            set cbeEpoch      = EXCLUDED.cbeEpoch,
                cbeSlot       = EXCLUDED.cbeSlot,
                cbeBlkHeight  = EXCLUDED.cbeBlkHeight,
                cbeTimeIssued = EXCLUDED.cbeTimeIssued,
                cbeTxNum      = EXCLUDED.cbeTxNum,
                cbeTotalSent  = EXCLUDED.cbeTotalSent,
                cbeSize       = EXCLUDED.cbeSize,
                cbeBlockLead  = EXCLUDED.cbeBlockLead,
                cbeFees       = EXCLUDED.cbeFees,
                cbsPrevHash   = EXCLUDED.cbsPrevHash,
                cbsNextHash   = EXCLUDED.cbsNextHash,
                cbsMerkleRoot = EXCLUDED.cbsMerkleRoot
        ''', *blockwrite)
    return


def commit_txwrite(DB, txwrite):
    if len(txwrite) > 0:
        DB.bulk_query('''
            insert into scraper.tx (ctsId, ctsTxTimeIssued, ctsBlockTimeIssued, ctsBlockHash, ctsTotalInput, ctsTotalOutput, ctsFees)
            values (:ctsId, :ctsTxTimeIssued, :ctsBlockTimeIssued, :ctsBlockHash, :ctsTotalInput, :ctsTotalOutput, :ctsFees)
            on conflict (ctsId) do update
            set ctsTxTimeIssued    = EXCLUDED.ctsTxTimeIssued,
                ctsBlockTimeIssued = EXCLUDED.ctsBlockTimeIssued,
                ctsBlockHash       = EXCLUDED.ctsBlockHash,
                ctsTotalInput      = EXCLUDED.ctsTotalInput,
                ctsTotalOutput     = EXCLUDED.ctsTotalOutput,
                ctsFees            = EXCLUDED.ctsFees
        ''', *txwrite)
    return


def commit_txinputwrite(DB, txinputwrite):
    if len(txinputwrite) > 0:
        DB.bulk_query('''
            insert into scraper.txinput (ctsId, ctsIdIndex, ctsTxTimeIssued, ctsInputAddr, ctsInput)
            values (:ctsId, :ctsIdIndex, :ctsTxTimeIssued, :ctsInputAddr, :ctsInput)
            on conflict (ctsId, ctsIdIndex) do update
            set ctsTxTimeIssued = EXCLUDED.ctsTxTimeIssued,
                ctsInputAddr    = EXCLUDED.ctsInputAddr,
                ctsInput        = EXCLUDED.ctsInput
        ''', *txinputwrite)
    return


def commit_txoutputwrite(DB, txoutputwrite):
    if len(txoutputwrite) > 0:
        DB.bulk_query('''
            insert into scraper.txoutput (ctsId, ctsIdIndex, ctsTxTimeIssued, ctsOutputAddr, ctsOutput)
            values (:ctsId, :ctsIdIndex, :ctsTxTimeIssued, :ctsOutputAddr, :ctsOutput)
            on conflict (ctsId, ctsIdIndex) do update
            set ctsTxTimeIssued = EXCLUDED.ctsTxTimeIssued,
                ctsOutputAddr   = EXCLUDED.ctsOutputAddr,
                ctsOutput       = EXCLUDED.ctsOutput
        ''', *txoutputwrite)
    return


def caTxSentDistinct(DB, caAddress):
    caTxSentDistinct = DB.query('''select count(distinct ctsid) from scraper.txinput where ctsinputaddr = :caAddress''', caAddress=caAddress).first()
    caTxSent = caTxSentDistinct['count']
    return(caTxSent)


def caTxReceivedDistinct(DB, caAddress):
    caTxReceivedDistinct = DB.query('''select count(distinct ctsid) from scraper.txoutput where ctsoutputaddr = :caAddress''', caAddress=caAddress).first()
    caTxReceived = caTxReceivedDistinct['count']
    return(caTxReceived)


def caTxSentRecord(DB, caAddress):
    caTxSentRecord = DB.query('''select ctsid, ctsidindex, ctsinputaddr, ctsinput, ctstxtimeissued from scraper.txinput where ctsid in (select ctsid from scraper.txinput where ctsinputaddr = :caAddress UNION ALL select ctsid from scraper.txoutput where ctsoutputaddr = :caAddress) order by ctstxtimeissued ASC, ctsid ASC, ctsidindex ASC''', caAddress=caAddress).all()
    return(caTxSentRecord)


def caTxReceivedRecord(DB, caAddress):
    caTxReceivedRecord = DB.query('''select ctsid, ctsidindex, ctsoutputaddr, ctsoutput, ctstxtimeissued from scraper.txoutput where ctsid in (select ctsid from scraper.txinput where ctsinputaddr = :caAddress UNION ALL select ctsid from scraper.txoutput where ctsoutputaddr = :caAddress) order by ctstxtimeissued ASC, ctsid ASC, ctsidindex ASC''', caAddress=caAddress).all()
    return(caTxReceivedRecord)


def caTxRecord(DB, caAddress):
    caTxRecord = DB.query('''select * from scraper.tx where ctsid in (select ctsid from scraper.txinput where ctsinputaddr = :caAddress UNION ALL select ctsid from scraper.txoutput where ctsoutputaddr = :caAddress) order by ctstxtimeissued ASC, ctsid ASC''', caAddress=caAddress).all()
    return(caTxRecord)
