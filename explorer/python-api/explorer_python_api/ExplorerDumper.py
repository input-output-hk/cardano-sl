import logging
import string
import requests
from requests.exceptions import ConnectionError
from datetime import datetime
from prometheus_client import Gauge
from explorer_python_api.explorer import ExplorerClient
from explorer_python_api import db
import time
from contextlib import contextmanager

class ExplorerDumper:
    """"Explorer Dumper"""

    processing = False
    resyncRequest = False
    resyncFromEpoch = 0
    blockHeight = 0
    epochSlots = 21600
    addrMaxLen = 200

    def __init__(self, logger, metrics_registry, dbc, url):
        self.logger = logger
        self.logger.info("Starting dumper")
        self.metrics_registry = metrics_registry
        self.dbc = dbc
        self.logger.info(f'self.dbc = {self.dbc}')
        self.explorerClient = ExplorerClient(logger, url)
        self.blockHeightMetric = Gauge("block_height", "Block Height", registry=metrics_registry)

    def getMetricsRegistry(self):
        return self.metrics_registry

    def dump(self):
        try:
            if not self.processing:
                self.logger.info('Dumping blocks to database')
                self.processing = True

                if self.resyncRequest:
                    db_tip = (self.resyncFromEpoch, -1)
                    self.logger.info(f'Resync requested from epoch: {self.resyncFromEpoch}')
                    self.resyncRequest = False
                    self.resyncFromEpoch = 0
                else:
                    db_tip = db.get_db_tip(self.dbc)

                self.logger.info(f'db_tip: {db_tip}')

                tip = self.getTip()
                if tip == (-1, -1):
                    self.logger.error('  Blockchain tip not found')
                    self.processing = False
                    return False
                self.logger.info(f'tip: {tip}, blockHeight: {self.blockHeight}')

                if db_tip[0] == tip[0] and db_tip[1] == tip[1]:
                    self.logger.debug('  Scraper is already up to date.')
                    self.processing = False
                    return True

                epoch = db_tip[0]

                # Process Full Epochs
                # A full resync of any epoch less than the current epoch will ensure
                # that any rollbacks that occured in a prior epoch but were not
                # captured since the dump is only scraping forward in time get
                # properly updated upon full epoch resync.
                while epoch < tip[0]:
                    validateRange = self.processRangeBlob(epoch, (0, self.epochSlots))
                    self.logger.info(f'  Full epoch sync: {epoch}, slot range: (0, {self.epochSlots - 1}), Blocks Requested, Returned, Errors: {validateRange}')
                    epoch = epoch + 1

                # Process the current Epoch
                if epoch == tip[0]:
                    if db_tip[0] < tip[0]:
                        process_slot = -1
                    elif db_tip[0] == tip[0]:
                        process_slot = db_tip[1]
                    else:
                        self.logger.error(f'  The database has recorded blocks from the future')
                        self.processing = False
                        return False

                    # If there is at least 1 new block available, always grab the
                    # last 2 blocks so that the missing cbsNextHash can be populated
                    # in the pre-existing db_tip
                    if process_slot > -1:
                        db_tip_offset = 0
                    else:
                        db_tip_offset = 1
                    validateRange = self.processRangeBlob(epoch, (process_slot + db_tip_offset, tip[1] + 1))

                    self.logger.info(f'  Partial epoch sync: {epoch}, slot range: ({process_slot + db_tip_offset}, {tip[1]}), Blocks Requested, Returned, Errors: {validateRange}')

                db_final_tip = db.get_db_tip(self.dbc)
                self.logger.info(f'db_final_tip: {db_final_tip}')

                self.blockHeightMetric.set(self.blockHeight)
                self.logger.info("Finished dumping blocks to database")
                self.processing = False
                return True
            else:
                self.logger.error("Skipping processing because last dump is processing")
        except Exception as e:
            self.logger.exception(e)
            self.logger.error("An error has occurred in dumping blocks to database!")
            self.processing = False
            return False

    def getTip(self):
        result = self.explorerClient.page()
        if 'ok' not in result:
            raise BaseException(f'Failed to call explorer! Cannot proceed with scraping: {result}')
        # Eliminate an empty page
        if result['ok']:
            tip = ((result['ok'][1][0]['cbeEpoch'], result['ok'][1][0]['cbeSlot']))
            self.blockHeight = result['ok'][1][0]['cbeBlkHeight']
        else:
            self.logger.error(f'Blockchain tip not found!')
            tip = (-1,-1)
        return tip

    def getTotalPages(self):
        result = self.explorerClient.total_pages()
        if 'ok' not in result:
            raise BaseException(f'Failed to call explorer! Cannot proceed with scraping: {result}')
        pages = result['ok']
        self.logger.debug(f'Current total pages: {pages}')
        return pages

    def getSlot(self, epoch, slot, scan=False):
        result = self.explorerClient.slot(epoch, slot)
        if 'ok' not in result and not scan:
            raise BaseException(f'Failed to call explorer! Cannot proceed with scraping: {result}')
        if not scan:
            slotdata = result['ok']
        else:
            slotdata = result
        self.logger.debug(f'Slotdata: {slotdata}')
        return slotdata

    def getRange(self, startHash, endHash):
        result = self.explorerClient.range(startHash, endHash)
        if 'ok' not in result:
            raise BaseException(f'Failed to call explorer! Cannot proceed with scraping: {result}')
        rangedata = result['ok']
        return rangedata

    def validateRange(self, startHashTuple, endHashTuple, rangedata):
        # The startHashTuple and endHashTuple parameters were already
        # pre-validated as successful endpoints in the processRangeBlob function
        startBlock = startHashTuple[2]
        epoch = startHashTuple[0]
        endBlock = endHashTuple[2]
        prevBlock = ""
        currBlock = startBlock
        nextBlock = ""
        blockCount = 0
        blockError = 0
        startBlockHeight = rangedata['cbrBlocks'][0]['cbsEntry']['cbeBlkHeight']
        endBlockHeight = rangedata['cbrBlocks'][-1]['cbsEntry']['cbeBlkHeight']
        targetBlockCount = endBlockHeight - startBlockHeight + 1
        for block in rangedata['cbrBlocks']:
            if currBlock != block['cbsEntry']['cbeBlkHash']:
                blockError = blockError + 1
            if prevBlock != block['cbsPrevHash']:
                if block['cbsEntry']['cbeBlkHash'] != startBlock:
                    blockError = blockError + 1
            blockCount = blockCount + 1
            currBlock = block['cbsNextHash']
            prevBlock = block['cbsEntry']['cbeBlkHash']
        if targetBlockCount != blockCount:
            blockError = blockError + 1
        if blockError > 0:
            self.logger.error(f'For Epoch {epoch}: Blocks checked: {blockCount}; Target Count: {targetBlockCount}; Block Errors: {blockError}')
            self.logger.error("  One possible reason for an error here may be that a rollback occurred.")
            self.logger.error("  In this case, the rollback will be corrected automatically in the database at the end of the epoch.")
            self.logger.error("  You can also force a resync of the erroring epoch manually and check if the error is corrected:")
            self.logger.error("    /api/resync/from/epoch/<epoch_reporting_an_error>")
        return((blockCount, targetBlockCount, blockError))

    def getBlockWrite(self, rangedata):
        blockwrite = []
        for block in rangedata['cbrBlocks']:
            blocktemp = {}
            blocktemp['cbeBlkHash'] = block['cbsEntry']['cbeBlkHash']
            blocktemp['cbeEpoch'] = block['cbsEntry']['cbeEpoch']
            blocktemp['cbeSlot'] = block['cbsEntry']['cbeSlot']
            blocktemp['cbeBlkHeight'] = block['cbsEntry']['cbeBlkHeight']
            blocktemp['cbeTimeIssued'] = block['cbsEntry']['cbeTimeIssued']
            blocktemp['cbeTxNum'] = block['cbsEntry']['cbeTxNum']
            blocktemp['cbeTotalSent'] = block['cbsEntry']['cbeTotalSent']['getCoin']
            blocktemp['cbeSize'] = block['cbsEntry']['cbeSize']
            blocktemp['cbeBlockLead'] = block['cbsEntry']['cbeBlockLead']
            blocktemp['cbeFees'] = block['cbsEntry']['cbeFees']['getCoin']
            blocktemp['cbsPrevHash'] = block['cbsPrevHash']
            blocktemp['cbsNextHash'] = block['cbsNextHash']
            blocktemp['cbsMerkleRoot'] = block['cbsMerkleRoot']
            blockwrite.append(blocktemp)
        for i in range(len(blockwrite)):
            blockwrite[i]['cbeTimeIssued'] = datetime.utcfromtimestamp(blockwrite[i]['cbeTimeIssued'])
        return(blockwrite)

    def getTxWrite(self, rangedata):
        txwrite = []
        for tx in rangedata['cbrTransactions']:
            txtemp = {}
            txtemp['ctsId'] = tx['ctsId']
            txtemp['ctsTxTimeIssued'] = tx['ctsTxTimeIssued']
            txtemp['ctsBlockTimeIssued'] = tx['ctsBlockTimeIssued']
            txtemp['ctsBlockHash'] = tx['ctsBlockHash']
            txtemp['ctsTotalInput'] = tx['ctsTotalInput']['getCoin']
            txtemp['ctsTotalOutput'] = tx['ctsTotalOutput']['getCoin']
            txtemp['ctsFees'] = tx['ctsFees']['getCoin']
            txwrite.append(txtemp)
        for i in range(len(txwrite)):
            txwrite[i]['ctsTxTimeIssued'] = datetime.utcfromtimestamp(txwrite[i]['ctsTxTimeIssued'])
            txwrite[i]['ctsBlockTimeIssued'] = datetime.utcfromtimestamp(txwrite[i]['ctsBlockTimeIssued'])
        return(txwrite)

    def getTxInputWrite(self, rangedata):
        txinputwrite = []
        for tx in rangedata['cbrTransactions']:
            for i in range(len(tx['ctsInputs'])):
                txinputtemp = {}
                address = tx['ctsInputs'][i][0][:self.addrMaxLen];
                txinput = tx['ctsInputs'][i][1]['getCoin'];
                txinputtemp['ctsId'] = tx['ctsId']
                txinputtemp['ctsIdIndex'] = i
                txinputtemp['ctsTxTimeIssued'] = tx['ctsTxTimeIssued']
                txinputtemp['ctsInputAddr'] = address
                txinputtemp['ctsInput'] = txinput
                txinputwrite.append(txinputtemp)
        for i in range(len(txinputwrite)):
            txinputwrite[i]['ctsTxTimeIssued'] = datetime.utcfromtimestamp(txinputwrite[i]['ctsTxTimeIssued'])
        return(txinputwrite)

    def getTxOutputWrite(self, rangedata):
        txoutputwrite = []
        for tx in rangedata['cbrTransactions']:
            for i in range(len(tx['ctsOutputs'])):
                txoutputtemp = {}
                address = tx['ctsOutputs'][i][0][:self.addrMaxLen];
                txoutput = tx['ctsOutputs'][i][1]['getCoin'];
                txoutputtemp['ctsId'] = tx['ctsId']
                txoutputtemp['ctsIdIndex'] = i
                txoutputtemp['ctsTxTimeIssued'] = tx['ctsTxTimeIssued']
                txoutputtemp['ctsOutputAddr'] = address
                txoutputtemp['ctsOutput'] = txoutput
                txoutputwrite.append(txoutputtemp)
        for i in range(len(txoutputwrite)):
            txoutputwrite[i]['ctsTxTimeIssued'] = datetime.utcfromtimestamp(txoutputwrite[i]['ctsTxTimeIssued'])
        return(txoutputwrite)

    def getRangeBlockhash(self, epoch, rangepoints, mode):
        if mode == "epochBeginHash":
            scanRange = range(rangepoints[0], rangepoints[1])
        elif mode == "epochEndHash":
            scanRange = range(rangepoints[1] - 1, rangepoints[0] - 1, -1)
        else:
            self.logger.error(f'  getRangeBlockhash fn: Bad mode value')
            return((epoch, None, None, "Bad mode value"))
        for i in scanRange:
            blockCheck = self.getSlot(epoch,i,scan=True)
            # Eliminate empty page and other errors
            if 'ok' in blockCheck:
                # Eliminate an empty slot
                if blockCheck['ok']:
                    blockHash = blockCheck['ok'][0]['cbeBlkHash']
                    # Validate the blockhash is proper type, length, character set
                    if (isinstance(blockHash, str) and len(blockHash) == 64 and
                        all(c in string.hexdigits for c in blockHash)):
                        return((epoch, i, blockHash, "success"))
                    else:
                        self.logger.debug(f'  {epoch}:{i}: Bad blockHash form')
                else:
                    self.logger.debug(f'  {epoch}:{i}: Empty slot')
            else:
                self.logger.debug(f'  {epoch}:{i}: Block request error')
        self.logger.error(f'  {epoch}: No blockHash found in this epoch!')
        return((epoch, None, None, "No blockHash found"))

    @contextmanager
    def transactionBackport(self):
        tx = self.dbc.transaction()
        try:
            yield self.dbc
            tx.commit()
        except BaseException as e:
            tx.rollback()
            self.logger.exception(f'Database error: {e}, rollback issued.')
            raise e

    def processRangeBlob(self, epoch, rangepoints):
        with self.transactionBackport() as conn:
            startHashTuple = self.getRangeBlockhash(epoch, rangepoints, mode="epochBeginHash")
            endHashTuple = self.getRangeBlockhash(epoch, rangepoints, mode="epochEndHash")

            if startHashTuple[3] == "success" and endHashTuple[3] == "success":
                startBlock = startHashTuple[2]
                endBlock = endHashTuple[2]
                self.logger.info(f'  Epoch {epoch}, slot range: ({startHashTuple[1]}, {endHashTuple[1]})')
            else:
                self.logger.error(f'  Unable to find suitable start and/or end blockHashes for epoch {epoch}')
                return False

            p1 = time.time()
            # Get the ranged data from the Cardano range API
            rangedata = self.getRange(startBlock, endBlock)

            p2 = time.time()
            validateRange = self.validateRange(startHashTuple, endHashTuple, rangedata)

            # Process data for blocks and write to table blocks
            p3 = time.time()
            blockwrite = self.getBlockWrite(rangedata)

            p4 = time.time()
            db.commit_blockwrite(conn, blockwrite)

            # Process data for txs and write to table tx
            p5 = time.time()
            txwrite = self.getTxWrite(rangedata)
            db.commit_txwrite(conn, txwrite)

            # Process data for tx inputs and write to table txinput
            txinputwrite = self.getTxInputWrite(rangedata)
            db.commit_txinputwrite(conn, txinputwrite)

            # Process data for tx outputs and write to table txoutput
            txoutputwrite = self.getTxOutputWrite(rangedata)
            db.commit_txoutputwrite(conn, txoutputwrite)

            p6 = time.time()

        self.logger.info(f'    Timers: [1 {(p2-p1):.1f}] [2 {(p3-p2):.1f}] [3 {(p4-p3):.1f}] [4 {(p5-p4):.1f}] [5 {(p6-p5):.1f}] [Total: {(p6-p1):.1f}]')
        return(validateRange)
