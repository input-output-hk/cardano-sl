import threading
import atexit
import json
import logging
import os
import records
import requests
import pytz
import time
from flask import Flask
from flask.logging import default_handler
from prometheus_flask_exporter import PrometheusMetrics
from prometheus_client import CollectorRegistry
from explorer_python_api.ExplorerDumper import ExplorerDumper
from explorer_python_api import db

POLL_TIME = 20 # Slot duration

# variables that are accessible from anywhere
commonDataStruct = {}
# lock to control access to variable
dataLock = threading.Lock()
# thread handler
explorerDumperThread = threading.Thread()
explorerDumper = False
logger_name = __name__
logger = logging.getLogger(__name__)
logger.addHandler(default_handler)

def create_app():
    app = Flask(__name__)
    gunicorn_logger = logging.getLogger('gunicorn.error')
    app.logger.handlers = gunicorn_logger.handlers
    app.logger.setLevel(gunicorn_logger.level)

    metrics_registry = CollectorRegistry()
    metrics = PrometheusMetrics(app, registry=metrics_registry)

    @app.route('/api/addresses/summary/<address>')
    def address_summary(address):
        app.logger.info("Address summary page accessed")
        return json.dumps(getAddressSummary(address))

    @app.route('/api/resync/from/epoch/<epoch>')
    def resync_epochs(epoch):
        global dbc
        app.logger.info(f'Scraper Resync requested from epoch {epoch}')
        try:
            epoch = int(epoch)
        except:
            app.logger.info("The epoch provided must be a positive integer")
            return json.dumps("The epoch provided must be a positive integer")
        currentEpoch = explorerDumper.getTip()[0]
        if currentEpoch == -1:
            app.logger.info(f'Blockchain tip not found!')
            return json.dumps(f'Blockchain tip not found!')
        elif epoch < 0 or epoch > currentEpoch:
            app.logger.info(f'The epoch provided must be greater or equal to 0 and less than or equal to the current epoch: {currentEpoch}')
            return json.dumps(f'The epoch provided must be greater or equal to 0 and less than or equal to the current epoch: {currentEpoch}')
        else:
            app.logger.info(f'Resync request submitted.  Check the logs for confirmation of resync.')
            explorerDumper.resyncRequest = True
            explorerDumper.resyncFromEpoch = epoch
            return json.dumps(f'Resync request submitted.  Check the logs for confirmation of resync.')

    @app.route('/', defaults={'u_path': ''})
    @app.route('/<path:u_path>')
    def explorer_proxy(u_path):
        global explorer_url
        app.logger.info(f'API proxied to Explorer: {u_path}')
        return requests.get(f'{explorer_url}/{u_path}').content

    def getAddressSummary(caAddress):
        global dbc

        with explorerDumper.transactionBackport() as conn:
            # p1 = time.time()
            caTxSent = db.caTxSentDistinct(dbc, caAddress)
            # p2 = time.time()
            # print(f'1: {p2-p1:.1f}')
            caTxReceived = db.caTxReceivedDistinct(dbc, caAddress)
            # p3 = time.time()
            # print(f'2: {p3-p2:.1f}')
            caTxSentRecord = db.caTxSentRecord(dbc, caAddress)
            # p4 = time.time()
            # print(f'3: {p4-p3:.1f}')
            caTxReceivedRecord = db.caTxReceivedRecord(dbc, caAddress)
            # p5 = time.time()
            # print(f'4: {p5-p4:.1f}')
            caTxRecord = db.caTxRecord(dbc, caAddress)
            # p6 = time.time()
            # print(f'5: {p6-p5:.1f}')
            caTxNum = len(caTxRecord)

        # These address metrics can be logged prior to any heavy processing
        # Seeing these logs may help explain any timeouts
        app.logger.info('')
        app.logger.info(f'caAddress: {caAddress}')
        app.logger.info(f'caTxNum: {caTxNum}')
        app.logger.info(f'caTxSent: {caTxSent}')
        app.logger.info(f'caTxReceived: {caTxReceived}')
        app.logger.info(f'len(caTxSentRecord): {len(caTxSentRecord)}')
        app.logger.info(f'len(caTxReceivedRecord): {len(caTxReceivedRecord)}')
        txs = []
        caBalanceInput = 0
        caBalanceOutput = 0
        caBalanceFee = 0
        ctbSelfInputSum = 0
        ctbSelfOutputSum = 0
        txLoopCount = 0
        inputRecord = 0
        outputRecord = 0
        for tx in caTxRecord:
            txtemp = {}
            txtempinput = []
            txtempinputs = []
            txtempoutput = []
            txtempoutputs = []
            ctbInputSum = 0
            ctbOutputSum = 0
            txtemp['ctbId'] = tx['ctsid']
            txtemp['ctbTimeIssued'] = int(tx['ctstxtimeissued'].replace(tzinfo=pytz.utc).timestamp())
            while inputRecord < len(caTxSentRecord) and caTxSentRecord[inputRecord]['ctsid'] == tx['ctsid']:
                txinput = caTxSentRecord[inputRecord]
                # if inputRecord % 10000 == 0:
                #     print(f'inputRecord = {inputRecord}')
                txtempinput.append(txinput['ctsinputaddr'])
                txtempinput.append({ "getCoin": str(txinput['ctsinput']) })
                ctbInputSum = ctbInputSum + txinput['ctsinput']
                if txinput['ctsinputaddr'] == caAddress:
                    ctbSelfInputSum = ctbSelfInputSum + txinput['ctsinput']
                txtempinputs.append(txtempinput)
                txtempinput = []
                inputRecord = inputRecord + 1
            while outputRecord < len(caTxReceivedRecord) and caTxReceivedRecord[outputRecord]['ctsid'] == tx['ctsid']:
                txoutput = caTxReceivedRecord[outputRecord]
                # if outputRecord % 10000 == 0:
                #     print(f'outputRecord = {outputRecord}')
                txtempoutput.append(txoutput['ctsoutputaddr'])
                txtempoutput.append({ "getCoin": str(txoutput['ctsoutput']) })
                ctbOutputSum = ctbOutputSum + txoutput['ctsoutput']
                if txoutput['ctsoutputaddr'] == caAddress:
                    ctbSelfOutputSum = ctbSelfOutputSum + txoutput['ctsoutput']
                txtempoutputs.append(txtempoutput)
                txtempoutput = []
                outputRecord = outputRecord + 1
            txtemp['ctbInputs'] = txtempinputs
            txtemp['ctbOutputs'] = txtempoutputs
            txtemp['ctbInputSum'] = { "getCoin": str(ctbInputSum) }
            txtemp['ctbOutputSum'] = { "getCoin": str(ctbOutputSum) }
            txtemp['ctsFees'] = { "getCoin": str(tx['ctsfees']) }
            caBalanceFee = caBalanceFee + tx['ctsfees']
            caBalanceInput = caBalanceInput + ctbInputSum
            caBalanceOutput = caBalanceOutput + ctbOutputSum
            txs.append(txtemp)
            txLoopCount = txLoopCount + 1
            # if txLoopCount % 1000 == 0:
            #     print(f'txLoopCount = {txLoopCount}')
            # if txLoopCount > 10:
            #     break
        caBalance = ctbSelfOutputSum - ctbSelfInputSum

        addressReport = { "Right": {
            "caAddress": caAddress,
            "caType": "CPubKeyAddress",
            "caTxNum": caTxNum,
            "caBalance": { "getCoin": str(caBalance) },
            "caTotalInput": { "getCoin": str(ctbSelfInputSum) },
            "caTotalOutput": { "getCoin": str(ctbSelfOutputSum) },
            "caTotalFee": { "getCoin": str(caBalanceFee) },
            "caTxList": txs
            }
        }
        app.logger.info(f'caBalance: {caBalance}')
        app.logger.info(f'caTotalInput: {ctbSelfInputSum}'),
        app.logger.info(f'caTotalOutput: {ctbSelfOutputSum}'),
        app.logger.info(f'caTotalFee: {caBalanceFee}'),
        app.logger.info('')
        return addressReport

    def interrupt():
        global explorerDumperThread
        explorerDumperThread.cancel()

    def explorerDumperRun():
        global commonDataStruct
        global explorerDumperThread
        global explorerDumper
        with dataLock:
            # Do your stuff with commonDataStruct Here
            app.logger.info("")
            app.logger.info("Preparing to run dumper")
            explorerDumper.dump()
            app.logger.info(f'Dump completed.  Restarting in {POLL_TIME} seconds.')

            # Set the next thread to happen
            explorerDumperThread = threading.Timer(POLL_TIME, explorerDumperRun, ())
            explorerDumperThread.start()

    def explorerDumperInit():
        # Do initialisation stuff here
        # TODO: wire in logger here
        global dbc
        global explorerDumper
        global logger_name
        global explorer_url
        dbuser = os.environ.get('DBUSER', 'explorer_python_api')
        dbname = os.environ.get('DBNAME', 'explorer_python_api')
        explorer_url = os.environ.get('EXPLORERURL', 'http://localhost:8100')
        dbsockpath = os.environ.get('DBSOCKPATH', '/run/postgresql')
        # Postgres with Ident and Socket (ex: Nix deploy)
        dbc = records.Database(f'postgres:///{dbname}?&host={dbsockpath}')
        # Postgres without Ident and Socket (ex: Nix deploy)
        # dbc = records.Database(f'postgres:///{dbname}?user={dbuser}&host={dbsockpath}')
        # Postgres without socket spec (ex: Docker)
        # dbc = records.Database(f'postgres://localhost:5432/{dbname}?user=postgres&sslmode=disable')
        app.logger.info(f'Starting Explorer Dumper in {POLL_TIME} seconds.')

        explorerDumper = ExplorerDumper('gunicorn.error', metrics_registry, dbc, explorer_url)

        global explorerDumperThread
        # Create your thread
        explorerDumperThread = threading.Timer(POLL_TIME, explorerDumperRun, ())
        explorerDumperThread.start()

    # Initiate
    explorerDumperInit()
    # When you kill Flask (SIGTERM), clear the trigger for the next thread
    atexit.register(interrupt)
    return app

app = create_app()
