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
from explorer_python_api import db

# variables that are accessible from anywhere
commonDataStruct = {}

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

    dbuser = os.environ.get('DBUSER', 'explorer_python_api')
    dbname = os.environ.get('DBNAME', 'explorer_python_api')
    dbsockpath = os.environ.get('DBSOCKPATH', '/tmp')
    addr_max_len = os.environ.get('ADDRMAXLEN', '200')
    dbstring = f'postgres:///{dbname}?user={dbuser}&host={dbsockpath}'
    explorer_url = os.environ.get('EXPLORERURL', 'http://localhost:8100')
    dbc = records.Database(dbstring)

    @app.route('/api/addresses/summary/<address>')
    @metrics.do_not_track()
    def address_summary(address):
        app.logger.debug("Address summary page accessed")
        return json.dumps(getAddressSummary(address))

    @app.route('/', defaults={'u_path': ''})
    @app.route('/<path:u_path>')
    def explorer_proxy(u_path):
        app.logger.info(f'API proxied to Explorer: {u_path}')
        return requests.get(f'{explorer_url}/{u_path}').content

    def getAddressSummary(caAddress):
        # p1 = time.time()
        app.logger.info(f'API request for address summary: {caAddress}')
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
        app.logger.debug(f'caAddress: {caAddress}')
        app.logger.debug(f'caTxNum: {caTxNum}')
        app.logger.debug(f'caTxSent: {caTxSent}')
        app.logger.debug(f'caTxReceived: {caTxReceived}')
        app.logger.debug(f'len(caTxSentRecord): {len(caTxSentRecord)}')
        app.logger.debug(f'len(caTxReceivedRecord): {len(caTxReceivedRecord)}')
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
        app.logger.debug(f'caBalance: {caBalance}')
        app.logger.debug(f'caTotalInput: {ctbSelfInputSum}'),
        app.logger.debug(f'caTotalOutput: {ctbSelfOutputSum}'),
        app.logger.debug(f'caTotalFee: {caBalanceFee}'),
        return addressReport
    return app

app = create_app()
