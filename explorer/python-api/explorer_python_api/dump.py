import os
from time import sleep
import logging
import records
from prometheus_client import CollectorRegistry, start_http_server, ProcessCollector
from explorer_python_api.ExplorerDumper import ExplorerDumper


EXPORTER_PORT = 7001
POLL_TIME = 20  # Slot duration


def explorerDumperInit(logger):
    metrics_registry = CollectorRegistry()
    start_http_server(EXPORTER_PORT, registry=metrics_registry)
    dbuser = os.environ.get('DBUSER', 'explorer_python_api')
    dbname = os.environ.get('DBNAME', 'explorer_python_api')
    epoch_slots = os.environ.get('EPOCHSLOTS', '21600')
    addr_max_len = os.environ.get('ADDRMAXLEN', '200')
    explorer_url = os.environ.get('EXPLORERURL', 'http://localhost:8100')
    dbsockpath = os.environ.get('DBSOCKPATH', '/tmp')
    # Postgres with Ident and Socket (ex: Nix deploy)
    # dbstring = f'postgres:///{dbname}?&host={dbsockpath}'
    # Postgres without Ident and Socket (ex: Nix deploy)
    dbstring = f'postgres:///{dbname}?user={dbuser}&host={dbsockpath}'
    # Postgres without socket spec (ex: Docker)
    # dbstring = f'postgres://localhost:5432/{dbname}?user=postgres&sslmode=disable'
    dbc = records.Database(dbstring)
    logger.info('Starting Explorer Dumper in %s seconds.', POLL_TIME)

    explorer_dumper = ExplorerDumper(logger,
                                     metrics_registry, dbc, explorer_url)
    try:
        epoch_slots = int(epoch_slots)
    except Exception as e:
        logger.exception(e)
        logger.info("The EPOCHSLOTS env parameter must be a positive integer: 0 < EPOCHSLOTS <= 21600. 21600 is default.  Please update and restart the service.")
        sleep(20)
        exit(1)
    if epoch_slots < 1 or epoch_slots > 21600:
        logger.info("The EPOCHSLOTS env parameter must be a positive integer: 0 < EPOCHSLOTS <= 21600. 21600 is default.  Please update and restart the service.")
        sleep(20)
        exit(1)
    elif epoch_slots != 60 and epoch_slots != 21600:
        logger.warning(f'EPOCHSLOTS of {epoch_slots} is not a standard deployment parameter.  If this is not intended, adjust the EPOCHSLOTS env parameter and restart the service.')

    logger.info(f'Setting epoch slots to {epoch_slots}')
    explorer_dumper.epochSlots = epoch_slots

    try:
        addr_max_len = int(addr_max_len)
    except Exception as e:
        logger.exception(e)
        logger.info("The ADDRMAXLEN env parameter must be a positive integer: 200 <= ADDRMAXLEN <= 8000. 200 is default.  Please update and restart the service.")
        sleep(20)
        exit(1)
    if addr_max_len < 200 or addr_max_len > 8000:
        logger.info("The ADDRMAXLEN env parameter must be a positive integer: 200 <= ADDRMAXLEN <= 8000. 200 is default.  Please update and restart the service.")
        sleep(20)
        exit(1)

    logger.info(f'Setting address max length to {addr_max_len}.  Larger addresses will be truncated.')
    explorer_dumper.addrMaxLen = addr_max_len
    return explorer_dumper

def runDumper():
    logger = initiateLogger()

    explorer_dumper = explorerDumperInit(logger)
    metrics_registry = explorer_dumper.getMetricsRegistry()
    process_collector = ProcessCollector(registry=metrics_registry)
    while True:
        process_collector.collect()
        logger.info("Preparing to run dumper")
        explorer_dumper.dump()
        logger.info('Dump completed.  Restarting in %s seconds.', POLL_TIME)
        sleep(POLL_TIME)

def initiateLogger():
    # create logger
    logger = logging.getLogger('explorer-postgres-dumper')
    logger.setLevel(logging.DEBUG)

    # create console handler and set level to debug
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)

    # create formatter
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')

    # add formatter to ch
    ch.setFormatter(formatter)

    # add ch to logger
    logger.addHandler(ch)

    # 'application' code
    return logger


if __name__ == "__main__":
    runDumper()
