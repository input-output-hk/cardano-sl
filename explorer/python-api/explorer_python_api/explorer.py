# Credit to Ruslan Dudin and Marcus Gufler for providing the Explorer
# client library this file is based on.

import requests
from requests.exceptions import ConnectionError
import logging
from datetime import datetime

CODE = {
    'NOT_ACCESSIBLE': -1, # can't ping explorer
    'RESPONSE_ERROR': -2, # explorer returned "Left"
    'WRONG_RESPONSE': -3, # some unexpected message in response
    'CALL_EXCEPTION': -4, # python exception while requesting
}

class ExplorerClient():

    def __init__(self, logger, url):
        self.logger = logger
        self.url = url

    def total_pages(self):
        return self.__req_right('blocks/pages/total')

    def slot(self, epoch, slot):
        return self.__req_right(f'epochs/{epoch}/{slot}')

    def range(self, startHash, endHash):
        return self.__req_right(f'blocks/range/{startHash}/{endHash}')

    def page(self):
        return self.__req_right(f'blocks/pages')

    def txs(self, hash):
        return self.__req_right(f'blocks/txs/{hash}?limit=5000')

    def tx(self, hash):
        return self.__req_right(f'txs/summary/{hash}')

    def __req_right(self, path):
        def processor(r):
            if 'Right' in r:
                return {'ok': r['Right']}
            if 'Left' in r:
                return {'error': f'Request returned error! {r}', 'code': CODE['RESPONSE_ERROR'], 'left': r['Left']}
            return {'error': f'Unexpected response format! {r}', 'code': CODE['WRONG_RESPONSE']}
        return self.__req(path, processor)

    def __req(self, path, result_processor):
        try:
            try:
                return result_processor(self.__reget(f'{self.url}/api/{path}'))
            except ConnectionError as e:
                self.__error(f"Explorer is not accessible trying remote! {e}")
                return {'error': msg, 'code': CODE['NOT_ACCESSIBLE']}
        except BaseException as e:
            msg = f'Explorer call has failed with an error! {e}'
            self.__error(msg)
            return {'error': msg, 'code': CODE['CALL_EXCEPTION']}

    def __reget(self, url):
        self.__debug(f'RE: {url}')
        json = requests.get(url).json()
        self.__debug(f'RP: {json}')
        return json

    def __debug(self, msg):
        self.logger.debug(msg)

    def __error(self, msg):
        self.logger.error(msg)
