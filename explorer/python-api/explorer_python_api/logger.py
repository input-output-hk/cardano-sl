from datetime import datetime

LEVELS = ['trace', 'debug', 'info', 'warn', 'severe']


class Logger():
    __slots__ = ['logger', 'level']

    # def __init__(self, logger='', level='debug'):
    def __init__(self, logger='', level='debug'):
        self.level = LEVELS.index(level)
        self.logger = logger

    def severe(self, text, logger='', report=None):
        return self.log(text, logger, level='severe')

    def warn(self, text, logger='', report=None):
        return self.log(text, logger, level='warn')

    def debug(self, text, logger='', report=None):
        return self.log(text, logger, level='debug')

    def trace(self, text, logger='', report=None):
        return self.log(text, logger, level='trace')

    def info(self, text, logger='', report=None):
        return self.log(text, logger)

    def log(self, text, logger='', level='info', report=None):
        if level not in LEVELS:
            raise BaseException(f'Illegal level: "{level}"! Expected one of: {LEVELS}')
        if LEVELS.index(level) < self.level:
            return 'ignored'
        logg = f'{self.logger}.{logger}' if (self.logger and logger) else (self.logger or logger)
        if not logg:
            raise BaseException('Logger required!')
        print(f"[{datetime.utcnow()}][{level.upper()}][{logg}] {text}", flush=True)
        return 'ok'
