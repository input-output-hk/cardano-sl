import {assert} from 'chai';
import sinon from 'sinon';
// import Daedalus from '../../dist/Daedalus';
// import ... does not work here, so just use require
const Daedalus = require ('../../dist/Daedalus');

describe('ClientApi', () => {

  let xhr;
  let requests;

  beforeEach(() => {
    requests = [];
    xhr = sinon.useFakeXMLHttpRequest();
    xhr.onCreate = (req) => requests.push(req);
  })

  afterEach(() => xhr.restore())

  describe('getWallets()', () => {

    it('returns a list of wallets', (done) => {
      const response = {
        Right: [{
          cwAddress: "XXX",
          cwAmount: {
            getCoin: 33333
          },
          cwMeta: {
            cwType: "CWTPersonal",
            cwCurrency: "ADA",
            "cwName":""
          }
        },
        { cwAddress: "XXX",
          cwAmount: {
            getCoin: 33333
          },
          cwMeta: {
            cwType: "CWTPersonal",
            cwCurrency: "ADA",
            "cwName":""
          }
        }]
      };

      Daedalus.ClientApi.getWallets()
        .then( (result) => {
          assert.deepEqual(result, response.Right, 'list of wallet data objects');
          done();
        }, (error) => {
          done(error)})
        .catch(done);

      requests[0].respond(200,
        { "Content-Type": "application/json" },
        JSON.stringify(response)
      );
    });

    it('rejects with a JSONDecodingError if server sends invalid json data', (done) => {
      const response = [{ anyOther: "XXX"}];

      Daedalus.ClientApi.getWallets()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'JSONDecodingError', 'includes JSONDecodingError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(response)
      );
    })

    it('rejects with a HTTPStatusError if server response with 400', (done) => {
      Daedalus.ClientApi.getWallets()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'HTTPStatusError', 'HTTPStatusError error message');
            done();
        })
        .catch(done);

      requests[0].respond(400,
        {"Content-Type": "application/json"}
      );
    })

    it('rejects with a ServerError if server response with Left', (done) => {
      const response = {
        Left: "Any error"
      }
      Daedalus.ClientApi.getWallets()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'ServerError', 'ServerError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(response)
      );
    })
  })

  describe('getWallet()', () => {

    it('returns a wallet', (done) => {
      const walletId = 'XXX';
      const response = {
        Right: {
          cwAddress: walletId,
            cwAmount: {
              getCoin: 33333
            },
            cwMeta: {
              cwType: "CWTPersonal",
              cwCurrency: "ADA",
              "cwName":""
            }
          }
        };

      Daedalus.ClientApi.getWallet(walletId)()
        .then( (result) => {
          assert.deepEqual(result, response.Right, 'wallet data object');
          done();
        }, (error) => done(error))
        .catch(done);

      requests[0]
        .respond(200,
          { "Content-Type": "application/json" },
          JSON.stringify(response)
      );
    })

    it('rejects with a JSONDecodingError if server sends invalid json data', (done) => {
      const response = {};

      Daedalus.ClientApi.getWallet('123')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'JSONDecodingError', 'includes JSONDecodingError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(response)
      );
    })

    it('rejects with a HTTPStatusError if server response with 400', (done) => {
      Daedalus.ClientApi.getWallet('not-exist')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'HTTPStatusError', 'HTTPStatusError error message');
            done();
        })
        .catch(done);

      requests[0].respond(400,
        {"Content-Type": "application/json"}
      );
    })

    it('rejects with a ServerError if server response with Left', (done) => {
      const response = {
        Left: "Any error"
      }
      Daedalus.ClientApi.getWallet('')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'ServerError', 'ServerError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(response)
      );
    })
  })

  describe('newWallet()', () => {

    it('returns a new wallet', (done) => {
      const response = {
        Right: {
          cwAddress: '123',
            cwAmount: {
              getCoin: 33333
            },
            cwMeta: {
              cwType: "CWTPersonal",
              cwCurrency: "ADA",
              "cwName":""
            }
          }
        };

      Daedalus.ClientApi.newWallet('CWTPersonal', 'ADA', '')()
        .then( (result) => {
          assert.deepEqual(result, response.Right, 'wallet data object');
          done();
        }, (error) => done(error))
        .catch(done);

      requests[0]
        .respond(200,
          { "Content-Type": "application/json" },
          JSON.stringify(response)
      );
    })

    it('rejects with a JSONDecodingError if server sends invalid json data', (done) => {
      const response = {};

      Daedalus.ClientApi.newWallet('', '', '')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'JSONDecodingError', 'includes JSONDecodingError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(response)
      );
    })

    it('rejects with a HTTPStatusError if server response with 400', (done) => {
      Daedalus.ClientApi.newWallet('')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'HTTPStatusError', 'HTTPStatusError error message');
            done();
        })
        .catch(done);

      requests[0].respond(400,
        {"Content-Type": "application/json"}
      );
    })

    it('rejects with a ServerError if server response with Left', (done) => {
      const response = {
        Left: "Any error"
      }
      Daedalus.ClientApi.newWallet('')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'ServerError', 'ServerError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(response)
      );
    })
  })

  describe('deleteWallet()', () => {

    it('returns empty object', (done) => {
      const response = {
        Right:[]
      };

      Daedalus.ClientApi.deleteWallet('123')()
        .then( (result) => {
          assert.deepEqual(result, {}, '"unit" as a return value');
          done();
        }, (error) => done(error))
        .catch(done);

      requests[0]
        .respond(200,
          { "Content-Type": "application/json" },
          JSON.stringify(response)
      );
    })

    it('returns a HTTPStatusError if server returns "400"', (done) => {

      Daedalus.ClientApi.deleteWallet('not-exist')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'HTTPStatusError',
            'includes HTTPStatusError error message');
            done();
        })
        .catch(done);

      requests[0]
        .respond(400,
          { "Content-Type": "application/json" }
      );
    })

    it('rejects with a ServerError if server response with Left', (done) => {
      const response = { Left: "Any error" }
      Daedalus.ClientApi.deleteWallet('')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'ServerError', 'ServerError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(response)
      );
    })

  })

  describe('send()', () => {

    it('returns a transaction', (done) => {
      const response = {
        Right: {
          ctId: "123",
          ctAmount:{
            getCoin:1000
          },
          ctType:{
            tag:"CTOut",
            contents: {
              ctmCurrency:"ADA",
              ctmTitle:"",
              ctmDescription:"",
              ctmDate:1.483461872037636e9
            }
          }
        }
      };

      Daedalus.ClientApi.send('12', '34', 300)()
        .then( (result) => {
          assert.deepEqual(result, response.Right, 'transaction object');
          done();
        }, (error) => done(error))
        .catch(done);

      requests[0]
        .respond(200,
          { "Content-Type": "application/json" },
          JSON.stringify(response)
      );
    })


    it('rejects with a JSONDecodingError if server sends invalid json data', (done) => {
      const data = {};

      Daedalus.ClientApi.send('', '', '')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'JSONDecodingError', 'includes JSONDecodingError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(data)
      );
    })

    it('rejects with a HTTPStatusError if server response with 400', (done) => {
      Daedalus.ClientApi.send('', '', 0)()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'HTTPStatusError', 'HTTPStatusError error message');
            done();
        })
        .catch(done);

      requests[0].respond(400,
        {"Content-Type": "application/json"}
      );
    })

    it('rejects with a ServerError if server response with Left', (done) => {
      const response = { Left: "Any error" }
      Daedalus.ClientApi.send('A', 'B', 1000)()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'ServerError', 'ServerError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(response)
      );
    })
  })

  describe('getHistory()', () => {

    it('returns a list of transaction objects', (done) => {
      const response = {
        Right: [{
          ctId: "123",
          ctAmount:{
            getCoin:1000
          },
          ctType:{
            tag:"CTOut",
            contents: {
              ctmCurrency:"ADA",
              ctmTitle:"",
              ctmDescription:"",
              ctmDate:1.483461872037636e9
            }
          }
        }]
    };

      Daedalus.ClientApi.getHistory('XXX')()
        .then( (result) => {
          assert.deepEqual(result, response.Right, 'list of transaction objects');
          done();
        }, (error) => done(error))
        .catch(done);

      requests[0]
        .respond(200,
          { "Content-Type": "application/json" },
          JSON.stringify(response)
      );
    })

    it('rejects with a JSONDecodingError if server sends invalid json data', (done) => {
      const response = {};

      Daedalus.ClientApi.getHistory('123')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'JSONDecodingError', 'includes JSONDecodingError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(response)
      );
    })

    it('rejects with a HTTPStatusError if server response with 400', (done) => {
      Daedalus.ClientApi.getHistory('not-exist')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'HTTPStatusError', 'HTTPStatusError error message');
            done();
        })
        .catch(done);

      requests[0].respond(400,
        {"Content-Type": "application/json"}
      );
    })

    it('rejects with a ServerError if server response with Left', (done) => {
      const response = {
        Left: "Any error"
      }
      Daedalus.ClientApi.getHistory('not-exist')()
        .then( (result) => done(),
          (error) => {
            assert.include(error.message, 'ServerError', 'ServerError error message');
            done();
        })
        .catch(done);

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(response)
      );
    })
  })
})
