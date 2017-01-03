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

  describe('getWallets', () => {

    it('returns a list of wallets', (done) => {
      const data = [
        { cwAddress: "XXX",
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
        }];

      Daedalus.ClientApi.getWallets()
        .then( (result) => {
          assert.deepEqual(result, data, 'list of wallet data objects');
          done();
        }, (error) => done(error))
        .catch(done);

      requests[0].respond(200,
        { "Content-Type": "application/json" },
        JSON.stringify(data)
      );
    });

    it('rejects with a JSONDecodingError if server sends invalid json data', (done) => {
      const data = [{ anyOther: "XXX"}];

      Daedalus.ClientApi.getWallets()
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
  })

  describe('getWallet', () => {

    it('returns a wallet', (done) => {
      const walletId = 'XXX';
      const data = {
        cwAddress: walletId,
          cwAmount: {
            getCoin: 33333
          },
          cwMeta: {
            cwType: "CWTPersonal",
            cwCurrency: "ADA",
            "cwName":""
          }
        };

      Daedalus.ClientApi.getWallet(walletId)()
        .then( (result) => {
          assert.deepEqual(result, data, 'wallet data object');
          done();
        }, (error) => done(error))
        .catch(done);

      requests[0]
        .respond(200,
          { "Content-Type": "application/json" },
          JSON.stringify(data)
      );
    })

    it('rejects with a JSONDecodingError if server sends invalid json data', (done) => {
      const data = {};

      Daedalus.ClientApi.getWallet('123')()
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
  })

  describe('newWallet', () => {

    it('returns a new wallet', (done) => {
      const data = {
        cwAddress: '123',
          cwAmount: {
            getCoin: 33333
          },
          cwMeta: {
            cwType: "CWTPersonal",
            cwCurrency: "ADA",
            "cwName":""
          }
        };

      Daedalus.ClientApi.newWallet('CWTPersonal', 'ADA', '')()
        .then( (result) => {
          assert.deepEqual(result, data, 'wallet data object');
          done();
        }, (error) => done(error))
        .catch(done);

      requests[0]
        .respond(200,
          { "Content-Type": "application/json" },
          JSON.stringify(data)
      );
    })

    it('rejects with a JSONDecodingError if server sends invalid json data', (done) => {
      const data = {};

      Daedalus.ClientApi.newWallet('', '', '')()
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

  })

})
