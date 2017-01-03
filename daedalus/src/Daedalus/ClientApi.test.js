import {assert} from 'chai';
import sinon from 'sinon';
// import Daedalus from '../../dist/Daedalus';
// import ... does not work here, so just use require
const Daedalus = require ('../../dist/Daedalus');

describe('ClientApi', () => {

  describe('getWallets', () => {
    let xhr;
    let requests;

    beforeEach(() => {
      requests = [];
      xhr = sinon.useFakeXMLHttpRequest();
      xhr.onCreate = (req) => requests.push(req);
    })

    afterEach(() => {
      xhr.restore();
    })

    it('resolves with list of wallets', (done) => {
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
          assert.equal(result.length, 2, 'includes wallets');
          assert.equal(result[0].cwAddress, 'XXX', 'has cwAddress field');
          done();
        }, (error) => {
          done(error);
        })
        .catch((err) => { console.log('catch: ', err.message); });

      requests[0].respond(200,
        { "Content-Type": "application/json" },
        JSON.stringify(data)
      );
    });

    it('rejects with JSONDecodingError', (done) => {
      const data = [{ anyOther: "XXX"}];

      Daedalus.ClientApi.getWallets()
        .then( (result) => {
          done();
        }, (error) => {
          assert.include(error.message, 'JSONDecodingError', 'includes JSONDecodingError error message');
          done();
        })
        .catch((err) => { console.log('catch: ', err.message); });

      requests[0].respond(200,
        {"Content-Type": "application/json"},
        JSON.stringify(data)
      );
    })
  })
})
