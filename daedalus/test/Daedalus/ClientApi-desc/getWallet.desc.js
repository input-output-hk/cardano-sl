import {assert} from 'chai';
import sinon from 'sinon';

const Daedalus = require ('../../../dist/Daedalus');

export default function () {

  describe('getWallet()', () => {

    let xhr;
    let requests;

    beforeEach(() => {
      requests = [];
      xhr = sinon.useFakeXMLHttpRequest();
      xhr.onCreate = (req) => requests.push(req);
    })

    afterEach(() => xhr.restore())
    
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
}
