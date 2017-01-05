import {assert} from 'chai';
import sinon from 'sinon';

const Daedalus = require ('../../../dist/Daedalus');

export default function () {

  describe('send()', () => {

    let xhr;
    let requests;

    beforeEach(() => {
      requests = [];
      xhr = sinon.useFakeXMLHttpRequest();
      xhr.onCreate = (req) => requests.push(req);
    })

    afterEach(() => xhr.restore())

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
}
