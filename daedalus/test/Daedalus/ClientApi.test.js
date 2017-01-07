import {assert} from 'chai';
import sinon from 'sinon';
const Daedalus = require ('../../dist/Daedalus');
import getWallets from './ClientApi-desc/getWallets.desc';
import getWallet from './ClientApi-desc/getWallet.desc';
import newWallet from './ClientApi-desc/newWallet.desc';
import deleteWallet from './ClientApi-desc/deleteWallet.desc';
import send from './ClientApi-desc/send.desc';
import getHistory from './ClientApi-desc/getHistory.desc';
import isValidAddress from './ClientApi-desc/isValidAddress.desc';

describe('ClientApi', () => {

  getWallets();
  getWallet();
  newWallet();
  deleteWallet();
  send();
  getHistory();
  isValidAddress();

})
