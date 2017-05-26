import {assert} from 'chai';
import sinon from 'sinon';
const Daedalus = require ('../../dist/Daedalus');
import getAccounts from './ClientApi-desc/getAccounts.desc';
import getAccount from './ClientApi-desc/getAccount.desc';
import newAccount from './ClientApi-desc/newAccount.desc';
import deleteAccount from './ClientApi-desc/deleteAccount.desc';
import send from './ClientApi-desc/send.desc';
import getHistory from './ClientApi-desc/getHistory.desc';
import isValidAddress from './ClientApi-desc/isValidAddress.desc';

describe('ClientApi', () => {

  getAccounts();
  getAccount();
  newAccount();
  deleteAccount();
  send();
  getHistory();
  isValidAddress();

})
