/**
 * This file is the entry point for all mocha tests
 */

import sinon from 'sinon';

// shim XMLHttpRequest for node - needed by mocha and wallaby
global.XMLHttpRequest = sinon.useFakeXMLHttpRequest();
