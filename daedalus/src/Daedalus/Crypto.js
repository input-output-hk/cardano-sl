"use strict"

var base64js = require('base64-js');
var bip39 = require('bip39')
var blakejs = require('blakejs');

exports.isValidMnemonic = bip39.validateMnemonic
exports.generateMnemonic = bip39.generateMnemonic

exports.b64ToBytes = base64js.toByteArray;
exports.bytesToB64 = base64js.fromByteArray;

exports.blake2b = function(data) {
  return blakejs.blake2b(data, null, 32);
}

