"use strict"

var bip39 = require('bip39')
var blakejs = require('blakejs');

exports.isValidMnemonic = bip39.validateMnemonic
exports.generateMnemonic = bip39.generateMnemonic

exports.bytesToB16 = function(bytes) {
    return Buffer.from(bytes).toString('hex');
}

exports.blake2b = function(data) {
  return blakejs.blake2b(data, null, 32);
}

