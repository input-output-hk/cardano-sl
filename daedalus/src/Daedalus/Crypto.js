"use strict"

var bip39 = require('bip39')

exports.isValidMnemonic = bip39.validateMnemonic
exports.generateMnemonic = bip39.generateMnemonic
