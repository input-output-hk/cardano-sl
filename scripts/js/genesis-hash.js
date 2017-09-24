#!/usr/bin/env node

var blake = require('blakejs');
var canonize = require('canonical-json')

var fs = require('fs')

var pretty = fs.readFileSync(process.argv[2]);
var json = JSON.parse(pretty.toString('ascii'))
var can = canonize(json)
console.log(blake.blake2bHex(can, null, 32))
