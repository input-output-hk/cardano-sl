#!/usr/bin/env node

var yaml = require('js-yaml');

var fs = require('fs')

var avvmFile = fs.readFileSync(process.argv[2]);
var avvmJson = JSON.parse(avvmFile.toString('ascii'))

var avvmMap = {};
avvmJson.utxo.forEach(function(v){
  avvmMap[v.address] = v.coin*1000000;
})

console.log(yaml.dump(avvmMap));
