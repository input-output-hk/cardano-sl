
var express = require('express');
var app = express();
var server = require('http').createServer(app);
var io = require('socket.io')(server);
var port = process.env.PORT || 9999;
var randomstring = require("randomstring");

// --------------------------
// express
// --------------------------

server.listen(port, function () {
  console.log('Server listening at port %d', port);
});

firstRun = true;

// --------------------------
// socket
// --------------------------

io.on('connection', function (socket) {

  if(firstRun) {
    // push blocks
    var blocksCount = 0;
    var sendBlocksId = setInterval(function() {
      blocksCount++;
      socket.broadcast.emit('latestBlocks', { "Right": randomBlocks(5) });
      if(blocksCount>10) {
        clearInterval(sendBlocksId);
      }
    }, randomNumber(450, 1000));

    // push transactions
    var txCount = 0;
    var sendTxId = setInterval(function() {
      txCount++;
      socket.broadcast.emit('latestTransactions', { "Right": randomTxs(5) });
      if(txCount>20) {
        clearInterval(sendTxId);
      }
    }, randomNumber(750, 1200));
  }

  firstRun = false;
});

// --------------------------
// mock blocks
// --------------------------

var blockId = 0;
var randomBlock = function() {
  // Encoded CBlockEntry
  // @see src/Generated/Pos/Explorer/Web/ClientTypes.purs
  return  { cbeBlkHash: randomNumber(1, 50000).toString()
          , cbeHeight: randomNumber(1, 50000)
          , cbeRelayedBy: randomStringFromList([null, "KNCMiner", "BMinor", "CMinor"])
          , cbeSize: randomNumber(1, 10000)
          , cbeTimeIssued: blockId++
          , cbeTxNum: randomNumber(1, 10000)
          , cbeTotalSent: { getCoin: randomNumber(1, 1000000) }
          }
}


var randomBlocks = function (max) {
  var maxRandom = randomNumber(1, max)
      , i = 0
      , blocks = [];
  for(i; i < maxRandom; i++ ) {
    blocks[i] = randomBlock();
  }
  return blocks;
}

// --------------------------
// mock transactions
// --------------------------

var txId = 0;
var randomTx = function() {
  // Encoded CTxEntry
  // @see src/Generated/Pos/Explorer/Web/ClientTypes.purs
  return  { cteId: randomstring.generate(63)
          , cteTimeIssued: txId++
          , cteAmount: { getCoin: randomNumber(1, 1000000) }
          }
}

var randomTxs = function (max) {
  var maxR = randomNumber(1, max)
      , i = 0
      , txs = [];
  for(i; i < maxR; i++ ) {
    txs[i] = randomTx();
  }
  return txs;
}


// --------------------------
// helper
// --------------------------


var randomNumber = function(from, to) {
  return Math.floor(Math.random() * ((to-from)+1) + from);
}

var randomStringFromList = function(list) {
  if (list !== undefined || list.length > 0) {
    return list[randomNumber(0, list.length - 1 )];
  } else {
    return '';
  }
}
