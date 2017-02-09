
var express = require('express');
var app = express();
var server = require('http').createServer(app);
var io = require('socket.io')(server);
var port = process.env.PORT || 9999;

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

    var sendBlockCount = 0;
    var sendBlocksId = setInterval(function() {
      var blocks = randomBlocks(3);
      sendBlockCount++;
      console.log("broadcast blocks", blocks);
      socket.broadcast.emit('latestBlocks', blocks);
      // if(sendBlockCount>100) {
      //   clearInterval(sendBlocksId);
      // }
    }, 250);
  }

  firstRun = false;
});

// --------------------------
// mock of blocks
// --------------------------
var blockId = 0;
var randomBlock = function() {
  return  { cbeHeight: randomNumber(1, 50000)
          , cbeRelayedBy: randomStringFromList([null, "KNCMiner", "BMinor", "CMinor"])
          , cbeSize: randomNumber(1, 10000)
          , cbeTimeIssued: blockId++
          , cbeTxNum: randomNumber(1, 10000)
          , cbeTotalSent: { getCoin: randomNumber(1, 1000000) }
          }
}


var randomBlocks = function (max) {
  var maxRandom = randomNumber(1, max);
  var i = 0;
  var blocks = [];
  for(i; i < maxRandom; i++ ) {
    blocks[i] = randomBlock();
  }
  return blocks;
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
