#!/bin/sh

###
### This script is a sort of an classifying exception filter for the mainnet cluster logs.
### It is intended to run on a stream of log messages with the '[Ee]xception' substring in it.
###
### Two useful usage scenarios:
###  - by commenting out any line, we can effectively query for a particular message
###  - we can monitor for any new exceptions to occur
###
### Misfeatures:
###  - Too verbose, since some messages are essentially products, like 'sending X to ... failed with Y'
###

grep 'cardano-node-start:' |
        cut -c 56- |
        grep -v 'Exception in tcp server: thread killed' |
        grep -v 'Exceptional user error (connectToPeer : you.re doing it wrong! Our node is closed!)' |
        grep -v 'Reporting error with reason "Worker/plugin with logger name LoggerName {getLoggerName = "node"} failed with exception: BlockNetLogicException: DialogUnexpected "enqueueMsgSingle: contacted no peers"' |
        grep -v 'Reporting error with reason "Worker/plugin with logger name LoggerName {loggerName = "node"} failed with exception: Network.Socket.bind: resource busy \(Address already in use\)' |
        grep -v 'ending MsgAnnounceBlockHeader OriginSender to .* failed with TransportError ConnectFailed "user error (Could not parse)" :: SomeException' |
        grep -v 'error, called at src/Node/Internal.hs' |
        grep -v 'exception while stopping node thread blocked indefinitely in an STM transaction' |
        grep -v 'exception while stopping node thread killed' |
        grep -v 'handleBlockRetrievalE: error handling nodeId=".*:0", header=.*: BlockNetLogicException: DialogUnexpected "Error retrieving blocks from .* to .* from peer \\\".*:0\\\": Failed to receive block #0"' |
        grep -v 'handleBlockRetrievalE: error handling nodeId=".*:0", header=.*: BlockNetLogicException: DialogUnexpected "requestHeaders: received MsgNoHeaders from \\\".*:0\\\", msg: server node is in recovery mode"' |
        grep -v 'handleBlockRetrievalE: error handling nodeId=.*BlockNetLogicException: DialogUnexpected "enqueueMsgSingle: contacted no peers"' |
        grep -v 'handleRecoveryE: error handling nodeId=.*: BlockNetLogicException: DialogUnexpected "enqueueMsgSingle: contacted no peers"' |
        grep -v 'handleRetrievalE: error handling nodeId=.* BlockNetLogicException: DialogUnexpected "enqueueMsgSingle: contacted no peers"' |
        grep -v 'node stopped with exception thread killed' |
        grep -v 'node stopped with exception user interrupt' |
        grep -v 'retrievalWorker mainLoopE: error caught BlockNetLogicException: DialogUnexpected "enqueueMsgSingle: contacted no peers"' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with Timeout :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError ConnectFailed "Network.Socket.ByteString.sendMany: does not exist (No route to host)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError ConnectFailed "Network.Socket.ByteString.sendMany: invalid argument (Bad file descriptor)"' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError ConnectFailed "Network.Socket.ByteString.sendMany: resource vanished (Broken pipe)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError ConnectFailed "Network.Socket.ByteString.sendMany: timeout (Connection timed out)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError ConnectFailed "Network.Socket.recvBuf: resource vanished (Connection reset by peer)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError ConnectFailed "setupRemoteEndPoint: Host mismatch. Claimed' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError ConnectNotFound "Network.Socket.connect: <socket: [0-9]*>: does not exist (Connection refused)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError ConnectNotFound "Network.Socket.connect: <socket: [0-9]*>: resource vanished (Connection reset by peer)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError ConnectNotFound "setupRemoteEndPoint: Invalid endpoint" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError SendFailed "Network.Socket.ByteString.sendMany: invalid argument (Bad file descriptor)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError SendFailed "Network.Socket.ByteString.sendMany: resource vanished (Broken pipe)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError SendFailed "Network.Socket.recvBuf: resource vanished (Connection reset by peer)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError SendFailed "Remote endpoint closed" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError SendFailed "user error (apiSend RELY violation)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with TransportError SendFailed "user error (recvExact: Socket closed)" :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .* failed with thread killed :: SomeException' |
        grep -v 'sending MsgAnnounceBlockHeader OriginSender to NodeId .*:0 failed with TransportError SendFailed "user error (apiSend RELY violation)" :: SomeException' |
        grep -v 'sending MsgRequestBlockHeaders (Just (fromList \[NodeId .*:0\])) to NodeId .*:0 failed with BlockNetLogicException: DialogUnexpected "requestHeaders: received MsgNoHeaders from \\\".*:0\\\", msg: server node is in recovery mode" :: SomeException' |
        grep -v 'sending MsgRequestBlockHeaders Nothing to NodeId .* failed with Timeout :: SomeException' |
        grep -v 'sending MsgRequestBlockHeaders Nothing to NodeId .* failed with TransportError ConnectNotFound "Network.Socket.connect: <socket: [0-9]*>: does not exist (Connection refused)" :: SomeException' |
        grep -v 'sending MsgRequestBlockHeaders Nothing to NodeId .*:0 failed with TransportError ConnectNotFound "setupRemoteEndPoint: Invalid endpoint" :: SomeException' |
        grep -v 'sending MsgRequestBlocks (fromList \[NodeId .*:0\]) to NodeId .*:0 failed with BlockNetLogicException: DialogUnexpected "Error retrieving blocks from .* to .* from peer \\\".*:0\\\": Failed to receive block #0" :: SomeException' |
        grep -v 'sending MsgRequestBlocks (fromList \[NodeId .*:0\]) to NodeId .*:0 failed with TransportError ConnectNotFound "Network.Socket.connect: <socket: [0-9]*>: does not exist (Connection refused)" :: SomeException' |
        grep -v 'sending MsgRequestBlocks (fromList \[NodeId .*:0\]) to NodeId .*:0 failed with TransportError ConnectNotFound "setupRemoteEndPoint: Invalid endpoint" :: SomeException' |
        grep -v 'sending MsgRequestBlocks (fromList \[NodeId .*:0\]) to NodeId .*:0 failed with thread killed :: SomeException' |
        grep -v 'sending MsgTransaction (.* (NodeId .*)) to NodeId .* failed with Timeout :: SomeException' |
        grep -v 'sending MsgTransaction (.* (NodeId .*)) to NodeId .* failed with TransportError ConnectNotFound "Network.Socket.connect: <socket: [0-9]*>: does not exist (Connection refused)" :: SomeException' |
        grep -v 'sending MsgTransaction (.* (NodeId .*)) to NodeId .* failed with UnexpectedEnd :: SomeException' |
        grep -v 'sending MsgTransaction (OriginForward (NodeId .*)) to NodeId .*:0 failed with TransportError ConnectFailed "Network.Socket.recvBuf: resource vanished (Connection reset by peer)" :: SomeException' |
        grep -v 'sending MsgTransaction (OriginForward (NodeId .*)) to NodeId .*:0 failed with TransportError ConnectNotFound "setupRemoteEndPoint: Invalid endpoint" :: SomeException' |
        grep -v 'sending MsgTransaction (OriginForward (NodeId .*:0)) to NodeId .* failed with TransportError ConnectFailed "Network.Socket.recvBuf: resource vanished (Connection reset by peer)" :: SomeException' |
        grep -v 'sending MsgTransaction (OriginForward (NodeId .*:0)) to NodeId .*:0 failed with TransportError ConnectFailed "setupRemoteEndPoint: Host mismatch. Claimed: .*; Numeric: .*; Resolved: .*" :: SomeException' |
        grep -v 'sending MsgTransaction (OriginForward (NodeId .*:0)) to NodeId .*:0 failed with TransportError ConnectNotFound "setupRemoteEndPoint: Invalid endpoint" :: SomeException' |
        grep -v 'sending MsgTransaction (OriginForward (NodeId .*:0)) to NodeId .*:0 failed with TransportError ConnectTimeout "Timed out" :: SomeException' |
        grep -v 'sending MsgTransaction (OriginForward (NodeId .*:0)) to NodeId .*:0 failed with thread killed :: SomeException' |
        grep -v 'subscriptionWorker: lost connection to NodeId .*:0 Exceptional (InternalError "connectToPeer : node closed while establishing connection!")' |
        grep -v 'subscriptionWorker: lost connection to NodeId .*:0 Exceptional (TransportError ConnectFailed "Network.Socket.recvBuf: resource vanished (Connection reset by peer)")' |
        grep -v 'subscriptionWorker: lost connection to NodeId .*:0 Exceptional (TransportError ConnectFailed "setupRemoteEndPoint: Host mismatch. Claimed: .*; Numeric: .*; Resolved: .*")' |
        grep -v 'subscriptionWorker: lost connection to NodeId .*:0 Exceptional (TransportError ConnectNotFound "Network.Socket.connect: <socket: [0-9]*>: does not exist (Connection refused)")' |
        grep -v 'subscriptionWorker: lost connection to NodeId .*:0 Exceptional (TransportError ConnectNotFound "setupRemoteEndPoint: Invalid endpoint")' |
        grep -v 'subscriptionWorker: lost connection to NodeId .*:0 Exceptional (TransportError ConnectTimeout "Timed out")' |
        grep -v 'subscriptionWorker: lost connection to NodeId .*:0 Exceptional (TransportError SendFailed "Remote endpoint closed")' |
        grep -v 'subscriptionWorker: lost connection to NodeId .*:0 Exceptional no peer data because the connection was lost' |
        cat
