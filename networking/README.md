# sketch-playground

Repository for modeling ideas of distributed systems networking and implementing sketches of these ideas.

## Overview

### Node

Node is a fundamental part of distributed system. Each node has:

1. transport endpoint (based on IP-address and port),
2. internal state,
3. thread for dispatching different network events.

One transport endpoint can contain one or more nodes. For example, two nodes started on endpoint `127.0.0.1:10183` can have identificators `127.0.0.1:10183:0` and `127.0.0.1:10183:1`.

There're sets of workers and listeners associated with each node. Workers and listeners exchange messages with each other.

### Worker

Worker is a black box that runs some custom code and sends messages to listeners. Worker is an **active** part because it starts messaging. It can just send isolated message without response expectation or establish a bi-directional conversation session (in this case worker expects response from the listener).

### Listener

Listener is a black box that receives messages from workers and runs some custom code. Listener is a **passive** part because it's waiting for a messages from the workers. It can just receive isolated message or receive it and response to it (bi-directional conversation, in this case worker is a client and listener is a server).

## Examples

You can find usage examples in `examples` directory. Start from the simplest one, PingPong.
