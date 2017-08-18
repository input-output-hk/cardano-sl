const fs = require('fs')
const WebSocket = require('ws');

module.exports = {
    // run this in main process
    /*
     * > var ca = readCA('ca.crt')
     * > var tls = initTLS(ca)
    */
    // then use this ^ tls for requests
    readCA: (path) => {
        return fs.readFileSync(path);
    },

    notify: function notify(ca, succ, err) {
        var ws = new WebSocket('wss://localhost:8090', {ca: ca})
        ws.on('close', () =>
            {
                setTimeout(() =>
                    {
                        // reconnect
                        ws.terminate()
                        notify(ca, succ, err)
                    }
                , 5000)
            }
        )
        ws.on('error', err)
        ws.on('message', succ)
    }
}
