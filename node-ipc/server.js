var child_process = require("child_process");
const fs = require('fs');

console.log(new Date(), "in parent")

fs.mkdirSync("test-state");
logfile = fs.createWriteStream("test-state/cardano-node.log", { flags: "a" });

logfile.on("open", function () {
  var timerid;
  console.log(new Date(), "log file opened");

  const subprocess = child_process.spawn("cardano-node", [
    "--wallet-address", "127.0.0.1:0",
    "--no-tls",
    "--configuration-key", "mainnet_dryrun_full",
    "--topology", "node-ipc/wallet-topology.yaml",
    "--wallet-db-path", "test-state/wallet",
    "--db-path", "test-state/db",
    "--keyfile", "test-state/secret",
    "--pubkeyfile", "test-state/public"
    ], {
    stdio: [ "inherit", logfile, logfile, "ipc" ]
  });
  subprocess.on("message", function (msg) {
    console.log(new Date(), "got reply",msg);
  });
  subprocess.on("close", function(code, signal) {
    console.log("all stdio to child has been closed", code, signal);
  });
  subprocess.on("disconnect", function() {
    console.log(new Date(), "all IPC handles closed");
  });
  subprocess.on("error", function (err) {
    console.log("error:", err);
  });
  subprocess.on("exit", function (code, signal) {
    console.log(new Date(), "child exited", code, signal);
    clearTimeout(timerid);
    if (code == 20) {
      process.exit(0);
    } else {
      process.exit(-1);
    }
  });

  subprocess.send({ QueryPort:[]})

  timerid = setTimeout(function () {
    //process.exit();
    console.log(new Date(), "sending disconnect");
    subprocess.disconnect();
    timerid = setTimeout(function () {
      console.log(new Date(), "it failed to exit, killing");
      subprocess.kill();
    },30000);
  }, 30000);
});
