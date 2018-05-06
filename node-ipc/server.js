var child_process = require("child_process");
const fs = require('fs');

console.log(new Date(), "in parent")

logfile = fs.createWriteStream("cardano-node.log", { flags: "a" });

logfile.on("open", function () {
  console.log(new Date(), "log file opened");

  const subprocess = child_process.spawn("cardano-node", [
    "--wallet-address", "127.0.0.1:0",
    "--no-tls",
    "--configuration-file", "../lib/configuration.yaml",
    "--configuration-key", "mainnet_full"
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
  });

  subprocess.send({ QueryPort:[]})

  setTimeout(function () {
    //process.exit();
    subprocess.disconnect();
  }, 30000);
});
