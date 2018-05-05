var child_process = require("child_process");
const fs = require('fs');

console.log("in parent")

logfile = fs.createWriteStream("cardano-node.log", { flags: "a" });

logfile.on("open", function () {
  console.log("log file opened");

  const subprocess = child_process.spawn("cardano-node", [
    "--wallet-address", "127.0.0.1:0",
    "--no-tls",
    "--configuration-file", "../lib/configuration.yaml",
    "--configuration-key", "mainnet_full"
    ], {
    stdio: [ "inherit", logfile, logfile, "ipc" ]
  });
  subprocess.on("message", function (msg) {
    console.log("got reply",msg);
  });

  subprocess.send({ QueryPort:[]})

  setTimeout(function () {
    process.exit();
  }, 10000);
});
