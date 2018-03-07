<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Requirements</a>
<ul>
<li><a href="#sec-1-1">1.1. Nix</a>
<ul>
<li><a href="#sec-1-1-1">1.1.1. Optional: Enable IOHK's binary cache</a></li>
</ul>
</li>
<li><a href="#sec-1-2">1.2. Miscellaneous Utilities</a></li>
</ul>
</li>
<li><a href="#sec-2">2. Mainnet Wallet</a>
<ul>
<li><a href="#sec-2-1">2.1. Backup local state</a></li>
<li><a href="#sec-2-2">2.2. Fetch latest code</a></li>
<li><a href="#sec-2-3">2.3. Build and run</a></li>
<li><a href="#sec-2-4">2.4. Usage FAQs</a>
<ul>
<li><a href="#faq-custom-config">2.4.1. How do I customize the wallet configuration?</a></li>
<li><a href="#sec-2-4-2">2.4.2. How do I know when the wallet has fetched all the blocks?</a></li>
<li><a href="#sec-2-4-3">2.4.3. Where can I find the API documentation?</a></li>
<li><a href="#sec-2-4-4">2.4.4. How can I inspect runtime metrics and statistics?</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>


# Requirements<a id="sec-1" name="sec-1"></a>

## Nix<a id="sec-1-1" name="sec-1-1"></a>

The wallet is built using [nix package manager](https://nixos.org/nix/). To install it on
most Linux distros download and run the installation script.

    curl https://nixos.org/nix/install > install-nix.sh
    . install-nix.sh

Follow the directions and then log out and back in.

### Optional: Enable IOHK's binary cache<a id="sec-1-1-1" name="sec-1-1-1"></a>

Skip this section if you prefer to build all code from IOHK
locally. When the binary cache is enabled build steps will tend
go faster.

    sudo mkdir -p /etc/nix
    cat <<EOF | sudo tee /etc/nix/nix.conf
    binary-caches            = https://cache.nixos.org https://hydra.iohk.io
    binary-cache-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
    EOF

## Miscellaneous Utilities<a id="sec-1-2" name="sec-1-2"></a>

Use `nix` to install essential utilities.

    nix-env -i git

# Mainnet Wallet<a id="sec-2" name="sec-2"></a>

## Backup local state<a id="sec-2-1" name="sec-2-1"></a>

Skip to the next section if this is target machine doesn't yet have
`cardano-sl` set up.

To avoid catastrophic data loss, stop the wallet and backup the
local databases, keys, certificates, and logs. By default, the
local state will be in `./state-wallet-mainnet`, but may be
elsewhere (see `stateDir` attribute in `./custom-wallet-config.nix`).

## Fetch latest code<a id="sec-2-2" name="sec-2-2"></a>

Clone the [cardano-sl repository](https://github.com/input-output-hk/cardano-sl) or `cd` into a preexisting copy.

    git clone https://github.com/input-output-hk/cardano-sl.git
    cd cardano-sl

Switch to the `master` branch and pull the latest code.

    git checkout master
    git pull

Dump the current revision and confirm with IOHK whether it is as
expected.

    git rev-parse HEAD

## Build and run<a id="sec-2-3" name="sec-2-3"></a>

By default the wallet's local state goes in
`./state-wallet-mainnet`. If you prefer to have this sensitive data
outside the git repository, see the FAQ about supported custom
configuration settings (See section 2.4.1).

Build the wallet and generate the shell script to connect to
mainnet.

    nix-build -A connectScripts.mainnetWallet -o "./launch_$(date -I)_$(git rev-parse --short HEAD)"

After the build finishes the generated connection script is
available as a symlink called `./launch_2018-01-30_0d4f79eea`, or
similar. Run that symlink as a script to start the wallet.

## Usage FAQs<a id="sec-2-4" name="sec-2-4"></a>

### How do I customize the wallet configuration?<a id="faq-custom-config" name="faq-custom-config"></a>


Before building the wallet copy `./sample-wallet-config.nix` to
`./custom-wallet-config.nix` and edit as needed.

Supported options include:

-   **`walletListen`:** Wallet API server
-   **`ekgListen`:** Runtime metrics server
-   **`stateDir`:** Directory for the wallet's local state. Must be
    enclosed in double quotes.
-   **`topologyFile`:** Used to connect to a custom set of nodes on
    the network. When unspecified an appropriate
    default topology is generated.

### How do I know when the wallet has fetched all the blocks?<a id="sec-2-4-2" name="sec-2-4-2"></a>

Monitor the logs in the wallet's local state directory for lines
like

    slot: 18262th slot of 25th epoch

If any of the recent matches are more than a slot lower than the
latest epoch and slot reported by [Cardano Explorer](https://cardanoexplorer.com/), the wallet is
still syncing.

### Where can I find the API documentation?<a id="sec-2-4-3" name="sec-2-4-3"></a>

Run the latest wallet and go to <https://127.0.0.1:8090/docs>.

### How can I inspect runtime metrics and statistics?<a id="sec-2-4-4" name="sec-2-4-4"></a>

Current metrics and stats are available at <http://127.0.0.1:8000/>.