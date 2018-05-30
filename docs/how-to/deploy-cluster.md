# How to deploy cluster, prepare installers

## Table of contents
  * [How to deploy developer cluster](#how-to-deploy-developer-cluster)
    + [Getting access to staging jumpserver](#getting-access-to-staging-jumpserver)
    + [Preparing branch and configuration](#preparing-branch-and-configuration)
    + [Creation of tmux session](#creation-of-tmux-session)
    + [Deployment and recommendations](#deployment-and-recommendations)
  * [How to prepare installers](#how-to-prepare-installers)
    + [Which parts Daedalus consists of](#which-parts-daedalus-consists-of)
    + [How to build installers](#how-to-build-installers)
    + [How to downloand installers](#how-to-download-installers)
    + [Recommendations and hacks](#recommendations-and-hacks)

## How to deploy developer cluster

### Getting access to staging jumpserver
First of all you should make sure you have access to `staging` jumpserver via SSH. 
You can check it trying to connect to cluster `ssh staging@35.156.156.28`.
If you don't have access you should create ssh key add it to you ssh agent and then 
follow [this](https://github.com/input-output-hk/iohk-ops#getting-ssh-access) instruction.
It may take DevOps some time to give you access, ping them periodically.

### Preparing branch and configuration
The next step is you need to prepare branch and configuration for deploy.
For example you want to deploy release branch with smaller `k`, 
then you must create new branch based on this release one and modify `devnet` configuration in `lib/configuration.yaml` in this branch.
Sometimes you will need to create your own configuration, however,
most likely it will be enough to modify existing `devnet` configuration.
Then you should push your branch on github.

### Creation of tmux session
After it you should connect to `staging` jumpserver and create tmux session for the issue within which you are going to deploy cluster:
`tmux new -s issue-id` (for instance `tmux new -s csl228`). 
It should be done for the two reasons: to keep state of `nix-shell` between reconnections to `staging` jumpserver
and if something goes wrong (and it will go) you can ask devops to help you attaching to this session.

To attach to this session when you reconnect to `staging` server, you can use `tmux a -t issue-id`.

### Deployment and recommendations
The next steps are described [here](https://github.com/input-output-hk/internal-documentation/wiki/Developer-clusters-HOWTO).
Some notes and recommendations about this instruction:
* `cardano-sl` git revision is most likely HEAD commit of your branch
* Ask DevOps which `iohk-ops` branch you should use before deploy.
* Specify `--bump-system-start-held-by` option for `io deploy`. 
The passed value depends on how long will deployment take: 
if `cardano-sl` is compiling from scratch, then it probably should be 15-20 mins, otherwise it's enough 5-10 mins. 
If deploy won't be done utill system start then the first slots will be without blocks and system most probably will die.
* Obtain logs from one of node after you deployed the system to make sure deployment was successful.

## How to prepare installers

Installer is file which can install Daedalus on your machine (`*.exe` file for Windows, `*.pkg` file for MacOS).

### Which parts Daedalus consists of
Before the process of preparing installers will be described I would like to say which parts Daedalus consists of.
There are three parts: `cardano-launcher`, `cardano-node` and Daedalus frontend.
`cardano-node` is like application which is launched on cluster, however, it also provides API to maintain your wallets, secret keys and etc.
So essentially it's backend for Daedalus frontend.

When you open Daedalus on your machine `cardano-launcher` is launched and it launches `cardano-node` and Daedalus frontend which uses provided API.
So installer contains all these three parts.
`cardano-node` and `cardano-launcher` come from [cardano-sl repository](https://github.com/input-output-hk/cardano-sl), 
Daedalus fronted comes from [daedalus repository](https://github.com/input-output-hk/daedalus).

### How to build installers
Preparing installers implies that cluster which Daedalus will connect to is already deployed.
Installers are a result of continuous integration build of daedalus repository commit. 
As result of this build, we get an installer for Windows and for MacOS.

#### Preparing confinguration.yaml
As first step you should add to `configuration.yaml` in cardano-sl repository new configurations: one for Windows, one for MacOS.
It should be derived from cluster configuration. For `devnet` configuration `devnet_staging_win64` and `devnet_staging_macos64`
already exist.

Configuration names don't matter, however, it matters those protocol parameters of these configurations are the same for base configuration, so
you should add configurations carefully.

After you added them, you should push commit with these changes on github, 
wait for a build for this commit and then follow the next steps.

#### Configure Daedalus installers
After build from the previous step is ready, you should proceed the following actions for daedalus repository:
1. Create new branch for your installers.

   The branch should be based on a version of Daedalus which you want installers to be prepared for.
2. Specify in `.buildkite/pipeline.yml` and `appveyor.yml` branch of cardano-sl repository which `cardano-node` will be taken from.

   This branch should be branch from the previous step and should contain added configurations.
   Make sure that backed API compatiable with Daedalus frontend, [this page](https://github.com/input-output-hk/internal-documentation/wiki/Daedalus-installer-history) may be useful. It reflects  correspondence between cardano revisions and Daedalus revisions.
   
   In `appveyor.yml` in [this line](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/appveyor.yml#L26)
   you should specify your branch instead of `release/1.1.0`.
   
   In `.buildkite/pipeline.yml` in [this](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/.buildkite/pipeline.yml#L6) and 
   [this](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/.buildkite/pipeline.yml#L14) lines
   you need to specify your branch instead of `release/1.1.0`.
3. Specify IP address of relay node in `installers/wallet-topology.yaml`.

   To get this IP address you should execute `io info` in `nix-shell` for your cluster, 
   IP address of node with name `r1` is needed address.
   After you get it you should write down it in `installers/wallet-topology.yaml`, like:
   ```
   wallet:
     relays: [[{"addr": "<your ip>"}]]
     valency: 1
     fallbacks: 3
   ```
   For instance, if IP is `18.196.10.154` the `wallet-topology.yaml` will look:
   ```
   wallet:
     relays: [[{"addr": "18.196.10.154"}]]
     valency: 1
     fallbacks: 3
   ```
4. Update Daedalus configs.

   There are two configs: `installers/launcher-config-mac.yaml` and `installers/launcher-config-windows.yaml` which should be updated.
   First of all you need to know system start. You can take it from `config.yaml`'s `systemStart` contents.
   
   You need to specify `key` [here](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/installers/launcher-config-mac.yaml#L42) as `devnet_macos64`, 
   and `key` [here](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/installers/launcher-config-windows.yaml#L41) as `devnet_win64`. 
   To be more precisely you should specify them as your configuration names which you created in `configuration.yaml`.
   
   You need to specify `systemStart` [here](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/installers/launcher-config-mac.yaml#L43) and [here](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/installers/launcher-config-windows.yaml#L42).
   
   You need to specify `--update-server` [here](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/installers/launcher-config-mac.yaml#L12) and [here](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/installers/launcher-config-windows.yaml#L12) as `https://s3.eu-central-1.amazonaws.com/update-system-testing`.
   
   You need to specify `reportServer` key [here](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/installers/launcher-config-mac.yaml#L37) and [here](https://github.com/input-output-hk/daedalus/blob/ba037d30491803500cca05829e6dce7533b1b021/installers/launcher-config-windows.yaml#L36) as `http://report-server.awstest.iohkdev.io:8080`.
   
Example of all changes you can find [here](https://github.com/input-output-hk/daedalus/commit/0f9eb4140eb9bd57f974878ac648bce349d824c2).
   
### How to download installers
Follow [this](https://github.com/input-output-hk/internal-documentation/wiki/Daedalus#q-where-can-i-find-built-installers) instruction.

### Recommendations and hacks
* If something went wrong and Daedalus failed to run, you can look at logs in `~/Library/Application Support/Daedalus/Logs` under MacOS (note that `~/Library` folder is hidden by default) and in `C:\Users\<USER-NAME>\AppData\Roaming\Daedalus\Logs` under Windows.
* Sometimes Daedalus is closed due to error earlier than it has time to write something to logs. 
  You can see the error in console if run `/Applications/Daedalus.app/Contents/MacOS/Daedalus` from console under MacOS and `C:\\Program Files\Daedalus\daedalus.bat` under Windows.
* If your goal is to install Daedalus on your machine, but you can't build installer due to some obscure reason, you can take installer 
  needed version from somewhere (for instance from [daedaluswallet.io](https://daedaluswallet.io/#download)), install it and replace `configuration.yaml`, `wallet-topology.yaml` with files for your cluster. Also don't forget specify valid parameters in execution scripts (`Daedalus` under MacOS and `daedalus.bat` under Windows). 
  All necessary files are in `/Applications/Daedalus.app/Contents/MacOS/` under MacOS and in `C:\\Program Files\Daedalus` under Windows.
* Don't forget to delete all files between reinstalations: `~/Library/Application Support/Daedalus` and `C:\Users\<USER-NAME>\AppData\Roaming\Daedalus\`.
