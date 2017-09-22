#!/usr/bin/env bash
set -ex

LOG="genesis-log.txt"
rm -f "$LOG"

###
# STEP 0: Preparation (everyone)
#bash ./build-cardano.sh 2>&1 | tee -a "$LOG"

###
# STEP 1: Delegate Keys (IOHK DevOps)
bash ./gen-delegate-keys.sh 2>&1 | tee -a "$LOG"

# TODO: $REPO_PATH/keys directory must be distributed to everyone so
# they can install it to the same path.

###
# STEP 2: Stakeholder Keys and Certs (each stakeholder independently)

# IOHK Org Rep
bash ./iohk-keys-and-certs.sh 2>&1 | tee -a "$LOG"
# SGG Org Rep
bash ./sgg-keys-and-certs.sh 2>&1 | tee -a "$LOG"
# CF Org Rep
bash ./cf-keys-and-certs.sh 2>&1 | tee -a "$LOG"

###
# STEP 3: Finalize AVVM utxo (IOHK DevOps)
bash ./gen-avvm-seeds.sh 2>&1 | tee -a "$LOG"

###
# STEP 4: VSS Certificates (IOHK DevOps)
bash ./gen-vss-certs.sh 2>&1 | tee -a "$LOG"

###
# STEP 5: Genesis (IOHK DevOps and Cardano-SL Core)
echo "Assemble genesis JSON file from the log"
