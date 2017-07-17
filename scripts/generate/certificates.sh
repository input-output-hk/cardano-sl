#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i bash -p nodejs-7_x stack

# NOTE: this file must be kept idempotent


set -ex

genesis=$1

if [[ ! -d "$genesis" ]]; then
  echo "No such folder $genesis"
  exit 1
fi

pushd "$genesis"
  if [[ ! -d postvend-app ]]; then
    git clone git@github.com:input-output-hk/postvend-app.git
  fi

  pushd postvend-app
    if [[ ! -f "build-done" ]]; then
      for i in certificate-generation-script new-certificate-generator paper-vend-generator; do
        pushd $i
          npm install
        popd
      done
      mkdir -p bin
      stack build --nix --local-bin-path bin --copy-bins
      echo "done" > build-done
    fi

    # cleanup
    rm -Rf paper-certs-* redeem-certs-* seeds.txt

    for i in ../avvm/*.seed; do
      cat "$i" >> seeds.txt
      echo '' >> seeds.txt
    done
  
    ./bin/postvend-cli -- gen-test-certs --seeds-file seeds.txt
  
    mkdir ../certs
  
    mv paper-certs-mnem-* ../certs/paper-certs-mnem
    mv paper-certs-* ../certs/paper-certs
    mv redeem-certs-* ../certs/redeem-certs
  popd
popd

tar -czf "$genesis-certs-secrets.tgz" "$genesis/certs" "$genesis/secrets"

echo "Done"
