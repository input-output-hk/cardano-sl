# This script will load nix-built docker images of cardano-sl wallet
# into the Docker daemon (must be running), and then push to the
# Docker Hub. Credentials for the hub must already be installed with
# "docker login".

let
  localLib = import ../../../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, iohkPkgs ? import ../../.. { inherit config system; }
, pkgs ? iohkPkgs.pkgs
, hostPkgs ? import <nixpkgs> { inherit config system; }
, dockerHubRepoName ? null
}:

with hostPkgs;
with hostPkgs.lib;

let
  imageTypes = ["wallet" "explorer" "node"];
  images = concatLists (attrValues (localLib.forEnvironments ({ environment, ...}:
    let genericImage = iohkPkgs.dockerImages.${environment};
    in [ genericImage ] ++ map (type: genericImage.${type}) imageTypes)));

in
  writeScript "docker-build-push" (''
    #!${stdenv.shell}

    set -euo pipefail

    export PATH=${lib.makeBinPath [ docker gnused ]}

    ${if dockerHubRepoName == null then ''
    reponame=cardano-sl
    username="$(docker info | sed '/Username:/!d;s/.* //')"
    fullrepo="$username/$reponame"
    '' else ''
    fullrepo="${dockerHubRepoName}"
    ''}

  '' + concatMapStringsSep "\n" (image: ''
    echo "Loading ${image}"
    branch="''${BUILDKITE_BRANCH:-}"
    if [[ "$branch" = master ]] || [[ "$branch" = release/* ]]; then
      tag="${image.imageTag}"
    else
      tag="$(echo ${image.imageTag} | sed -e s/${image.version}/develop/)"
    fi
    tagged="$fullrepo:$tag"
    docker load -i "${image}"
    if [ "$tagged" != "${image.imageName}:${image.imageTag}" ]; then
      docker tag "${image.imageName}:${image.imageTag}" "$tagged"
    fi
    docker push "$tagged"
  '') images)
