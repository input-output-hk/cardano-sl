create a volume for the wallet&logs

`docker volume create cardano-state-1`

load the docker image downloaded from hydra

`docker load < image.tar.gz`

run cardano with the volume and bring port 8090 out to the host

`docker run --rm -it -p 127.0.0.1:8090:8090 -v cardano-state-1:/wallet cardano-container-mainnet-1.0:latest`

(`-it` is optional)

`docker container ls` and `docker cp jolly_gates:/wallet/tls/server.cert ./server.cert` to extract the auto-generated cert from the container

`curl --cacert server.cert https://localhost:8090/api/settings/version` to confirm the api is working
