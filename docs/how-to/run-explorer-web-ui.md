# How to run Explorer Web UI

## Build frontend

You should build frontend part of Explorer to work with web UI via browser. Assumed that you already cloned [`cardano-sl`](https://github.com/input-output-hk/cardano-sl) repository.

Please make sure you have installed [`yarn`](https://yarnpkg.com/lang/en/docs/install/) program (at least `0.27.5` version).

Then do:

```
$ cd cardano-sl/explorer/frontend
$ ./scripts/build-explorer-frontend.sh
```

## Run frontend

Now run frontend (from within `explorer/frontend` subdirectory):

```
$ yarn start
```

It will take some time.

After that go to [localhost:3100](http://localhost:3100/).
