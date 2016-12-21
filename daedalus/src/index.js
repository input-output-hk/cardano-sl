// entry point for calling Cardano's api
export const ClientApi = require('./Daedalus/ClientApi.purs');

// all the following entry points are just for debugging
// TODO jk: remove it later
// entry point of Main -
export const Main = require('./Main.purs');
// entry point of Debug
export const Debug = require('./Daedalus/Debug.purs');
// entry point of Api
export const BackendApi = require('./Daedalus/BackendApi.purs');
