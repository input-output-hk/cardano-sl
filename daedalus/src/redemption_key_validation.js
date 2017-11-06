"use strict";

const bs58 = require("bs58");

module.exports = {
    // This function mirrors purescript variant from daedalus/src/Daedalus/ClientApi.purs
    /* This is a part of rewrite from purescript into js and depricating purescript bits
    */
    //
    isValidPaperVendRedemptionKey: (code) => {
        try {
            return bs58.decode(code).length == 32;
        } catch (err) {
            return false;
        }
    },
}
