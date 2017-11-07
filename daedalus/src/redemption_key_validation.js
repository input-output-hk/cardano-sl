"use strict";

// This functions mirrors purescript variants from daedalus/src/Daedalus/ClientApi.purs
/* This is a part of rewrite from purescript into js and depricating purescript bits
*/
//

const bs58 = require("bs58");

// Decode a Base64-encoded string using Node's `Buffer` API
function decodeNode (str) {
    var result;

    // Check that the input string is a valid Base64-encoded string as Node.js
    // decided that it would be a good idea to NOT throw on invalid input strings
    // but return an empty buffer instead which cannot be distinguished from the
    // empty string case.
    var reEmptyString = "^$";
    var leadingQuanta = "^([A-Za-z0-9+/]{4})*";
    var finalQuantum =
        "([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}(?:=)?|[A-Za-z0-9+/]{2}(?:=){0,2})$";
    var reValidBase64 =
        new RegExp([reEmptyString, "|", leadingQuanta, finalQuantum].join(""));

    if (!reValidBase64.test(str)) { throw new Error("Invalid input string");}
    result = Buffer.from(str, "base64").toString("utf-8");

    return result;
};

function toRfc4648(str) {
    return str.replace('_', '/').replace('-', '+');
}

function unsafeStringToUint8ArrayOfCharCodes(str) {
    var buf = new ArrayBuffer(str.length);
    var bufView = new Uint8Array(buf);
    for (var i=0, strLen=str.length; i<strLen; i++) {
        bufView[i] = str.codePointAt(i);
    }
    return bufView;
}

var atobIsDefined = typeof atob === "function";

function decodeUtf8(str) {
    var decoder = new TextDecoder('utf8');
    decoder.decode(str);
}

module.exports = {
    // Implements: "Valid redemption key should be base64 and base64url decodable, it should end with '=' and it should be 44 chars long."
    isValidRedemptionKey: (code) => {
        var res = false;
        try {
            if(atobIsDefined) {
                decodeUtf8(unsafeStringToUint8ArrayOrCharCodes(atob(toRfc4648(code))));
                res = true;
            } else {
                decodeNode(toRfc4648(code));
                res = true;
            }
        } catch (err) {
            res = false;
        }
        return res && code.endsWith('=') && code.length == 44;
    },
    // Implements: "Valid paper vend redemption key should be base58 decodable 32 byte stream."
    isValidPaperVendRedemptionKey: (code) => {
        try {
            return bs58.decode(code).length == 32;
        } catch (err) {
            return false;
        }
    },
}
