"use strict";

// This functions mirrors purescript variants from daedalus/src/Daedalus/ClientApi.purs
// This is a part of rewrite from purescript into js and depricating purescript bits

const bs58 = require("bs58");

// Decode a Base64-encoded string using Node's `Buffer` API. Its trying to mimic atob functionality available in browsers.
// This runs default node facility to decode base64 but additionally runs base64 validation code.
// Note that atob under the hood runs validation. See example:
//  > atob("====")
//  InvalidCharacterError: String contains an invalid character
//  > atob("aa==")
//  "i"
//  > atob("aa=")
//  InvalidCharacterError: String contains an invalid character
//
//  Where node's implementation doesn't:
//  > Buffer.from("====", "base64").toString("utf-8")
//  ''
//  > Buffer.from("aa==", "base64").toString("utf-8")
//  'i'
//  > Buffer.from("aa=", "base64").toString("utf-8")
//  'i'
//
// And that's the reason this funciton exists: to extend Node's base64 decode with validity check.
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

// Convert base64url into base64
function toRfc4648(str) {
    return str.replace('_', '/').replace('-', '+');
}

// Convert String into Uint8Array buffer
function unsafeStringToUint8ArrayOfCharCodes(str) {
    var buf = new ArrayBuffer(str.length);
    var bufView = new Uint8Array(buf);
    for (var i=0, strLen=str.length; i<strLen; i++) {
        bufView[i] = str.codePointAt(i);
    }
    return bufView;
}

// The atob() method decodes a base-64 encoded string.
// It lives within browser implementation (non existent in nodejs)
// I believe it exists in Electron as its chrome box.
var atobIsDefined = typeof atob === "function";

// Decode buffer into utf8 string
function decodeUtf8(buf) {
    var decoder = new TextDecoder('utf8');
    decoder.decode(buf);
}

module.exports = {
    // Implements: "Valid redemption key should be base64 and base64url decodable, it should end with '=' and it should be 44 chars long."
    isValidRedemptionKey: (code) => {
        var res = true;
        try {
            // Check if atob is defined (if we are in browser - it should be as we are in Electron)
            // Note that with Electron we will probably run the first branch (so, atob is defined) but just in case we add fallback - as there are some mentiones that it was not working sometimes https://github.com/electron/electron/issues/8529
            if(atobIsDefined) {
                // If we are in browser then:
                //  * convert input data from base64url into base64 - as vending was done in a messy way with both base64 and base64 url
                //  * decode base64 with atob - note that atob usually does base64 validity check under the hood
                //  * convert string into Uint8Array with unsafeStringToUint8ArrayOrCharCodes
                //  * decode Uint8Array into a utf-8 string
                decodeUtf8(unsafeStringToUint8ArrayOrCharCodes(atob(toRfc4648(code))));
            } else {
                // If we are not in browser then:
                //  * convert input data from base64url into base64 - as vending was done in a messy way with both base64 and base64 url
                //  * decode base64 using node facilities but additionally run manual base64 validity check (which Node doesn't implement)
                //
                //  This branch probably won't run at all in Electron.
                decodeNode(toRfc4648(code));
            }
        } catch (err) {
            res = false;
        }
        // Return true if:
        //  * its base64 or base64url decodable - ada vending state mess
        //  * ends with '=' - base64 padds string with optional '=' or two '=' signs. In ada vending phase we were using base64 configured where '=' was always added at the end.
        //  * length of base64 string should be 44 chars long. Its 32 byte decoded data - but this check is more exact becase of optional '=' in base64. See example:
        //      > base64url.toBuffer("qL8R4QIcQ_ZsRqOAbeRfcZhilN_MksRtDaEAAAArM-A=").length
        //          32
        //      > base64url.toBuffer("qL8R4QIcQ_ZsRqOAbeRfcZhilN_MksRtDaEAAAArM-A").length
        //          32
        //  Later example above is 32 byte long but its not valid ada redemption key because ada redemption key ends with '=' (lib used in above example is https://www.npmjs.com/package/base64url)
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
