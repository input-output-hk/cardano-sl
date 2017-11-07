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
    var decoder = new TextDecoder('utf-8');
    decoder.decode(buf);
}

// Implements: "Valid redemption key should be base64 and base64url decodable, it should end with '=' and it should be 44 chars long."
function isValidRedemptionKey(code) {
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
}

// Implements: "Valid paper vend redemption key should be base58 decodable 32 byte stream."
function isValidPaperVendRedemptionKey(code) {
    try {
        return bs58.decode(code).length == 32;
    } catch (err) {
        return false;
    }
}

function tests() {
    // Before running tests be sure to build client api. For more information see cardano docs
    const api = require('../output/Daedalus.ClientApi')

    // taken from https://stackoverflow.com/a/1349462/1924817
    var rardomString = function(len, charSet) {
        charSet = charSet || 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
        var randomString = '';
        for (var i = 0; i < len; i++) {
            var randomPoz = Math.floor(Math.random() * charSet.length);
            randomString += charSet.substring(randomPoz,randomPoz+1);
        }
        return randomString;
    }

    // generate base64 samples
    // Some will be valid, some won't
    var b64Length = function(l, charSet) {
        var b64 = rardomString(l, charSet) // valid base64 but might be invalid redemption key (depending on the size)
        var b64Pad1 = b64 + "="   // should be valid depending on the size
        var b64Pad2 = b64 + "=="  // should be valid depending on the size
        var b64Pad3 = b64 + "===" // invalid
        return [b64, b64Pad1, b64Pad2, b64Pad3]
    }

    var testList = []
    var numberOfGroupTests = 100;
    var b64Charset = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
    var b64urlCharset = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
    var b58Charset = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
    for (var i = 0; i < numberOfGroupTests; i++) {
        // generate base64 test strings
        Array.prototype.push.apply(testList, b64Length(40, b64Charset))
        Array.prototype.push.apply(testList, b64Length(41, b64Charset))
        Array.prototype.push.apply(testList, b64Length(42, b64Charset))
        Array.prototype.push.apply(testList, b64Length(43, b64Charset))
        Array.prototype.push.apply(testList, b64Length(44, b64Charset))
        Array.prototype.push.apply(testList, b64Length(45, b64Charset))

        // generate base64url test strings
        Array.prototype.push.apply(testList, b64Length(40, b64urlCharset))
        Array.prototype.push.apply(testList, b64Length(41, b64urlCharset))
        Array.prototype.push.apply(testList, b64Length(42, b64urlCharset))
        Array.prototype.push.apply(testList, b64Length(43, b64urlCharset))
        Array.prototype.push.apply(testList, b64Length(44, b64urlCharset))
        Array.prototype.push.apply(testList, b64Length(45, b64urlCharset))

        // generate base58 test strings
        Array.prototype.push.apply(testList, [rardomString(40, b58Charset)])
        Array.prototype.push.apply(testList, [rardomString(41, b58Charset)])
        Array.prototype.push.apply(testList, [rardomString(42, b58Charset)])
        Array.prototype.push.apply(testList, [rardomString(43, b58Charset)])
        Array.prototype.push.apply(testList, [rardomString(44, b58Charset)]) // most of the time this will be correct base58
        Array.prototype.push.apply(testList, [rardomString(45, b58Charset)])
        Array.prototype.push.apply(testList, [rardomString(46, b58Charset)])
    }

    var redemptionKeyValid = 0
    var redemptionKeyInvalid = 0
    var paperVendRedemptionKeyValid = 0
    var paperVendRedemptionKeyInvalid = 0
    for (var i = 0, len = testList.length; i < len ; i++) {
        if(isValidRedemptionKey(testList[i]) == api.isValidRedemptionKey(testList[i])) {
            isValidRedemptionKey(testList[i])? redemptionKeyValid++: redemptionKeyInvalid++;
        } else {
            console.log("Test failed for redemption key: " + testList[i])
        }
        if(isValidPaperVendRedemptionKey(testList[i]) == api.isValidPaperVendRedemptionKey(testList[i])) {
            isValidPaperVendRedemptionKey(testList[i])? paperVendRedemptionKeyValid++: paperVendRedemptionKeyInvalid++;
        } else {
            console.log("Test failed for paper vend redemption key: " + testList[i])
        }
    }

    console.log("Successfull tests for redemption key: " + (redemptionKeyValid + redemptionKeyInvalid))
    console.log("Failed tests for redemption key: " + (testList.length - redemptionKeyValid - redemptionKeyInvalid))
    console.log("redemptionKeyValid: " + redemptionKeyValid)
    console.log("redemptionKeyInvalid: " + redemptionKeyInvalid)

    console.log("Successfull tests for paper vend redemption key: " + (paperVendRedemptionKeyValid + paperVendRedemptionKeyInvalid))
    console.log("Failed tests for paper vend redemption key: " + (testList.length - paperVendRedemptionKeyValid - paperVendRedemptionKeyInvalid))
    console.log("paperVendRedemptionKeyValid: " + paperVendRedemptionKeyValid)
    console.log("paperVendRedemptionKeyInvalid: " + paperVendRedemptionKeyInvalid)

}

module.exports = {
    isValidRedemptionKey: isValidRedemptionKey,
    isValidPaperVendRedemptionKey: isValidPaperVendRedemptionKey,
    tests: tests,
}
