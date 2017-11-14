"use strict";

// This functions mirrors purescript variants from daedalus/src/Daedalus/ClientApi.purs
// This is a part of rewrite from purescript into js and depricating purescript bits

const bs58 = require("bs58");

// Convert base64url into base64
function toRfc4648(str) {
    return str.replace(new RegExp('_', 'g'), '/').replace(new RegExp('-', 'g'), '+');
}

// Checks is input string valid base64 or base64url
function isValidBase64Url(code) {
    try {
        // Note that atob is defined in Electron render process where its being used. This won't work in Node
        //  * convert input data from base64url into base64 - as vending was done in a messy way with both base64 and base64 url
        //  * decode base64 with atob - note that atob usually does base64 validity check under the hood
        atob(toRfc4648(code));
        return true;
    } catch (err) {
        return false;
    }
}

// Implements: "Valid redemption key should be base64 and base64url decodable, it should end with '=' and it should be 44 chars long."
function isValidRedemptionKey(code) {
    // Return true if:
    //  * its base64 or base64url decodable - ada vending state mess
    //  * ends with '=' - base64 padds string with optional '=' or two '=' signs. In ada vending phase we were using base64 configured where '=' was always added at the end.
    //  * length of base64 string should be 44 chars long. Its 32 byte decoded data - but this check is more exact becase of optional '=' in base64. See example:
    //      > base64url.toBuffer("qL8R4QIcQ_ZsRqOAbeRfcZhilN_MksRtDaEAAAArM-A=").length
    //          32
    //      > base64url.toBuffer("qL8R4QIcQ_ZsRqOAbeRfcZhilN_MksRtDaEAAAArM-A").length
    //          32
    //  Later example above is 32 byte long but its not valid ada redemption key because ada redemption key ends with '=' (lib used in above example is https://www.npmjs.com/package/base64url)
    return isValidBase64Url(code) && code.endsWith('=') && code.length == 44;
}

// Implements: "Valid paper vend redemption key should be base58 decodable 32 byte stream."
function isValidPaperVendRedemptionKey(code) {
    try {
        return bs58.decode(code).length == 32;
    } catch (err) {
        return false;
    }
}

module.exports = {
    isValidRedemptionKey: isValidRedemptionKey,
    isValidPaperVendRedemptionKey: isValidPaperVendRedemptionKey,
}
