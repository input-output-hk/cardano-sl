const BigNumber = require('bignumber.js');

exports.bigNumberImpl = function (just, nothing, value) {
    try {
        const bigNumber = new BigNumber(value);
        return just(bigNumber);
    } catch (e) {
        return nothing;
    }
}

exports.dividedByImpl = function (bigNumber, by) {
    return bigNumber.dividedBy(by);
}

exports.toStringImpl = function (bigNumber, base) {
    return bigNumber.toString(base);
}

const format = function (format) {
    BigNumber.config({
        FORMAT: format
    });
}

exports.formatImpl = format;

exports.toFormatImpl = function (bigNumber, formatObj, decimalPlaces) {
    // Note: Set format before adding decimal places.
    // This is needed to change formats while switching locales
    format(formatObj);
    return bigNumber.toFormat(decimalPlaces);
}
