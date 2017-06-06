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

exports.format = format;

exports.toFormatImpl = function (bigNumber, decimalPlaces) {
    return bigNumber.toFormat(decimalPlaces);
}

exports.toFormatImpl_ = function (bigNumber, formatObj, decimalPlaces) {
    format(formatObj);
    return bigNumber.toFormat(decimalPlaces);
}
