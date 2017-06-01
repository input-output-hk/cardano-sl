const BigNumber = require('bignumber.js');

exports.bigNumberImpl = function (just, nothing, value) {
    try {
        const bigNumber = new BigNumber(value);
        return just(bigNumber);
    } catch (e) {
        return nothing;
    }
}

exports.dividedBy = function (bigNumber, by) {
    return bigNumber.dividedBy(by);
}

exports.toString = function (bigNumber) {
    return bigNumber.toString();
}
