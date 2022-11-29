const {BigNumber} = require("bignumber.js")

const JSONbig = require("@cardanosolutions/json-bigint")({
    alwaysParseAsBig: true
})

const identity = x => x
exports.fromBoolean = identity
exports.fromString = identity
exports.fromBigNumber = identity
exports.fromArray = identity
exports.fromObject = identity
exports.aesonNull = null

const _caseAeson =
    caseNull =>
    caseBoolean =>
    caseBigNumber =>
    caseString =>
    caseArray =>
    caseObject =>
    json => {
        if (json === null)
            return caseNull(json)

        if (typeof json === "boolean")
            return caseBoolean(json)

        if (typeof json === "string")
            return caseString(json)

        if (BigNumber.isBigNumber(json))
            return caseBigNumber(json);

        if (Array.isArray(json))
            return caseArray(json)

        if (typeof json === "object")
            return caseObject(json)

        throw "Imposible happened: JSON object is incorrect: "
            + json.toString() + " " + typeof json;
    }
exports._caseAeson = _caseAeson

// Hack zone.
// BigNumberFixed is instanceof (same as) BigNumber but
// redefines toJSON method to ensure no exponential notation
// in toJSON result, which JSONbig.stringify use internally

class BigNumberFixed extends BigNumber {
    constructor(bignum) {
        super(bignum)
    }
    toJSON() {
        return this.toFixed()
    }
}

//---

const traverseFormattingBigInt = _caseAeson
    // caseNull
    (identity)
    // caseBoolean
    (identity)
    // caseBigNumber
    (bn => new BigNumberFixed(bn))
    // caseString
    (identity)
    // caseArray
    (arr => arr.map(traverseFormattingBigInt))
    // caseObject
    (object => {
        const tmp = {}
        Object.keys(object).forEach(key =>
            tmp[key] = traverseFormattingBigInt(object[key]))
        return tmp
    })

exports.stringifyAeson = json => JSONbig.stringify(traverseFormattingBigInt(json))

exports.parseAeson = Nothing => Just => jsonStr => {
    try {
        return Just(JSONbig.parse(jsonStr))
    } catch (_) {
        return Nothing
    }
}


