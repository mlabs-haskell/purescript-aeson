const {BigNumber} = require("bignumber.js")

const JSONbig = require("@khavryliuk/json-bigint")({})

//---

const identity = x => x
exports.fromBoolean = identity
exports.fromString = identity
exports.fromFiniteBigNumber = identity
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
// BigNumberFixed is instanceof BigNumber but
// redefines toJSON method to ensure no exponential notation
// in toJSON result, for integer number x, where |x| <= 2^512

// const twoIn512 = BigNumber(2).pow(512)

// class BigNumberFixed extends BigNumber {
//     constructor(bignum) {
//         super(bignum)
//     }
//     toJSON() {
//         if (this.isInteger() && this.abs().lte(twoIn512)) {
//             console.log(this.toString())
//             return this.toFixed()
//         }

//         return this.toJSON()
//     }
// }

//---

// This one is not stack-safe and can
// throw "Maximum call stack size exceeded" for big json's in practice
// thus, commented until we found better way to perform this.

const traverseFormattingBigNumber = identity
// _caseAeson
//     (identity)                                    // caseNull
//     (identity)                                    // caseBoolean
//     (bn => new BigNumberFixed(bn))                // caseBigNumber
//     (identity)                                    // caseString
//     (arr => arr.map(traverseFormattingBigNumber)) // caseArray
//     (object => {                                  // caseObject
//         const tmp = {}
//         Object.keys(object).forEach(key =>
//             tmp[key] = traverseFormattingBigNumber(object[key]))
//         return tmp
//     })

exports.stringifyAeson = json => JSONbig.stringify(traverseFormattingBigNumber(json))

exports.parseAeson = Nothing => Just => jsonStr => {
    try {
        return Just(JSONbig.parse(jsonStr))
    } catch (err) {
        return Nothing
    }
}

// ---

const constant = x => _ => x

// Compare two arrays
const arrEq = (a, b) =>{
    // a is referentially equal to b
    // i.e they are the same array and
    // thus are equal
    if (a === b)
      return true

    // if arrays have different length
    // we don't want to compare them
    // they are not equal
    if (a.length !== b.length)
      return false

    // Loop here is better than something like:
    //
    // return Array.from(a)
    //   .reduce((acc, ai, i) => acc && aesonEq(ai, b[i]), true)
    //
    // ... because it allows us to fail fast
    // arrays are not equal
    // as soon as we encounter first inequality
    for (let i = 0; i < a.length; i++)
        if (!aesonEq(a[i], b[i]))
            return false

    // Are you still here?
    return true
}

const objectEq = (a, b) => {
    // referentially equal
    if (a === b)
      return true

    const aKeys = Object.keys(a)
    const bKeys = Object.keys(b)

    // if objects have different sets of keys
    // they are not equal, but to compare lists of keys,
    // we have to sort them first. This is pretty expensive,
    // If these lists have the same length, and
    // for every `key` from `a`, a[key] equals b[key],
    // then objects are equal
    if (aKeys.length !== bKeys.length)
        return false

    for (let i = 0; i < aKeys.length; i++) {
        let key = aKeys[i]
        if (!aesonEq(a[key], b[key]))
            return false
    }

    return true
}

// Comparable tags for each possible aeson "constructor"
const tNull = "null"
const tBool = "bool"
const tBNum = "bnum"
const tStr  = "str"
const tArr  = "arr"
const tObj  = "obj"

const typeOf = _caseAeson
    (constant(tNull))
    (constant(tBool))
    (constant(tBNum))
    (constant(tStr))
    (constant(tArr))
    (constant(tObj))

const aesonEq = (a, b) => {
    // If "constructors" are different
    // aesons are not equal
    const tOfA = typeOf(a)
    if (tOfA !== typeOf(b))
        return false

    switch (tOfA) {
        case tNull: return true
        case tBool: return a === b
        case tBNum: return a.eq(b)
        case tStr : return a === b
        case tArr : return arrEq(a, b)
        case tObj : return objectEq(a, b)
    }

    throw "Imposible happened: Unexpected type of JSON: " + a.toString
}

exports.aesonEq = a => b => aesonEq(a, b)
