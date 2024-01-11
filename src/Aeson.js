import JSONbig from "@mlabs-haskell/json-bigint";

const identity = x => x;
const constant = x => _ => x;

export const fromBoolean = identity;
export const fromString = identity;
export const fromBigInt = identity;
export const fromFiniteNumber = identity;
export const fromArray = identity;
export const fromObject = identity;
export const aesonNull = null;

export const caseAeson =
  ({
    caseNull,
    caseBoolean,
    caseFiniteNumber,
    caseBigInt,
    caseString,
    caseArray,
    caseObject
  }) =>
  json => {
    if (typeof json === "string") return caseString(json);

    if (Array.isArray(json)) return caseArray(json);

    if (typeof json === "object" && json !== null) return caseObject(json);

    if (typeof json === "bigint") return caseBigInt(json);

    if (typeof json === "number" && !isNaN(json) && isFinite(json))
      return caseFiniteNumber(json);

    if (typeof json === "boolean") return caseBoolean(json);

    if (json === null) return caseNull(json);

    throw "Imposible happened: JSON object is incorrect: " + " " + typeof json;
  };

export function stringifyAeson(json) {
  return JSONbig.stringify(json);
}

export const parseAeson = Nothing => Just => jsonStr => {
  try {
    return Just(JSONbig.parse(jsonStr));
  } catch (err) {
    return Nothing;
  }
};

// Compare two arrays
const arrEq = (a, b) => {
  // a is referentially equal to b
  // i.e they are the same array and
  // thus are equal
  if (a === b) return true;

  // if arrays have different length
  // we don't want to compare them
  // they are not equal
  if (a.length !== b.length) return false;

  // Loop here is better than something like:
  //
  // return Array.from(a)
  //   .reduce((acc, ai, i) => acc && aesonEqUncurried(ai, b[i]), true)
  //
  // ... because it allows us to fail fast
  // arrays are not equal
  // as soon as we encounter first inequality
  for (let i = 0; i < a.length; i++)
    if (!aesonEqUncurried(a[i], b[i])) return false;

  // Are you still here?
  return true;
};

const objectEq = (a, b) => {
  // referentially equal
  if (a === b) return true;

  const aKeys = Object.keys(a);
  const bKeys = Object.keys(b);

  // if objects have different sets of keys
  // they are not equal, but to compare lists of keys,
  // we have to sort them first. This is pretty expensive,
  // If these lists have the same length, and
  // for every `key` from `a`, a[key] equals b[key],
  // then objects are equal
  if (aKeys.length !== bKeys.length) return false;

  for (let i = 0; i < aKeys.length; i++) {
    let key = aKeys[i];
    if (!aesonEqUncurried(a[key], b[key])) return false;
  }

  return true;
};

// Comparable tags for each possible aeson "constructor"
const tNull = "null";
const tBool = "bool";
const tNum = "num";
const tStr = "str";
const tArr = "arr";
const tObj = "obj";
const tBigInt = "bigint";

const typeOf = caseAeson({
  caseNull: constant(tNull),
  caseBoolean: constant(tBool),
  caseFiniteNumber: constant(tNum),
  caseBigInt: constant(tBigInt),
  caseString: constant(tStr),
  caseArray: constant(tArr),
  caseObject: constant(tObj)
});

const aesonEqUncurried = (a, b) => {
  // If "constructors" are different
  // aesons are not equal
  const tOfA = typeOf(a);

  if (tOfA !== typeOf(b)) return false;

  switch (tOfA) {
    case tNull:
      return true;
    case tBool:
      return a === b;
    case tNum:
      return a === b;
    case tBigInt:
      return a === b;
    case tStr:
      return a === b;
    case tArr:
      return arrEq(a, b);
    case tObj:
      return objectEq(a, b);
  }

  throw (
    "purescript-aeson: Imposible happened: Unexpected type of JSON: " +
    a.toString()
  );
};

export function aesonEq(a) {
  return b => aesonEqUncurried(a, b);
}
