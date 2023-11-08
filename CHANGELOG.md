# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v2.0.0]

### Added

### Removed

- Runtime NPM dependency on [`big-integer`](https://www.npmjs.com/package/big-integer) ([#20](https://github.com/mlabs-haskell/purescript-aeson/pull/20))
- Some functions related to `big-integer` were removed ([#20](https://github.com/mlabs-haskell/purescript-aeson/pull/20)):
  - `caseAesonBigNumber`
  - `caseAesonFiniteBigNumber`
  - `isBigNumber`
  - `partialFiniteNumber`
  - `partialFiniteBigNumber`
  - `toBigNumber`
  - `finiteBigNumber`
    - `fromFiniteBigNumber`
- Support for numeric literals with long decimal part ([#20](https://github.com/mlabs-haskell/purescript-aeson/pull/20))

### Changed

- Updated `@mlabs-haskell/json-bigint` runtime dependency version to [`2.0.0`](https://github.com/mlabs-haskell/json-bigint/tree/v2.0.0) ([#20](https://github.com/mlabs-haskell/purescript-aeson/pull/20))
- `unpackFinite` -> `unFinite` ([#20](https://github.com/mlabs-haskell/purescript-aeson/pull/20))
- `Number` and `BigInt` are now separate entities at runtime. Due to that `caseAeson` now accepts more cases. Note that `Number` is decode-able from a number without a decimal part when using `caseAesonNumber`, but `caseAeson` is not consistent with this behavior: numeric literals without a decimal part will always be interpreted as `BigInt`. ([#20](https://github.com/mlabs-haskell/purescript-aeson/pull/20))


## [v1.0.0]

Initial version.

Supports long numeric literals (including long decimal parts). Depends on [`big-integer`](https://www.npmjs.com/package/big-integer)
