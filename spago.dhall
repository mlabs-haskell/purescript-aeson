{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "aeson"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "bignumber"
  , "const"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "js-bigints"
  , "lists"
  , "maybe"
  , "mote"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "record"
  , "spec"
  , "strings"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "uint"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
