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
  , "bigints"
  , "bignumber"
  , "const"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "lists"
  , "maybe"
  , "mote"
  , "numbers"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "record"
  , "sequences"
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
