{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "aeson"
, dependencies =
  [ "aff"
  , "argonaut"
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
  , "gen"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "medea"
  , "mote"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "nonempty"
  , "numbers"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "record"
  , "sequences"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "uint"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
