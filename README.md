# purescript-aeson

Json (de/en)coders library that works with arbitrary-length numeric types (compatible with Aeson).

The motivation for this library is the fact that `purescript-argonaut` does not support long arithmetic
(JavaScript numbers silently overflow during `JSON.parse` call). We need long arithmetics to work with
software written in Haskell, because Haskell's Aeson serializes long integers as long numeric literals 
(without adding quotes).
